# Combine child, mother, HH data
# Join HH to mother with v001, v002
# Join mother to child with v001, v002, v003

# ##############################################################################
# Setup of libraries and directories
# ##############################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lubridate)
library(foreign)
library(designmatch)

datadir <- "/Users/susanna/projects/sanitation/data/nfhs/"
figdir <- "/Users/susanna/projects/sanitation/output/figures/"

# ##############################################################################
# Join HH, mother data to child data
# ##############################################################################
child.data <- readRDS(paste0(datadir, "/child_data.rds"))
mom.data <- readRDS(paste0(datadir, "/mother_data.rds"))
hh.data <- readRDS(paste0(datadir, "/hh_data.rds"))

# Remove variables we don't need in hh and mom data
mom.data <- dplyr::select(mom.data, -c(rural.urban, mom.sample.wt, state))
hh.data <- hh.data %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(rural.urban, state))

# Here we need to include state because psu.id and hh.id are NOT unique across
# states in NFHS2 HH data
mom.data <- inner_join(mom.data, hh.data,
                       by = c("round", "state.name", "psu.id", "hh.id"))

child.data <- inner_join(child.data, mom.data,
                         by = c("round", "state.name", "psu.id", "hh.id", "mom.id"))

# ##############################################################################
# Filter
# ##############################################################################

# Keep only kids with non-flagged height-for-age
# z-scores, kids age 3 and under, kids born to women ever-married women 15-49
# (NFHS1 included women 13-49, NFHS2 and 3 women 15-49)
child.data <- dplyr::filter(child.data,
                              !is.na(hh.od) & flag.zlen == 0 &
                              child.age.mos < 36 & mom.evermarried == 1 &
                              mom.age.mos.interview >= (15*12))

# SAVE
saveRDS(child.data, paste0(datadir, "/compiled_child_data.rds"))

# ##############################################################################
# Compile the PSU-level data
# ##############################################################################
psu_data_hh <- readRDS(paste0(datadir, "/hh_psu_data.rds"))
#psu_data_mom <- readRDS(paste0(datadir, "/mother_psu_data.rds"))
#psu_data_kid <- readRDS(paste0(datadir, "/child_psu_data.rds"))

# Drop where the sample size is < 15
psu_data_hh$min_ss <- pmin(psu_data_hh$ss_hh_assets, psu_data_hh$ss_hh_hindu,
                           psu_data_hh$ss_hh_muslim, psu_data_hh$ss_hh_od,
                           psu_data_hh$ss_hh_water)
psu_data_hh <- dplyr::filter(psu_data_hh, min_ss >= 15)
#psu_data_hh <- dplyr::filter(psu_data_hh, hh_sample_size >= 15)
#psu_data_mom <- dplyr::filter(psu_data_mom, mom_sample_size >= 15)
#psu_data_kid <- dplyr::filter(psu_data_kid, child_sample_size >= 15)

# Merge to make PSU-level data
#psu_data <- inner_join(psu_data_hh, psu_data_mom,
#                       by = c("round", "state.name", "psu.id"))
#psu_data <- inner_join(psu_data, psu_data_kid,
#                       by = c("round", "state.name", "psu.id"))

# ##############################################################################
# Generalized propensity score model
# ##############################################################################
# To model the GPS, we fit a model to the treatment: T ~ f(X) + epsilon
psu_data_hh$is.NFHS1 <- as.numeric(psu_data_hh$round == "NFHS1")
psu_data_hh$is.NFHS2 <- as.numeric(psu_data_hh$round == "NFHS2")
psu_data_hh$is.NFHS3 <- as.numeric(psu_data_hh$round == "NFHS3")

gps_mod <- lmer(prop_hh_od ~ 0 + is.NFHS1 + is.NFHS2 + is.NFHS3 +
                  prop_hh_assets + prop_hh_water +
                  prop_hh_muslim + prop_hh_hindu +  (1|state.name),
                data = psu_data_hh)
gps_resids <- resid(gps_mod)
gps_preds <- predict(gps_mod)

# Use Imai & van Dyk method to check balance:
# 1. regress each predictor above on prop_hh_od
# 2. regress each predictor above on prop_hh_od + gps_preds
# 3. compare distributions of t-stats for coeff on prop_hh_od
cov.list <- c("is.NFHS1", "is.NFHS2", "is.NFHS3", "prop_hh_assets",
               "prop_hh_water", "prop_hh_muslim", "prop_hh_hindu")
t.list1 <- rep(NA, length(cov.list))
t.list2 <- rep(NA, length(cov.list))
for (j in 1:length(cov.list)) {
  curr.cov <- cov.list[j]
  if (j <= 3) {
    m1 <- glm(get(curr.cov) ~ prop_hh_od, data = psu_data_hh,
              family = binomial(link = "logit"))
    m2 <- glm(psu_data_hh[[curr.cov]] ~ psu_data_hh$prop_hh_od + gps_preds,
              family = binomial(link = "logit"))
  } else {
    m1 <- glm(get(curr.cov) ~ prop_hh_od, data = psu_data_hh,
              family = gaussian(link = "identity"))
    m2 <- glm(psu_data_hh[[curr.cov]] ~ psu_data_hh$prop_hh_od + gps_preds,
              family = gaussian(link = "identity"))
  }
  m1.mat <- summary(m1)$coefficients
  t.list1[j] <- m1.mat[2,3]
  m2.mat <- summary(m2)$coefficients
  t.list2[j] <- m2.mat[2,3]
}
df1 <- nrow(psu_data_hh) - 2
df2 <- nrow(psu_data_hh) - 3
par(mfrow = c(1,2))
qqplot(qt(ppoints(100), df = df1), t.list1, ylim = c(-100, 40),
       xlab = "t-dist quantiles", ylab = "t-stat quantiles")
qqline(t.list1, distribution = function(p) qt(p, df = df1))
qqplot(qt(ppoints(100), df = df2), t.list2, ylim = c(-100, 40),
       xlab = "t-dist quantiles", ylab = "t-stat quantiles")
qqline(t.list2, distribution = function(p) qt(p, df = df2))

par(mfrow = c(1,1))
qqplot(qt(ppoints(100), df = df2), t.list2,
       xlab = "t-dist quantiles", ylab = "t-stat quantiles")
qqline(t.list2, distribution = function(p) qt(p, df = df2))

qqplot(qnorm(ppoints(100)), t.list2,
       xlab = "std normal quantiles", ylab = "t-stat quantiles")
qqline(t.list2)


plot(gps_preds, gps_resids)

plot(psu_data_hh$prop_hh_assets, gps_resids)
abline(h = 0, col = "red")
abline(lm(gps_resids ~ psu_data_hh$prop_hh_assets), col = "blue")
plot(psu_data_hh$prop_hh_water, gps_resids)
abline(h = 0, col = "red")
abline(lm(gps_resids ~ psu_data_hh$prop_hh_water), col = "blue")
plot(psu_data_hh$prop_hh_muslim, gps_resids)
abline(h = 0, col = "red")
abline(lm(gps_resids ~ psu_data_hh$prop_hh_muslim), col = "blue")
plot(psu_data_hh$prop_hh_hindu, gps_resids)
abline(h = 0, col = "red")

# # Merge PSU-level data to child data
# child.data <- left_join(child.data, psu_data_hh,
#                         by = c("round", "state.name", "psu.id"))
# child.data <- left_join(child.data, psu_data_mom,
#                         by = c("round", "state.name", "psu.id"))
# child.data <- left_join(child.data, psu_data_kid,
#                         by = c("round", "state.name", "psu.id"))

# ##############################################################################
# Compile the PSU-level data
# ##############################################################################

# Density plot of height-for-age by round
ggplot(child.data) +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_line(aes(x = zlen, colour = round), stat = "density") +
  xlab("Height-for-age z-score") +
  ylab("Density") +
  scale_colour_discrete("") +
  theme_bw() +
  theme(legend.position = "bottom")

# Test propensity score model
cat.covars <- c("round", "state.name", "urban", "mom.edu.level")
cont.covars <- c("hh.asset_prop", "hh.muslim", "hh.improved_water", "mom.literate",
                 "mom.lives.with.inlaws", "mom.age.mos.interview")

# pscore.formula <- formula(hh.od ~ cont.covars +
#                             paste0("factor(", cat.covars, ")", , collapse = " + "))
# pscore <- glm(pscore.formula, data = child.data, family = binomial())
# pscore <- glm(hh.od ~ hh.asset_prop + factor(round) + factor(state.name) +
#                 factor(urban) + factor(mom.edu.level) + hh.muslim +
#                 hh.improved_water + mom.lives.with.inlaws + mom.literate +
#                 mom.age.mos.interview,
#               data = child.data, family = binomial())

# See what the before-matching difference in the above covars is
cat.cov.diffs.before <- data.frame()
for (covar in cat.covars) {
  child.data %>%
    dplyr::group_by_(.dots = c("hh.od", covar)) %>%
    dplyr::summarise(num.obs = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hh.od = ifelse(hh.od == 1, "OD_1", "OD_0")) %>%
    tidyr::spread(key = hh.od, value = num.obs) %>%
    dplyr::mutate(diff = OD_1 - OD_0,
                  rel_diff = (OD_1 - OD_0) / OD_1,
                  covar_name = covar) -> tmp
  tmp$cat_name <- tmp[[covar]]
  tmp[[covar]] <- NULL
  tmp <- tmp[, c("covar_name", "cat_name", "OD_0", "OD_1", "diff", "rel_diff")]
  cat.cov.diffs.before <- rbind(cat.cov.diffs.before, tmp)
}
std.diffs.before <- data.frame()
for (covar in cont.covars) {
  mean.c <- mean(child.data[child.data$hh.od == 0, covar], na.rm = TRUE)
  mean.t <- mean(child.data[child.data$hh.od == 1, covar], na.rm = TRUE)
  var.c <- var(child.data[child.data$hh.od == 0, covar], na.rm = TRUE)
  var.t <- var(child.data[child.data$hh.od == 1, covar], na.rm = TRUE)
  denom <- sqrt(0.5 * (var.c + var.t))
  std.diff <- (mean.t - mean.c) / denom
  tmp <- data.frame(covar, mean.t, mean.c, denom, std.diff)
  std.diffs.before <- rbind(std.diffs.before, tmp)
}

# Pair matching
# Exact on: round, state, rural/urban, improved water, religion, mom.literate
# Mean balancing:
exact_cov_names <- c("round", "state.name", "urban", "hh.improved_water",
                     "hh.religion", "mom.literate")
child.data$urban <- ifelse(child.data$urban == 1, "urban", "rural")

# Match parameters
dist_mat <- NULL 
subset_weight <- 1
t_max <- 60*100
solver <- "cplex"
approximate <- 0
solver <- list(name = solver, t_max = t_max, approximate = approximate,
               round_cplex = 0, trace = 0)

# Collapse on exact match variables so we can match within each of those cells
child.data %>%
  dplyr::select_(.dots = c("hh.od", exact_cov_names)) %>%
  na.omit() %>%
  dplyr::group_by_(.dots = c("hh.od", exact_cov_names)) %>%
  dplyr::summarise(num.obs = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(hh.od = ifelse(hh.od == 1, "OD_1", "OD_0")) %>%
  tidyr::spread(key = hh.od, value = num.obs) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(min_obs = min(OD_1, OD_0)) %>%
  dplyr::filter(min_obs >= 2) -> match.cats

# Loop through rows of tmp to match
count <- 0
match.results <- NULL
for (j in 1:nrow(match.cats)) {
  curr.round <- match.cats$round[j]
  curr.state <- match.cats$state.name[j]
  curr.urban <- match.cats$urban[j]
  curr.hh.water <- match.cats$hh.improved_water[j]
  curr.hh.religion <- match.cats$hh.religion[j]
  curr.mom.literate <- match.cats$mom.literate[j]
  
  subset <- paste(curr.round, curr.state, curr.urban, "water =", curr.hh.water,
                  curr.hh.religion, "mom.literate =", curr.mom.literate, sep = " ")
  cat("########################################################################\n")
  cat("Currently matching:", subset, "\n")
  
  child.data %>%
    dplyr::filter(round == curr.round & state.name == curr.state &
                    urban == curr.urban & hh.improved_water == curr.hh.water &
                    hh.religion == curr.hh.religion &
                    mom.literate == curr.mom.literate) %>%
    dplyr::arrange(desc(hh.od)) -> curr.data
  
  cat("Number of treated/control units:\n")
  print(table(curr.data$hh.od))
  
  t_ind <- curr.data$hh.od
  mom_cov_names <- c("hh.asset_prop", "mom.lives.with.inlaws", "mom.age.mos.interview")
  mom_covs <- curr.data[, mom_cov_names]
  mom_tols <- round(absstddif(mom_covs, t_ind, .1), 2)
  mom <- list(covs = mom_covs, tols = mom_tols)
  out <- bmatch(t_ind = t_ind, subset_weight = subset_weight, solver = solver,
                mom = mom)
  
  # Output of matching
  t_id <- out$t_id
  c_id <- out$c_id
  matchtime <- out$time
  cat("\nNumber of matches:\n")
  cat("Treated:", length(out$t_id), "\n")
  cat("Control:", length(out$c_id), "\n")
  group_id <- out$group_id + count
  
  # If we didn't get any matches, move on to the next subset
  if (length(t_id) == 0) {
    cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    cat("Didn't get any matches for", subset, "! Returning empty match.results\n")
    next
  }
  
  # Append results
  d_aux <- cbind(curr.data[c(t_id, c_id), ], group_id)
  match.results <- rbind(match.results, d_aux)
  cat("\nAfter match, number of rows in match.results:", nrow(match.results), "\n")
  # divide by 2 b/c obs in match.results are units whereas count is for pairs
  count <- nrow(match.results)/2
  cat("\nNew count:", count, "\n")
  cat("\n")
  cat("* Number of matched pairs:", length(t_id), "\n")
  cat("* Matching time (mins):",
      round(matchtime / 60, 2), "\n")
  cat("********************************************************************\n")
}

# Recalculate std diff in means
# See what the before-matching difference in the above covars is
cat.cov.diffs.after <- data.frame()
for (covar in cat.covars) {
  match.results %>%
    dplyr::group_by_(.dots = c("hh.od", covar)) %>%
    dplyr::summarise(num.obs = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(hh.od = ifelse(hh.od == 1, "OD_1", "OD_0")) %>%
    tidyr::spread(key = hh.od, value = num.obs) %>%
    dplyr::mutate(diff = OD_1 - OD_0,
                  rel_diff = (OD_1 - OD_0) / OD_1,
                  covar_name = covar) -> tmp
  tmp$cat_name <- tmp[[covar]]
  tmp[[covar]] <- NULL
  tmp <- tmp[, c("covar_name", "cat_name", "OD_0", "OD_1", "diff", "rel_diff")]
  cat.cov.diffs.after <- rbind(cat.cov.diffs.after, tmp)
}
std.diffs.after <- data.frame()
for (covar in cont.covars) {
  mean.c <- mean(match.results[match.results$hh.od == 0, covar], na.rm = TRUE)
  mean.t <- mean(match.results[match.results$hh.od == 1, covar], na.rm = TRUE)
  denom <- std.diffs.before$denom[std.diffs.before$covar == covar]
  std.diff <- (mean.t - mean.c) / denom
  tmp <- data.frame(covar, mean.t, mean.c, std.diff)
  std.diffs.after <- rbind(std.diffs.after, tmp)
}

mr.c <- match.results[match.results$hh.od == 0, c(exact_cov_names, "group_id")]
mr.t <- match.results[match.results$hh.od == 1, c(exact_cov_names, "group_id")]
mr.w <- dplyr::inner_join(mr.t, mr.c, by = c(exact_cov_names, "group_id"))


# Collapse to PSU level
child.data %>%
  dplyr::group_by(round, psu.id) %>%
  dplyr::summarise(psu_od = mean(hh.od, na.rm = TRUE),
                   psu_zlen = mean(zlen, na.rm = TRUE),
                   muslim = mean(hh.muslim, na.rm = TRUE),
                   electricity = mean(hh.electricity, na.rm = TRUE),
                   improved_water = mean(hh.improved_water, na.rm = TRUE),
                   asset_prop = mean(hh.asset_prop, na.rm = TRUE)) -> child.psu
child.psu$psu_latrine_use <- 1 - child.psu$psu_od

child.psu %>%
  tidyr::gather(key = indicator, value = value, -c(muslim, round, psu.id)) %>%
  dplyr::filter(indicator != "od") %>%
  dplyr::mutate(indicator = as.character(indicator)) -> child.psu.long

ggplot(child.psu.long[!(child.psu.long$indicator %in% c("psu_od", "psu_zlen")) &
                        child.psu.long$muslim > 0 & child.psu$muslim < 1, ],
       aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator), span = 0.9, se = FALSE) +
  #geom_smooth(aes(colour = indicator, size = indicator), span = 0.8, se = FALSE) +
  #scale_size_manual("", values = c(1, 1, 1, 2), guide = FALSE) +
  scale_colour_discrete("") +
  #facet_wrap(~ round) +
  xlab("PSU % Muslim") +
  ylab(expression(paste("Predicts better health ", symbol('\256')))) +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(child.psu.long[!(child.psu.long$indicator %in% c("psu_od", "psu_zlen")), ],
       aes(x = muslim, y = value)) +
  geom_point(aes(colour = indicator), shape = 1) +
  #geom_smooth(aes(colour = indicator, size = indicator), span = 0.8, se = FALSE) +
  #scale_size_manual("", values = c(1, 1, 1, 2), guide = FALSE) +
  scale_colour_discrete("") +
  facet_wrap(~ round) +
  xlab("PSU % Muslim") +
  ylab(expression(paste("Predicts better health ", symbol('\256')))) +
  theme_bw() +
  theme(legend.position = "bottom")





ggplot(child.psu)

ggplot(child.psu[child.psu$psu_od > 0 & child.psu$psu_od < 1, ], aes(x = psu_od)) +
  geom_line(stat = "density", aes(colour = round)) +
  theme_bw()

ggplot(child.psu[child.psu$psu_od > 0 & child.psu$psu_od < 1, ], aes(x = psu_od)) +
  geom_histogram() +
  facet_wrap(~round) +
  theme_bw()


ggplot(child.psu, aes(x = psu_od, y = psu_zlen)) +
  geom_point(shape = 1) +
  facet_wrap(~ round) +
  theme_bw()


ggplot(child.data, aes(x = zlen)) +
  geom_line(stat = "density", aes(colour = factor(hh.hindu))) +
  facet_wrap(~ round) +
  theme_bw()


ggplot(child.data[child.data$flag.zlen == 0 & !is.na(child.data$flag.zlen) &
                    child.data$child.age.mos < 36, ],
       aes(x = psu.od_loo, y = zlen)) +
  geom_smooth(aes(group = factor(hh.od), colour = factor(hh.od)), se = FALSE) +
  scale_colour_discrete("HH OD") +
  xlab("PSU % Neighbor OD") +
  ylab("Height-for-age z-score") +
  facet_wrap(~ round) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file = paste0(figdir, "/hfa_vs_psu_od_by_round.pdf"), width = 8, height = 6)

ggplot(child.data[child.data$flag.zlen == 0 & !is.na(child.data$flag.zlen), ],
       aes(x = zlen)) +
  geom_line(stat = "density") +
  facet_wrap(~ round) +
  theme_bw()

ggplot(child.data[child.data$flag.zlen == 0 & !is.na(child.data$flag.zlen) &
                    child.data$child.age.mos < 36, ],
       aes(x = psu.muslim, y = zlen)) +
  geom_smooth(aes(group = factor(hh.hindu), colour = factor(hh.hindu)), se = FALSE) +
  scale_colour_discrete("HH Hindu") +
  xlab("PSU % Muslim") +
  ylab("Height-for-age z-score") +
  facet_wrap(~ round) +
  theme_bw() +
  theme(legend.position = "bottom")
