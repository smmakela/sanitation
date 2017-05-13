# Analysis as in H&R 2006

# ##############################################################################
# Setup of libraries and directories
# ##############################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(rstan)
library(lme4)
library(designmatch)

datadir <- "/Users/susanna/projects/sanitation/data/nfhs/"
figdir <- "/Users/susanna/projects/sanitation/output/figures/"
codedir <- "/Users/susanna/projects/sanitation/src/"

# ##############################################################################
# Load and prep data
# ##############################################################################
child.data <- readRDS(paste0(datadir, "/compiled_child_data.rds"))
psu.data <- readRDS(paste0(datadir, "/hh_psu_data.rds"))

# See how many mothers/households are in our data
tmp <- child.data %>%
  dplyr::group_by(round, state.name, psu.id, hh.id, mom.caseid) %>%
  dplyr::summarise(num.kids = n())
tmp2 <- tmp %>%
  dplyr::group_by(round, state.name, psu.id, hh.id) %>%
  dplyr::summarise(num.kids = n())

# Prop of moms with X number of kids
table(tmp$num.kids) / nrow(tmp)
table(tmp2$num.kids) / nrow(tmp)

# Prop of kids with X number of siblings
table(tmp$num.kids) / sum(tmp$num.kids)
table(tmp2$num.kids) / sum(tmp2$num.kids)

# PSUs by round
psus.before <- table(psu.data$round)

# Drop where the sample size is < 15 hh/PSU
psu.data$min_ss <- pmin(psu.data$ss_hh_assets, psu.data$ss_hh_hindu,
                        psu.data$ss_hh_muslim, psu.data$ss_hh_od,
                        psu.data$ss_hh_water)
psu.data <- dplyr::filter(psu.data, min_ss >= 15)

# PSUs by round
table(psu.data$round)
cat("Number of PSUs dropped b/c of small sample sizes:\n")
psus.before - table(psu.data$round)

# Make rural/urban into factor
psu.data$rural.urban <- factor(psu.data$rural.urban, levels = c(1,2),
                               labels = c("urban", "rural"))
child.data$rural.urban <- factor(child.data$rural.urban, levels = c(1,2),
                               labels = c("urban", "rural"))

# Join psu data to child data
child.data <- left_join(child.data, psu.data,
                        by = c("round", "state.name", "psu.id", "rural.urban"))

# Plots of zlen by round, OD
ggplot(child.data) +
  geom_line(stat = "density", aes(x = zlen, colour = factor(hh.od))) +
  facet_grid(~ round) +
  xlab("Height-for-age z-score") +
  ylab("Density") +
  scale_colour_discrete("HH OD status") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "zlen_density.pdf"), width = 5, height = 3)

# Number of obs by round
aa <- table(child.data$round)
xtable(aa)

# ##############################################################################
# Empirically identify high-OD PSUs by comparing to national avgs by rural/urban
# and round
# ##############################################################################
urban1 <- 0.241
urban2 <- 0.192
urban3 <- 0.168
rural1 <- 0.871
rural2 <- 0.808
rural3 <- 0.740

# In nfhs coding, 1 = urban, 2 = rural
natl.od.avgs <- data.frame(round = rep(paste0("NFHS", c(1:3)), times = 2),
                           rural.urban = rep(c(1,2), each = 3),
                           avg_od = c(urban1, urban2, urban3, rural1, rural2, rural3))
natl.od.avgs$round <- as.character(natl.od.avgs$round)
natl.od.avgs$rural.urban <- factor(natl.od.avgs$rural.urban, levels = c(1,2),
                                   labels = c("urban", "rural"))
psu.data <- left_join(psu.data, natl.od.avgs, by = c("round", "rural.urban"))

# Mark whether is high-od psu
psu.data$high_od_psu <- as.numeric(psu.data$prop_hh_od > psu.data$avg_od)

# Plot
ggplot(psu.data) +
  geom_histogram(aes(x = prop_hh_od)) +
  geom_vline(aes(xintercept = avg_od)) +
  facet_grid(round ~ rural.urban) +
  xlab("PSU OD rate") +
  ylab("Number of PSUs") +
  theme_bw()
ggsave(paste0(figdir, "/psu_od_rate_hist.pdf"), width = 4, height = 3)

# ##############################################################################
# Estimate propensity of adopting high OD rate
# ##############################################################################
# Combine states that were newly created with their parent states
psu.data$state.name[psu.data$state.name == "chhattisgarh"] <- "madhya pradesh"
psu.data$state.name[psu.data$state.name == "jharkhand"] <- "bihar"
psu.data$state.name[psu.data$state.name == "uttaranchal"] <- "uttar pradesh"
psu.data$state.name[psu.data$state.name == "uttaranchal"] <- "uttar pradesh"
# sikkim isn't a new state but doesn't have any obs in NFHS1 so add to
# west bengal since that's the state it's next to...
psu.data$state.name[psu.data$state.name == "sikkim"] <- "west bengal"

stan.data.psu <- list(N = nrow(psu.data),
                      J = length(unique(psu.data$state.name)),
                      round_2 = as.numeric(psu.data$round == "NFHS2"),
                      round_3 = as.numeric(psu.data$round == "NFHS3"),
                      state = as.integer(factor(psu.data$state.name)),
                      high_od = psu.data$high_od_psu,
                      rural = as.numeric(psu.data$rural.urban == "rural"),
                      assets = psu.data$prop_hh_assets,
                      water = psu.data$prop_hh_water,
                      muslim = psu.data$prop_hh_muslim,
                      hindu = psu.data$prop_hh_hindu)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_mod <- stan_model(file = paste0(codedir, "/psu_od_propensity.stan"))
stan_res <- sampling(stan_mod, data = stan.data.psu, iter = 1000, chains = 4)

# ##############################################################################
# See how well stan model balances observed covariates
# ##############################################################################
# Extract logit_Q
extr <- rstan::extract(stan_res)
logit_Q <- colMeans(extr$logit_Q)
rm(extr)

# Histogram of posterior dist of logit_Q
hist(logit_Q)

# Cut into X strata
Q_strats <- cut_number(logit_Q, n = 10)
#Q_strats <- cut_interval(logit_Q, n = 10)
table(Q_strats, psu.data$high_od_psu, useNA="ifany")
tt <- data.frame(table(Q_strats, psu.data$high_od_psu))
names(tt) <- c("Q_strats", "high_od_psu", "freq")
no_overlap <- tt$Q_strats[tt$freq == 0]
one_obs <- tt$Q_strats[tt$freq == 1]

# Add strata to data
psu.data$logit_Q <- logit_Q
psu.data$Q_strats <- Q_strats
psu.data$Q_strats_lab <- as.integer(psu.data$Q_strats)
psu.data$overlap <- 1
psu.data$overlap[psu.data$Q_strats %in% no_overlap] <- 0
psu.data$one_obs <- 0
psu.data$one_obs[psu.data$Q_strats %in% one_obs] <- 1

# Tables
ff <- psu.data %>%
  dplyr::group_by(Q_strats_lab, high_od_psu) %>%
  dplyr::summarise(n = n(),
                   mean_lq = mean(logit_Q),
                   sd_lq = sd(logit_Q))
ff0 <- ff[ff$high_od_psu == 0, c("Q_strats_lab", "n")]
ff0$high_od_psu <- NULL
ff1 <- ff[ff$high_od_psu == 1, "n"]
ff1$high_od_psu <- NULL
gg <- data.frame(ff0, ff1)
print(xtable(gg), include.rownames = FALSE)

hh <- psu.data %>%
  dplyr::group_by(Q_strats_lab, high_od_psu) %>%
  dplyr::summarise(prop1 = mean(round == "NFHS1"),
                   prop2 = mean(round == "NFHS2"),
                   prop3 = mean(round == "NFHS3"),
                   rural = mean(rural.urban == "rural"))
hh0 <- hh[hh$high_od_psu == 0, ]
hh0$high_od_psu <- NULL
hh1 <- hh[hh$high_od_psu == 1, c("Q_strats_lab", "prop1", "prop2", "prop3", "rural")]
hh1$high_od_psu <- NULL
gg <- merge(hh0, hh1, by = "Q_strats_lab", suffixes = c("_0", "_1"))
vv <- c(paste0(paste0("prop1", c("_0", "_1"))),
        paste0(paste0("prop2", c("_0", "_1"))),
        paste0(paste0("prop3", c("_0", "_1"))),
        "rural_0", "rural_1")
gg <- gg[, c("Q_strats_lab", vv)]
print(xtable(gg), include.rownames = FALSE)
kk <- psu.data %>%
  dplyr::group_by(Q_strats_lab, high_od_lab) %>%
  dplyr::summarise(prop1 = mean(round == "NFHS1"),
                   prop2 = mean(round == "NFHS2"),
                   prop3 = mean(round == "NFHS3"),
                   rural = mean(rural.urban == "rural"))
kk <- gather(kk, key = var, value = value, -c(Q_strats_lab, high_od_lab))
kk$var[kk$var == "prop1"] <- "NFHS1"
kk$var[kk$var == "prop2"] <- "NFHS2"
kk$var[kk$var == "prop3"] <- "NFHS3"
kk$var[kk$var == "rural"] <- "Rural"
ggplot(kk) +
  geom_point(aes(x = value, y = factor(Q_strats_lab), colour = high_od_lab)) +
  scale_colour_discrete("") +
  facet_wrap(~var) +
  xlab("Proportion") +
  ylab("Stratum") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/cat_props_by_strat.pdf"), width = 4, height = 4)

# Plot density of logit_Q in each stratum of high and low OD PSUs
psu.data$high_od_lab <- factor(psu.data$high_od_psu, levels = c(0,1),
                               labels = c("Low-OD PSU", "High-OD PSU"))
ggplot(psu.data[psu.data$overlap == 1 & psu.data$one_obs == 0, ]) +
  geom_line(stat = "density", aes(x = logit_Q, colour = high_od_lab)) +
  scale_colour_discrete("") +
  facet_wrap(~ Q_strats_lab, scales = "free_x", nrow = 2) +
  xlab("logit(Q)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/logit_q_density_by_strat.pdf"), width = 6, height = 4)

# Plot densities of other covariates within strata
ggplot(psu.data[psu.data$overlap == 1 & psu.data$one_obs == 0, ]) +
  geom_line(stat = "density", aes(x = prop_hh_assets, colour = high_od_lab)) +
  scale_colour_discrete("") +
  facet_wrap(~ Q_strats_lab, scales = "free", nrow = 2) +
  xlab("Mean proportion of 7 assets owned") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/prop_assets_by_strat.pdf"), width = 7, height = 4)

ggplot(psu.data[psu.data$overlap == 1 & psu.data$one_obs == 0, ]) +
  geom_line(stat = "density", aes(x = prop_hh_water, colour = high_od_lab)) +
  scale_colour_discrete("") +
  facet_wrap(~ Q_strats_lab, scales = "free", nrow = 2) +
  xlab("Proportion of HH's with improved water") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/prop_water_by_strat.pdf"), width = 7, height = 4)

ggplot(psu.data[psu.data$overlap == 1 & psu.data$one_obs == 0, ]) +
  geom_line(stat = "density", aes(x = prop_hh_muslim, colour = high_od_lab)) +
  scale_colour_discrete("") +
  facet_wrap(~ Q_strats_lab, scales = "free", nrow = 2) +
  xlab("Proportion of Muslim HH's") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/prop_muslim_by_strat.pdf"), width = 7, height = 4)

ggplot(psu.data[psu.data$overlap == 1 & psu.data$one_obs == 0, ]) +
  geom_line(stat = "density", aes(x = prop_hh_hindu, colour = high_od_lab)) +
  scale_colour_discrete("") +
  facet_wrap(~ Q_strats_lab, scales = "free", nrow = 2) +
  xlab("Proportion of Hindu HH's") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/prop_hindu_by_strat.pdf"), width = 7, height = 4)

# ##############################################################################
# Estimate q_1 for kids in high-OD neighborhoods
# ##############################################################################
child.data <- left_join(child.data, natl.od.avgs, by = c("round", "rural.urban"))
child.data$high_od_psu <- child.data$prop_hh_od > child.data$avg_od
q1dat <- child.data[child.data$high_od_psu == 1, ]
q1dat <- na.omit(q1dat[, c("hh.id", "psu.id", "state.name", "round", "rural.urban",
                           "hh.od", "hh.asset_prop", "hh.improved_water",
                           "hh.muslim", "hh.hindu")])
psu_states <- unique(q1dat[, c("psu.id", "state.name", "round")])
state <- as.integer(factor(psu_states$state.name))
q1.stan <- list(N = nrow(q1dat),
                n_psu = nrow(unique(q1dat[, c("psu.id", "state.name", "round")])),
                # n_state = nrow(unique(q1dat[, c("state.name", "round")])),
                # state = state,
                psu = as.integer(factor(paste0(q1dat$psu.id, q1dat$state.name, q1dat$round))),
                round_2 = as.numeric(q1dat$round == "NFHS2"),
                round_3 = as.numeric(q1dat$round == "NFHS3"),
                hh_od = q1dat$hh.od,
                rural = as.numeric(q1dat$rural.urban == "rural"),
                assets = q1dat$hh.asset_prop,
                water = q1dat$hh.improved_water,
                muslim = q1dat$hh.muslim,
                hindu = q1dat$hh.hindu)

stan_mod_q1 <- stan_model(file = paste0(codedir, "/q1.stan"))
stan_res_q1 <- sampling(stan_mod_q1, data = q1.stan, iter = 2000, chains = 4)

q1dat$psu_unique <- as.integer(factor(paste0(q1dat$psu.id, q1dat$state.name, q1dat$round)))

# q1_mod <- glmer(hh.od ~ factor(round) + factor(rural.urban) +
#                   hh.muslim + hh.hindu + hh.improved_water +
#                   hh.asset_prop + factor(state.name) + (1|psu_unique),
#                 data = q1dat, family = binomial(link="logit"))

q1_mod <- glm(hh.od ~ factor(round) + factor(rural.urban) +
                  hh.muslim + hh.hindu + hh.improved_water +
                  hh.asset_prop + factor(state.name),
                data = q1dat, family = binomial(link="logit"))
q1dat$logit_q1 <- predict(q1_mod, type = "link")

# Calculate min(logit_q1) among OD hh's; children in NON-OD hh's with logit_q1
# below this cutoff are in
# stratify logit_q0
q1dat$q1_strats <- cut_interval(q1dat$logit_q1, n = 10)
table(q1dat$q1_strats, q1dat$hh.od)
q1dat$q1_strats_lab <- as.integer(q1dat$q1_strats)
#min_q1 <- min(q1dat$logit_q1[q1dat$q1_strats_lab == 1])
min_q1 <- min(q1dat$logit_q1[q1dat$hh.od == 1])

ggplot(q1dat) +
  geom_line(stat = "density", aes(x = logit_q1, colour = factor(hh.od))) +
  scale_colour_discrete("HH OD status") +
  xlab("logit(q1)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/logit_q1.pdf"), width = 4, height = 4)

q1dat$subpop <- NA
q1dat$subpop[q1dat$logit_q1 < min_q1] <- "C"
q1dat$subpop[q1dat$logit_q1 >= min_q1] <- "AB"

# ##############################################################################
# Estimate q_0 for kids in low-OD neighborhoods
# ##############################################################################
q0dat <- child.data[child.data$high_od_psu == 0, ]
q0dat <- na.omit(q0dat[, c("psu.id", "hh.id", "state.name", "round", "rural.urban",
                           "hh.od", "hh.asset_prop", "hh.improved_water",
                           "hh.muslim", "hh.hindu")])

# q0_mod <- glmer(hh.od ~ factor(round) + factor(rural.urban) + factor(state) +
#                   factor(hh.religion) + hh.improved_water +
#                   hh.husb_parents_in_hh + hh.electricity + hh. +
#                   prop_hh_assets + prop_hh_water +
#                   prop_hh_hindu + prop_hh_od + (1|psu.id),
#                 data = q0dat, family = binomial(link="logit"))
q0_mod <- glm(hh.od ~ factor(round) + factor(rural.urban) +
                  hh.muslim + hh.hindu + hh.improved_water +
                  hh.asset_prop + factor(state.name),
                data = q0dat, family = binomial(link="logit"))
q0dat$logit_q0 <- predict(q0_mod, type = "link")

# stratify logit_q0
q0dat$q0_strats <- cut_number(q0dat$logit_q0, n = 10)
table(q0dat$q0_strats, q0dat$hh.od)
q0dat$q0_strats_lab <- as.integer(q0dat$q0_strats)
# min_q0 <- min(q0dat$logit_q0[q0dat$q0_strats_lab == 1])
min_q0 <- min(q0dat$logit_q0[q0dat$hh.od == 1])

ggplot(q0dat) +
  geom_line(stat = "density", aes(x = logit_q0, colour = factor(hh.od))) +
  scale_colour_discrete("HH OD status") +
  xlab("logit(q0)") +
  ylab("Density") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(figdir, "/logit_q0.pdf"), width = 4, height = 4)


q0dat$subpop <- NA
q0dat$subpop[q0dat$logit_q0 < min_q0] <- "C"
q0dat$subpop[q0dat$logit_q0 >= min_q0] <- "A"

# ##############################################################################
# Estimate delta_Z0 for kids in subpop A
# ##############################################################################
subpopA <- q0dat[q0dat$subpop=="A",]
subpopA <- left_join(subpopA, child.data[, c("round", "state.name", "psu.id", "hh.id", "zlen")],
                     by = c("round", "state.name", "psu.id", "hh.id"))
subpopA$psu_unique <- as.integer(factor(paste0(subpopA$psu.id, subpopA$state.name, subpopA$round)))


m1 <- lmer(zlen ~ hh.od + factor(q0_strats) + logit_q0 + factor(round) +
             factor(state.name) + (1 + hh.od | state.name/psu_unique),
           data = subpopA)
summary(m1)

# ##############################################################################
# Estimate delta_Z0 for kids in subpop AB
# ##############################################################################
vv <- c("round", "state.name", "psu.id", "hh.id", "hh.od", "hh.asset_prop",
        "hh.improved_water", "hh.muslim", "hh.hindu")
qq0 <- q0dat[q0dat$subpop=="A", vv]
qq0$logit_q <- q0dat$logit_q0[q0dat$subpop=="A"]
qq1 <- q1dat[q1dat$subpop=="AB", vv]
qq1$logit_q <- q1dat$logit_q1[q1dat$subpop=="AB"]

subpopAB <- rbind(qq0, qq1)
subpopAB <- left_join(subpopAB, child.data[, c("round", "state.name", "psu.id", "hh.id", "zlen")],
                     by = c("round", "state.name", "psu.id", "hh.id"))
subpopAB$psu_unique <- as.integer(factor(paste0(subpopAB$psu.id,
                                                subpopAB$state.name,
                                                subpopAB$round)))


m2 <- lmer(zlen ~ logit_q + hh.od + (1 + hh.od | psu_unique),
           data = subpopAB)
summary(m2)
