# Analysis as in H&R 2006

# ##############################################################################
# Setup of libraries and directories
# ##############################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(rstan)
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

# # For each PSU, calculate lower bound of CI for OD using the Agresti-Coull
# # interval
# z <- 1.96
# ac_n_tilde <- psu.data$ss_hh_od + z^2
# ac_p_tilde <- (1 / ac_n_tilde) * (psu.data$prop_hh_od*psu.data$ss_hh_od + (z^2)/2)
# ac_se <- sqrt(ac_p_tilde * (1 - ac_p_tilde) / ac_n_tilde)
# psu.data$ac_lower_95ci <- ac_p_tilde - z * ac_se
# 
# # Mark whether lower bound of AG CI 
# psu.data$high_od_psu <- as.numeric(psu.data$ac_lower_95ci > psu.data$avg_od)

# ggplot(psu.data) +
#   geom_histogram(aes(x = ac_lower_95ci)) +
#   geom_vline(aes(xintercept = avg_od)) +
#   facet_grid(round ~ rural.urban) +
#   theme_bw()


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
ggsave(paste0(figdir, "/psu_od_rate_hist.pdf"), width = 8, height = 8)

# ##############################################################################
# Estimate propensity of adopting high OD rate
# ##############################################################################
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
Q_strats <- 