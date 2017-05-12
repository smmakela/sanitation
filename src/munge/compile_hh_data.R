# Code to compile household-level information from NFHS1-3


# ##############################################################################
# Setup of libraries and directories
# ##############################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(foreign)
library(survey)

nfhsdir <- "/Users/susanna/Documents/NFHS/"
figdir <- "/Users/susanna/projects/sanitation/output/figures/"

# ##############################################################################
# NFHS1
# ##############################################################################
# Here there are actually TWO HH files, one that has info on religion and the
# other that has info on hh size and sanitation facilities

# ------------------------------------------------------------------------------
# Part 1 -- info on HH head religion

# Use convert.factors = FALSE so the state codes don't get messed up
nfhs1a <- read.dta(paste0(nfhsdir, "/NFHS1/iahh21dt/IAHH21FL.dta"),
                   convert.factors = FALSE)

# Generate state, urban/rural, household numbers, using the standard NFHS variable names
nfhs1a$hv024 <- nfhs1a$hhstate
nfhs1a$hv025 <- nfhs1a$hhtype
nfhs1a$hv002 <- nfhs1a$hhnumber

# Remake hv001 (PSU id) since it's coded differently than in the HH recode
# (doesn't include state number)
nfhs1a$psustr <- as.character(nfhs1a$hhpsu)
nfhs1a$hv001str <- NA
i1 <- nchar(nfhs1a$psustr) == 1
nfhs1a$hv001str[i1] <- paste0(nfhs1a$hv024[i1], "00", nfhs1a$psustr[i1])
i2 <- nchar(nfhs1a$psustr) == 2
nfhs1a$hv001str[i2] <- paste0(nfhs1a$hv024[i2], "0", nfhs1a$psustr[i2])
i3 <- nchar(nfhs1a$psustr) == 3
nfhs1a$hv001str[i3] <- paste0(nfhs1a$hv024[i3], nfhs1a$psustr[i3])
nfhs1a$hv001 <- as.integer(nfhs1a$hv001str)

# Check for duplicates by state/region/psu/hh -- number of rows in dupcheck gives
# number of duplicated HH records
nfhs1a %>%
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
  dplyr::summarise(num.obs = n()) %>%
  dplyr::filter(num.obs > 1) -> dupcheck
nrow(dupcheck) # 6 rows = 3 duplicates
table(dupcheck$num.obs)

  # 1st duplicate
  nfhs1a %>%
    dplyr::filter(hv024 == dupcheck$hv024[1] & hv025 == dupcheck$hv025[1] &
                    hv001 == dupcheck$hv001[1] & hv002 == dupcheck$hv002[1]) -> d1
  identical(d1[1, ], d1[2, ]) # super picky
  all.equal(d1[1, ], d1[2, ]) # less strict
  
  # 2nd duplicate
  nfhs1a %>%
    dplyr::filter(hv024 == dupcheck$hv024[2] & hv025 == dupcheck$hv025[2] &
                    hv001 == dupcheck$hv001[2] & hv002 == dupcheck$hv002[2]) -> d2
  identical(d2[1, ], d2[2, ])
  all.equal(d2[1, ], d2[2, ])
  
  # 3rd duplicate
  nfhs1a %>%
    dplyr::filter(hv024 == dupcheck$hv024[3] & hv025 == dupcheck$hv025[3] &
                    hv001 == dupcheck$hv001[3] & hv002 == dupcheck$hv002[3]) -> d3
  identical(d3[1, ], d3[2, ])
  all.equal(d3[1, ], d3[2, ])

  # The actual duplicate (the others are just duplicates in terms of row nubmers)
  # is in the code for caste of HH head, but we don't have a map for these
  # anyway so it doesn't matter that they're different (it's the detailed caste
  # code, not just OBC/SC/ST/other categorization)
  
# Just pick one of the duplicated rows to keep
nfhs1a %>%
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
  dplyr::mutate(rownum = row_number()) %>%
  dplyr::filter(rownum == 1) -> nfhs1a
# check that it worked
nfhs1a %>%
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
  dplyr::summarise(num.obs = n()) -> dupcheck2
table(dupcheck2$num.obs)

# Make categorical var and indicator for HH head being Hindu, Muslim, and other
nfhs1a$hh.religion <- NA
nfhs1a$hh.religion[nfhs1a$h032 == 1] <- "Hindu"
nfhs1a$hh.religion[nfhs1a$h032 == 7] <- "Muslim"
nfhs1a$hh.religion[nfhs1a$h032 != 1 & nfhs1a$h032 != 7] <- "Other"
nfhs1a$hh.hindu <- as.numeric(nfhs1a$h032 == 1)
nfhs1a$hh.muslim <- as.numeric(nfhs1a$h032 == 7)
nfhs1a$hh.other_relig <- as.numeric(nfhs1a$h032 != 1 & nfhs1a$h032 != 7)

# Keep just the variables we need
nfhs1a <- dplyr::select(nfhs1a, hv024, hv025, hv001, hv002, hh.religion,
                        hh.hindu, hh.muslim, hh.other_relig)

# ------------------------------------------------------------------------------
# Part 2 -- number of de jure HH members, asset ownership

nfhs1b <- read.dta(paste0(nfhsdir, "/NFHS1/iahr23dt/IAHR23FL.dta"),
                   convert.factors = FALSE)

# Make string version of state UGH and merge to data
state.map <- attr(nfhs1b, "label.table")$hv024
state.map <- data.frame(hv024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name[state.map$state.name == "arunachalpradesh"] <- "arunachal pradesh"
rownames(state.map) <- NULL
nfhs1b <- left_join(nfhs1b, state.map, by = "hv024")
                    
# Check for duplicate HH records
nfhs1b %>%
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
  dplyr::summarise(num.obs = n()) -> dupcheck
table(dupcheck$num.obs)
# note there are 3 more rows here than in nfhs1a, probably due to those 3
# duplicates we dropped -- they must've had the hh numbers miscoded

# HH size -- "number of de jure members"
nfhs1b$hh.size <- nfhs1b$hv012

# Indicator for HH open defecation (there are no NA's here)
nfhs1b$hh.od <- as.numeric(nfhs1b$hv205 == 31)
# table(nfhs1b$hv205, useNA="ifany") # NA check

# Indicator for improved water source
# improved sources are: piped into residence (11), public tap (12),
# handpump in yard/plot (23), public handpump (24)
nfhs1b$hh.improved_water <- as.numeric(nfhs1b$hv201 %in% c(11, 12, 23, 24))
nfhs1b$hh.improved_water[nfhs1b$hv201 == 99] <- NA

# Indicators for electricity + asset ownership
asset.names <- c("electricity", "radio", "tv", "fridge", "bike", "motorcycle",
                 "car", "clock", "sewing")
asset.names2 <- paste0("hh.", asset.names)
asset.vars <- c(paste0("hv", c(206:212)), "sh042b", "sh042a")
for (j in 1:length(asset.names)) {
  new.var <- paste0("hh.", asset.names[j])
  old.var <- asset.vars[j]
  nfhs1b[[new.var]] <- as.numeric(nfhs1b[[old.var]] == 1)
  na.inds <- nfhs1b[[old.var]] == 9
  nfhs1b[[new.var]][na.inds] <- NA
}

# Indicator for if head of household is male *and* his parents live in the hh
nfhs1b %>%
  dplyr::select(hhid, hv219, contains("hv101")) %>%
  dplyr::mutate_each(funs(ifelse(. == 6, 1, 0)), -c(hhid, hv219)) %>%
  dplyr::mutate(has_parents = rowSums(select(., contains("hv101")), na.rm = TRUE)) %>%
  dplyr::select(hhid, hv219, has_parents) %>%
  dplyr::mutate(hh.husb_parents_in_hh = ifelse(has_parents > 0 & hv219 == 1, 1, 0)) %>%
  dplyr::select(hhid, hh.husb_parents_in_hh) -> tmp
nfhs1b <- left_join(nfhs1b, tmp, by = "hhid")

# Keep just the variables we need
nfhs1b %>%
  dplyr::select_(.dots = c("hhid", "hv024", "state.name", "hv025",
                           "hv001", "hv002", "hv005", "hh.size",
                           "hh.size", "hh.od", "hh.improved_water",
                           "hh.husb_parents_in_hh", asset.names2)) %>%
  dplyr::mutate(round = "NFHS1") -> nfhs1b

# ------------------------------------------------------------------------------
# Join parts 1 and 2

nfhs1 <- inner_join(nfhs1a, nfhs1b, by = c("hv024", "hv025", "hv001", "hv002"))
nfhs1 <- dplyr::rename(nfhs1,
                       psu.id = hv001,
                       hh.id = hv002,
                       hh.sample.wt = hv005,
                       state = hv024,
                       rural.urban = hv025) -> nfhs1
  
cat("dims of nfhs1a, nfhs1b, nfhs1:\n")
cat(dim(nfhs1a), "\n")
cat(dim(nfhs1b), "\n")
cat(dim(nfhs1), "\n")

# ##############################################################################
# NFHS2
# ##############################################################################
nfhs2 <- read.dta(paste0(nfhsdir, "/NFHS2/iahr42dt/IAHR42FL.DTA"),
                  convert.factors = FALSE)

# Check for duplicate HH records
nfhs2 %>%
  dplyr::group_by(hhid) %>%
  dplyr::summarise(num.obs = n()) -> dupcheck
table(dupcheck$num.obs)

# Make string version of state UGH and merge to data
state.map <- attr(nfhs2, "label.table")$hv024
state.map <- data.frame(hv024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name[state.map$state.name == "arunachalpradesh"] <- "arunachal pradesh"
rownames(state.map) <- NULL
nfhs2 <- left_join(nfhs2, state.map, by = "hv024")

# Make categorical var and indicator for HH head being Hindu, Muslim, and other
nfhs2$hh.religion <- NA
nfhs2$hh.religion[nfhs2$sh39 == 1] <- "Hindu"
nfhs2$hh.religion[nfhs2$sh39 == 2] <- "Muslim"
nfhs2$hh.religion[nfhs2$sh39 != 1 & nfhs2$sh39 != 2] <- "Other"
nfhs2$hh.hindu <- as.numeric(nfhs2$sh39 == 1)
nfhs2$hh.muslim <- as.numeric(nfhs2$sh39 == 2)
nfhs2$hh.other_relig <- as.numeric(nfhs2$sh39 != 1 & nfhs2$sh39 != 2)

# Indicator for HH open defecation (drop the 8 NA's)
nfhs2$hh.od <- as.numeric(nfhs2$hv205 == 31)

# HH size -- "number of de jure household members"
nfhs2$hh.size <- nfhs2$hv009

# Indicator for improved water source
# improved sources are: piped into residence/yard/plot (11), public tap (12),
# handpump in residence/yard/plot (21), public handpump (22), well in
# residence/yard/plot (23), public covered well (25), rainwater (41)
# (for whatever reason, it seems like the JMP India country data excel sheet
# doesn't include rainwater as improved for NFHS1 but does for NFHS2 -- doesn't
# make much of a difference either way though)
nfhs2$hh.improved_water <- as.numeric(nfhs2$hv201 %in% c(11, 12, 21, 22, 23, 25, 41))

# Indicators for electricity + asset ownership
asset.names <- c("electricity", "radio", "tv", "fridge", "bike", "motorcycle",
                 "car", "clock", "sewing")
asset.names2 <- paste0("hh.", asset.names)
asset.vars <- c(paste0("hv", c(206:212)), "sh47f", "sh47j")
for (j in 1:length(asset.names)) {
  new.var <- paste0("hh.", asset.names[j])
  old.var <- asset.vars[j]
  nfhs2[[new.var]] <- as.numeric(nfhs2[[old.var]] == 1)
  na.inds <- nfhs2[[old.var]] == 9
  nfhs2[[new.var]][na.inds] <- NA
}

# Indicator for if head of household is male *and* his parents live in the hh
nfhs2 %>%
  dplyr::select(hhid, hv219, contains("hv101")) %>%
  dplyr::mutate_each(funs(ifelse(. == 6, 1, 0)), -c(hhid, hv219)) %>%
  dplyr::mutate(has_parents = rowSums(select(., contains("hv101")), na.rm = TRUE)) %>%
  dplyr::select(hhid, hv219, has_parents) %>%
  dplyr::mutate(hh.husb_parents_in_hh = ifelse(has_parents > 0 & hv219 == 1, 1, 0)) %>%
  dplyr::select(hhid, hh.husb_parents_in_hh) -> tmp
nfhs2 <- left_join(nfhs2, tmp, by = "hhid")

# Keep just the variables we need
nfhs2 %>%
  dplyr::select_(.dots = c("hhid", "hv024", "state.name", "hv025",
                           "hv001", "hv002", "hv005", "hh.religion",
                           "hh.hindu", "hh.muslim", "hh.other_relig",
                           "hh.size", "hh.od", "hh.improved_water",
                           "hh.husb_parents_in_hh", asset.names2)) %>%
  dplyr::rename(psu.id = hv001,
                hh.id = hv002,
                hh.sample.wt = hv005,
                state = hv024,
                rural.urban = hv025) %>%
  dplyr::mutate(round = "NFHS2") -> nfhs2

# ##############################################################################
# NFHS3
# ##############################################################################
nfhs3 <- read.dta(paste0(nfhsdir, "/NFHS3/iahr52dt/IAHR52FL.dta"),
                  convert.factors = FALSE)

# Check for duplicate HH records
nfhs3 %>%
  dplyr::group_by(hhid) %>%
  dplyr::summarise(num.obs = n()) -> dupcheck
table(dupcheck$num.obs)

# Make string version of state UGH and merge to data -- for NFHS3 we also have
# to change the state labels that come with the data so that they match the ones
# in NFHS1 and 2
state.map <- attr(nfhs3, "label.table")$LABC
state.map <- data.frame(hv024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name <- substr(state.map$state.name, 6, nchar(state.map$state.name))
state.map$state.name[state.map$state.name == "jammu and kashmir"] <- "jammu"
state.map$state.name[state.map$state.name == "delhi"] <- "new delhi"
rownames(state.map) <- NULL
nfhs3 <- left_join(nfhs3, state.map, by = "hv024")

# Make categorical var and indicator for HH head being Hindu, Muslim, and other
nfhs3$hh.religion <- NA
nfhs3$hh.religion[nfhs3$sh44 == 1] <- "Hindu"
nfhs3$hh.religion[nfhs3$sh44 == 2] <- "Muslim"
nfhs3$hh.religion[nfhs3$sh44 != 1 & nfhs3$sh44 != 2] <- "Other"
nfhs3$hh.hindu <- as.numeric(nfhs3$sh44 == 1)
nfhs3$hh.muslim <- as.numeric(nfhs3$sh44 == 2)
nfhs3$hh.other_relig <- as.numeric(nfhs3$sh44 != 1 & nfhs3$sh44 != 2)

# Indicator for HH open defecation
nfhs3$hh.od <- as.numeric(nfhs3$hv205 %in% c(30, 31))

# HH size -- "number of usual household members" (same as de jure I think)
nfhs3$hh.size <- nfhs3$hv009

# Indicator for improved water source
# improved sources are: piped into dwelling (11), piped into yard/plot (12),
# public tap/standpipe (13), tube well or borehole (21), protected well (31),
# protected spring (41), rainwater (51), bottled water (71)
# handpump in residence/yard/plot (21), public handpump (22), well in
# residence/yard/plot (23), public covered well (25), rainwater (41)
# (for whatever reason, it seems like the JMP India country data excel sheet
# doesn't include rainwater as improved for NFHS1 but does for NFHS2 -- doesn't
# make much of a difference either way though)
nfhs3$hh.improved_water <- as.numeric(nfhs3$hv201 %in% c(11:13, 21, 31, 51, 71))

# Indicators for electricity + asset ownership
asset.names <- c("electricity", "radio", "tv", "fridge", "bike", "motorcycle",
                 "car", "clock", "sewing")
asset.names2 <- paste0("hh.", asset.names)
asset.vars <- c(paste0("hv", c(206:212)), "hv243b", "sh47k")
for (j in 1:length(asset.names)) {
  new.var <- paste0("hh.", asset.names[j])
  old.var <- asset.vars[j]
  nfhs3[[new.var]] <- as.numeric(nfhs3[[old.var]] == 1)
  na.inds <- nfhs3[[old.var]] == 9
  nfhs3[[new.var]][na.inds] <- NA
}

# Indicator for if head of household is male *and* his parents live in the hh
nfhs3 %>%
  dplyr::select(hhid, hv219, contains("hv101")) %>%
  dplyr::mutate_each(funs(ifelse(. == 6, 1, 0)), -c(hhid, hv219)) %>%
  dplyr::mutate(has_parents = rowSums(select(., contains("hv101")), na.rm = TRUE)) %>%
  dplyr::select(hhid, hv219, has_parents) %>%
  dplyr::mutate(hh.husb_parents_in_hh = ifelse(has_parents > 0 & hv219 == 1, 1, 0)) %>%
  dplyr::select(hhid, hh.husb_parents_in_hh) -> tmp
nfhs3 <- left_join(nfhs3, tmp, by = "hhid")

# Keep just the variables we need
nfhs3 %>%
  dplyr::select_(.dots = c("hhid", "hv024", "state.name", "hv025",
                           "hv001", "hv002", "hv005", "hh.religion",
                           "hh.hindu", "hh.muslim", "hh.other_relig",
                           "hh.size", "hh.od", "hh.improved_water",
                           "hh.husb_parents_in_hh", asset.names2)) %>%
  dplyr::rename(psu.id = hv001,
                hh.id = hv002,
                hh.sample.wt = hv005,
                state = hv024,
                rural.urban = hv025) %>%
  dplyr::mutate(round = "NFHS3") -> nfhs3

# ##############################################################################
# Append all 3 together and calculate PSU-level characteristics
# ##############################################################################
nfhs <- rbind(data.frame(nfhs1), data.frame(nfhs2), data.frame(nfhs3))
nfhs$psu.od_denom <- ifelse(is.na(nfhs$hh.od), NA, 1)
nfhs$hh.asset_prop <- rowSums(nfhs[, asset.names2], na.rm = TRUE) / length(asset.names2)
min.ss <- 10 # min PSU sample size
nfhs %>%
  dplyr::group_by(round, state, psu.id) %>%
  dplyr::mutate(psu.od_denom = sum(psu.od_denom, na.rm = TRUE) - 1,
                psu.od_loo = (sum(hh.od, na.rm = TRUE) - hh.od) / psu.od_denom,
                psu.muslim = mean(hh.muslim, na.rm = TRUE),
                psu.sample_size = n(),
                psu.flag_ss = ifelse(psu.sample_size < min.ss, 1, 0)) -> nfhs

saveRDS(nfhs, "/Users/susanna/projects/sanitation/data/nfhs/hh_data.rds")

hh_psu_data <- nfhs %>%
  dplyr::group_by(round, state.name, rural.urban, psu.id) %>%
  dplyr::summarise(prop_hh_assets = mean(hh.asset_prop, na.rm = TRUE),
                   prop_hh_water = mean(hh.improved_water, na.rm = TRUE),
                   prop_hh_muslim = mean(hh.muslim, na.rm = TRUE),
                   prop_hh_hindu = mean(hh.hindu, na.rm = TRUE),
                   prop_hh_od = mean(hh.od, na.rm = TRUE),
                   ss_hh_assets = sum(!is.na(hh.asset_prop)),
                   ss_hh_water = sum(!is.na(hh.improved_water)),
                   ss_hh_muslim = sum(!is.na(hh.muslim)),
                   ss_hh_hindu = sum(!is.na(hh.hindu)),
                   ss_hh_od = sum(!is.na(hh.od)))

saveRDS(hh_psu_data,
        "/Users/susanna/projects/sanitation/data/nfhs/hh_psu_data.rds")


#nfhs$stratum <- paste0(as.character(nfhs$state.name), "_", nfhs$hv025)
#nfhs$wt <- nfhs$hh.sample.wt / 1e6

# Drop all HH's headed by "other" religion
nfhs <- dplyr::filter(nfhs, muslim_hhh == 1 | hindu_hhh == 1)

# Create svydesign object to get weighted proportions
# nfhs.design.obj <- svydesign(id = ~hv001, strata = ~stratum, data = nfhs,
#                              weights = ~wt, nest = TRUE)
# svyby(~electricity, ~round, design = nfhs.design.obj, FUN = svymean, na.rm = TRUE)


# PSU-level data
min.ss <- 10
nfhs %>%
  dplyr::group_by(round, state, psu.id) %>%
  dplyr::summarise(od = mean(od_hh, na.rm = TRUE),
                   muslim = mean(muslim_hhh, na.rm = TRUE),
                   electricity = mean(electricity, na.rm = TRUE),
                   improved_water = mean(improved_water, na.rm = TRUE),
                   asset_prop = mean(asset_prop, na.rm = TRUE),
                   sample_size = n()) %>%
  dplyr::mutate(flag_ss = ifelse(sample_size < min.ss, 1, 0)) -> nfhs_psu

# Summarise the differences between flagged PSUs and non-flagged ones
# nfhs_psu %>%
#   dplyr::group_by(flag_ss) %>%
#   dplyr::summarise_each(funs(min, mean, median, max, sd),
#                         od, muslim, electricity, improved_water,
#                                  asset_prop) -> ff
# round(t(ff), digits = 2)

# nfhs %>%
#   dplyr::group_by(round, hv024, hv001) %>%
#   dplyr::summarise(od = mean(od_hh, na.rm = TRUE),
#                    muslim = mean(muslim_hhh, na.rm = TRUE),
#                    electricity = mean(electricity, na.rm = TRUE),
#                    improved_water = mean(improved_water, na.rm = TRUE),
#                    asset_prop = mean(asset_prop, na.rm = TRUE)) %>%
#   dplyr::mutate(muslim_cat = ntile(muslim, 10)) %>%
#   dplyr::group_by(round, muslim_cat) %>%
#   dplyr::summarise(od = mean(od, na.rm = TRUE),
#                    electricity = mean(electricity, na.rm = TRUE),
#                    improved_water = mean(improved_water, na.rm = TRUE),
#                    asset_prop = mean(asset_prop, na.rm = TRUE)) -> means_by_muslim_cat

# Plot prop muslim vs prop OD (both at PSU level)

# Partial replication of Figure 2:
nfhs_psu$latrine_use <- 1 - nfhs_psu$od
# DROP PSUs with sample size < 15
nfhs_psu_filt <- dplyr::filter(nfhs_psu, flag_ss == 0)
df <- gather(nfhs_psu_filt, key = indicator, value = value,
             -c(muslim, round, state, psu.id))
df <- dplyr::filter(df, indicator != "od")
df$indicator <- as.character(df$indicator)
ggplot(df[df$indicator != "flag_ss" & df$indicator != "sample_size", ],
       aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator, size = indicator), span = 0.8, se = FALSE) +
  scale_size_manual("", values = c(1, 1, 1, 2), guide = FALSE) +
  scale_colour_discrete("") +
  facet_wrap(~ round) +
  xlab("PSU % Muslim") +
  ylab(expression(paste("Predicts better health ", symbol('\256')))) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file = paste0(figdir, "/loess_psu_indicators_by_round_all.pdf"),
       width = 8, height = 6)

ggplot(df[df$muslim > 0 & df$muslim < 1 &
            df$indicator != "flag_ss" & df$indicator != "sample_size", ],
       aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator, size = indicator), span = 0.5, se = FALSE) +
  scale_size_manual("", values = c(1, 1, 1, 2), guide = FALSE) +
  scale_colour_discrete("") +
  facet_wrap(~ round) +
  xlab("PSU % Muslim") +
  ylab(expression(paste("Predicts better health ", symbol('\256')))) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file = paste0(figdir, "/loess_psu_indicators_by_round_mixed.pdf"),
       width = 8, height = 6)

ggplot(df[df$indicator != "flag_ss" & df$indicator != "sample_size", ],
       aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator, size = indicator), span = 0.5, se = FALSE) +
  scale_size_manual("", values = c(1, 1, 1, 2), guide = FALSE) +
  scale_colour_discrete("") +
  xlab("PSU % Muslim") +
  ylab(expression(paste("Predicts better health ", symbol('\256')))) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file = paste0(figdir, "/loess_psu_indicators_all.pdf"),
       width = 8, height = 6)

ggplot(df[df$muslim > 0 & df$muslim < 1 &
            df$indicator != "flag_ss" & df$indicator != "sample_size", ],
       aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator, size = indicator), span = 0.5, se = FALSE) +
  scale_size_manual("", values = c(1, 1, 1, 2), guide = FALSE) +
  scale_colour_discrete("") +
  xlab("PSU % Muslim") +
  ylab(expression(paste("Predicts better health ", symbol('\256')))) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(file = paste0(figdir, "/loess_psu_indicators_mixed.pdf"),
       width = 8, height = 6)

ggplot(df[df$muslim > 0 & df$muslim < 1, ], aes(x = muslim)) +
  geom_histogram() +
  facet_wrap(~ round) +
  theme_bw()

ggplot(df[df$muslim > 0 & df$muslim < 1, ], aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator), method = "glm", formula = y ~ x) +
  facet_wrap(~ round) +
  theme_bw() +
  theme(legend.position = "bottom")


means_by_muslim_cat$latrine_use <- 1 - means_by_muslim_cat$od
df2 <- gather(means_by_muslim_cat, key = indicator, value = value,
             -c(muslim_cat, round))
df2 <- dplyr::filter(df2, indicator != "od")

ggplot(df2, aes(x = muslim_cat, y = value)) +
  geom_point(aes(colour = indicator)) +
  geom_line(aes(colour = indicator)) +
  facet_wrap(~ round) +
  theme_bw() +
  theme(legend.position = "bottom")
