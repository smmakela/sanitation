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
nfhs1a$religion_hhh <- NA
nfhs1a$religion_hhh[nfhs1a$h032 == 1] <- "Hindu"
nfhs1a$religion_hhh[nfhs1a$h032 == 7] <- "Muslim"
nfhs1a$religion_hhh[nfhs1a$h032 != 1 & nfhs1a$h032 != 7] <- "Other"
nfhs1a$hindu_hhh <- as.numeric(nfhs1a$h032 == 1)
nfhs1a$muslim_hhh <- as.numeric(nfhs1a$h032 == 7)
nfhs1a$other_hhh <- as.numeric(nfhs1a$h032 != 1 & nfhs1a$h032 != 7)

# Keep just the variables we need
nfhs1a <- dplyr::select(nfhs1a, hv024, hv025, hv001, hv002, religion_hhh,
                        hindu_hhh, muslim_hhh, other_hhh)

# ------------------------------------------------------------------------------
# Part 2 -- number of de jure HH members, asset ownership

nfhs1b <- read.dta(paste0(nfhsdir, "/NFHS1/iahr23dt/IAHR23FL.dta"),
                   convert.factors = FALSE)

# Check for duplicate HH records
nfhs1b %>%
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
  dplyr::summarise(num.obs = n()) -> dupcheck
table(dupcheck$num.obs)
# note there are 3 more rows here than in nfhs1a, probably due to those 3
# duplicates we dropped -- they must've had the hh numbers miscoded

# Make string version of state UGH and merge to data
state.map <- attr(nfhs1b, "label.table")$hv024
state.map <- data.frame(hv024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name[state.map$state.name == "arunachalpradesh"] <- "arunachal pradesh"
state.map$state.name <- as.character(state.map$state.name)
rownames(state.map) <- NULL
nfhs1b <- left_join(nfhs1b, state.map, by = "hv024")

# HH size -- "number of de jure members"
nfhs1b$hh_size <- nfhs1b$hv012

# Indicator for HH open defecation (there are no NA's here)
nfhs1b$od_hh <- as.numeric(nfhs1b$hv205 == 31)
# table(nfhs1b$hv205, useNA="ifany") # NA check

# Indicator for improved water source
# improved sources are: piped into residence (11), public tap (12),
# handpump in yard/plot (23), public handpump (24)
nfhs1b$improved_water <- as.numeric(nfhs1b$hv201 %in% c(11, 12, 23, 24))
nfhs1b$improved_water[nfhs1b$hv201 == 99] <- NA

# Indicators for electricity + asset ownership
asset.names <- c("electricity", "radio", "tv", "fridge", "bike", "motorcycle",
                 "car", "clock", "sewing")
asset.vars <- c(paste0("hv", c(206:212)), "sh042b", "sh042a")
for (j in 1:length(asset.names)) {
  new.var <- asset.names[j]
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
  dplyr::mutate(husb_parents_in_hh = ifelse(has_parents > 0 & hv219 == 1, 1, 0)) %>%
  dplyr::select(hhid, husb_parents_in_hh) -> tmp
nfhs1b <- left_join(nfhs1b, tmp, by = "hhid")

# Keep just the variables we need
nfhs1b <- dplyr::select_(nfhs1b, .dots = c("hhid", "hv024", "state.name", "hv025",
                                           "hv001", "hv002", "hv005", "hh_size",
                                           "hh_size", "od_hh", "improved_water",
                                           "husb_parents_in_hh", asset.names))

# ------------------------------------------------------------------------------
# Join parts 1 and 2

nfhs1 <- inner_join(nfhs1a, nfhs1b, by = c("hv024", "hv025", "hv001", "hv002"))
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
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
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
nfhs2$religion_hhh <- NA
nfhs2$religion_hhh[nfhs2$sh39 == 1] <- "Hindu"
nfhs2$religion_hhh[nfhs2$sh39 == 2] <- "Muslim"
nfhs2$religion_hhh[nfhs2$sh39 != 1 & nfhs2$sh39 != 2] <- "Other"
nfhs2$hindu_hhh <- as.numeric(nfhs2$sh39 == 1)
nfhs2$muslim_hhh <- as.numeric(nfhs2$sh39 == 2)
nfhs2$other_hhh <- as.numeric(nfhs2$sh39 != 1 & nfhs2$sh39 != 2)

# Indicator for HH open defecation (drop the 8 NA's)
nfhs2$od_hh <- as.numeric(nfhs2$hv205 == 31)

# HH size -- "number of de jure household members"
nfhs2$hh_size <- nfhs2$hv009

# Indicator for improved water source
# improved sources are: piped into residence/yard/plot (11), public tap (12),
# handpump in residence/yard/plot (21), public handpump (22), well in
# residence/yard/plot (23), public covered well (25), rainwater (41)
# (for whatever reason, it seems like the JMP India country data excel sheet
# doesn't include rainwater as improved for NFHS1 but does for NFHS2 -- doesn't
# make much of a difference either way though)
nfhs2$improved_water <- as.numeric(nfhs2$hv201 %in% c(11, 12, 21, 22, 23, 25, 41))

# Indicators for electricity + asset ownership
asset.names <- c("electricity", "radio", "tv", "fridge", "bike", "motorcycle",
                 "car", "clock", "sewing")
asset.vars <- c(paste0("hv", c(206:212)), "sh47f", "sh47j")
for (j in 1:length(asset.names)) {
  new.var <- asset.names[j]
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
  dplyr::mutate(husb_parents_in_hh = ifelse(has_parents > 0 & hv219 == 1, 1, 0)) %>%
  dplyr::select(hhid, husb_parents_in_hh) -> tmp
nfhs2 <- left_join(nfhs2, tmp, by = "hhid")

# Keep just the variables we need
nfhs2 <- dplyr::select_(nfhs2, .dots = c("hhid", "hv024", "state.name", "hv025",
                                         "hv001", "hv002", "hv005", "religion_hhh",
                                         "hindu_hhh", "muslim_hhh", "other_hhh",
                                         "hh_size", "od_hh", "improved_water",
                                         "husb_parents_in_hh", asset.names))

# ##############################################################################
# NFHS3
# ##############################################################################
nfhs3 <- read.dta(paste0(nfhsdir, "/NFHS3/iahr52dt/IAHR52FL.dta"),
                  convert.factors = FALSE)

# Check for duplicate HH records
nfhs3 %>%
  dplyr::group_by(hv024, hv025, hv001, hv002) %>%
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
nfhs3$religion_hhh <- NA
nfhs3$religion_hhh[nfhs3$sh44 == 1] <- "Hindu"
nfhs3$religion_hhh[nfhs3$sh44 == 2] <- "Muslim"
nfhs3$religion_hhh[nfhs3$sh44 != 1 & nfhs3$sh44 != 2] <- "Other"
nfhs3$hindu_hhh <- as.numeric(nfhs3$sh44 == 1)
nfhs3$muslim_hhh <- as.numeric(nfhs3$sh44 == 2)
nfhs3$other_hhh <- as.numeric(nfhs3$sh44 != 1 & nfhs3$sh44 != 2)

# Indicator for HH open defecation
nfhs3$od_hh <- as.numeric(nfhs3$hv205 %in% c(30, 31))

# HH size -- "number of usual household members" (same as de jure I think)
nfhs3$hh_size <- nfhs3$hv009

# Indicator for improved water source
# improved sources are: piped into dwelling (11), piped into yard/plot (12),
# public tap/standpipe (13), tube well or borehole (21), protected well (31),
# protected spring (41), rainwater (51), bottled water (71)
# handpump in residence/yard/plot (21), public handpump (22), well in
# residence/yard/plot (23), public covered well (25), rainwater (41)
# (for whatever reason, it seems like the JMP India country data excel sheet
# doesn't include rainwater as improved for NFHS1 but does for NFHS2 -- doesn't
# make much of a difference either way though)
nfhs3$improved_water <- as.numeric(nfhs3$hv201 %in% c(11:13, 21, 31, 51, 71))

# Indicators for electricity + asset ownership
asset.names <- c("electricity", "radio", "tv", "fridge", "bike", "motorcycle",
                 "car", "clock", "sewing")
asset.vars <- c(paste0("hv", c(206:212)), "hv243b", "sh47k")
for (j in 1:length(asset.names)) {
  new.var <- asset.names[j]
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
  dplyr::mutate(husb_parents_in_hh = ifelse(has_parents > 0 & hv219 == 1, 1, 0)) %>%
  dplyr::select(hhid, husb_parents_in_hh) -> tmp
nfhs3 <- left_join(nfhs3, tmp, by = "hhid")

# Keep just the variables we need
nfhs3 <- dplyr::select_(nfhs3, .dots = c("hhid", "hv024", "state.name", "hv025",
                                         "hv001", "hv002", "hv005", "religion_hhh",
                                         "hindu_hhh", "muslim_hhh", "other_hhh",
                                         "hh_size", "od_hh", "improved_water",
                                         "husb_parents_in_hh", asset.names))

# ##############################################################################
# Append all 3 together and calculate PSU-level OD and prop Muslim
# ##############################################################################
nfhs <- rbind(data.frame(nfhs1), data.frame(nfhs2), data.frame(nfhs3))
nfhs$round <- rep(c("NFHS1", "NFHS2", "NFHS3"),
                  times = c(nrow(nfhs1), nrow(nfhs2), nrow(nfhs3)))
nfhs$od_psu_denom <- ifelse(is.na(nfhs$od_hh), NA, 1)
nfhs$asset_prop <- rowMeans(nfhs[, c("radio", "tv", "fridge", "bike",
                                     "motorcycle", "car", "clock", "sewing")],
                            na.rm = TRUE)
nfhs$stratum <- paste0(as.character(nfhs$state.name), "_", nfhs$hv025)
nfhs$wt <- nfhs$hv005 / 1e6

# Drop all HH's headed by "other" religion
nfhs <- dplyr::filter(nfhs, muslim_hhh == 1 | hindu_hhh == 1)

# Create svydesign object to get weighted proportions
# nfhs.design.obj <- svydesign(id = ~hv001, strata = ~stratum, data = nfhs,
#                              weights = ~wt, nest = TRUE)
# svyby(~electricity, ~round, design = nfhs.design.obj, FUN = svymean, na.rm = TRUE)

nfhs %>%
  dplyr::group_by(round, hv024, hv001, hv002) %>%
  dplyr::mutate(od_psu_denom = sum(od_psu_denom, na.rm = TRUE) - 1,
                od_psu_loo = (sum(od_hh, na.rm = TRUE) - od_hh) / od_psu_denom,
                muslim_psu = mean(muslim_hhh, na.rm = TRUE)) -> nfhs

# PSU-level data
nfhs %>%
  dplyr::group_by(round, hv024, hv001) %>%
  dplyr::summarise(od = mean(od_hh, na.rm = TRUE),
                   muslim = mean(muslim_hhh, na.rm = TRUE),
                   electricity = mean(electricity, na.rm = TRUE),
                   improved_water = mean(improved_water, na.rm = TRUE),
                   asset_prop = mean(asset_prop, na.rm = TRUE),
                   sample_size = n()) %>%
  dplyr::mutate(flag_ss = ifelse(sample_size < 15, 1, 0)) -> nfhs_psu

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

# Partial replication of Figure 2:
nfhs_psu$latrine_use <- 1 - nfhs_psu$od
df <- gather(nfhs_psu, key = indicator, value = value,
             -c(muslim, round, hv024, hv001))
df <- dplyr::filter(df, indicator != "od")
ggplot(df[df$indicator != "sample_size", ], aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator), span = 0.8) +
  facet_wrap(~ round) +
  theme_bw()

ggplot(df[df$indicator == "sample_size", ], aes(x = muslim, y = value)) +
  geom_point() +
  facet_wrap(~ round) +
  theme_bw()


ggplot(df[df$muslim > 0 & df$muslim < 1, ], aes(x = muslim, y = value)) +
  geom_smooth(aes(colour = indicator), span = 0.5) +
  facet_wrap(~ round) +
  theme_bw() +
  theme(legend.position = "bottom")

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
