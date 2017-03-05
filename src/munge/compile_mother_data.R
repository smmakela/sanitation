# Code to compile mother-level information from NFHS 1-3
# NOTE: use v001, v002 to merge with HH data

# ##############################################################################
# Setup of libraries and directories
# ##############################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(foreign)

nfhsdir <- "/Users/susanna/Documents/NFHS/"

# ##############################################################################
# NFHS1
# ##############################################################################
nfhs1 <- read.dta(paste0(nfhsdir, "/NFHS1/iair23dt/IAIR23FL.dta"),
                  convert.factors = FALSE)

# Mother's age at interview
nfhs1$mom.age.mos.interview <- nfhs1$v008 - nfhs1$v011

# Mother's height in cm (not in NFHS1)
nfhs1$mom.height.cm <- NA

# Mother's education level and years, literacy
nfhs1$mom.edu.level <- nfhs1$v106
nfhs1$mom.edu.years <- nfhs1$v107
# whether mother can read
nfhs1$mom.literate <- as.numeric(nfhs1$s108 == 1)
nfhs1$mom.literate[nfhs1$s108 == 9 | is.na(nfhs1$s108)] <- NA

# Mother has own money (not in NFHS1)
nfhs1$mom.own.money <- NA

# Mother has (own) healthcare say (not in NFHS1)
nfhs1$mom.healthcare.say <- NA

# NFHS1 doesn't have info on hh structure; impute based on mother's relationship
# to head of hh
nfhs1$mom.lives.with.inlaws <- as.numeric(nfhs1$v150 == 4)
nfhs1$mom.lives.with.inlaws[nfhs1$v150 == 98 | nfhs1$v150 == 99 | is.na(nfhs1$v150)] <- NA

# Make string version of state UGH and merge to data
state.map <- attr(nfhs1, "label.table")$LABD
state.map <- data.frame(v024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name[state.map$state.name == "arunachalpradesh"] <- "arunachal pradesh"
rownames(state.map) <- NULL
nfhs1 <- left_join(nfhs1, state.map, by = "v024")

# Keep just the variables we need
nfhs1 %>%
  dplyr::select(caseid, v002, v003, v005, v006, v007, v008, v009, v010, v011,
                v012, v021, v024, v025, mom.age.mos.interview, mom.height.cm,
                mom.edu.level, mom.edu.years, mom.literate, mom.own.money,
                mom.healthcare.say, mom.lives.with.inlaws, state.name) %>%
  dplyr::rename(mom.caseid = caseid,
                hh.id = v002,
                mom.id = v003,
                sample.wt = v005,
                interview.mo = v006,
                interview.yr = v007,
                interview.cmc = v008,
                mom.birth.mo = v009,
                mom.birth.yr = v010,
                mom.birth.cmc = v011,
                mom.curr.age.yrs = v012,
                psu.id = v021,
                state = v024,
                urban = v025) %>%
  dplyr::mutate(round = "NFHS1")  -> nfhs1

# ##############################################################################
# NFHS2
# ##############################################################################
nfhs2 <- read.dta(paste0(nfhsdir, "/NFHS2/iair42dt/IAIR42FL.dta"),
                  convert.factors = FALSE)

# Mother's age at interview
nfhs2$mom.age.mos.interview <- nfhs2$v008 - nfhs2$v011

# Mother's height in cm
nfhs2$mom.height.cm <- nfhs2$v438/10

# Mother's education level and years, literacy
nfhs2$mom.edu.level <- nfhs2$v106
nfhs2$mom.edu.years <- nfhs2$v107
# whether mother can read
nfhs2$mom.literate <- nfhs2$s119

# Mother has own money
nfhs2$mom.own.money <- nfhs2$s513

# Mother has (own) healthcare say
nfhs2$mom.healthcare.say <- as.numeric(nfhs2$s511b %in% c(1, 3, 5))
nfhs2$mom.healthcare.say[is.na(nfhs2$s511b)] <- NA

# NFHS2 doesn't have info on hh structure; impute based on mother's relationship
# to head of hh 
nfhs2$mom.lives.with.inlaws <- as.numeric(nfhs2$v150 == 4)
nfhs2$mom.lives.with.inlaws[nfhs2$v150 == 98 | nfhs2$v150 == 99 | is.na(nfhs2$v150)] <- NA

# Make string version of state UGH and merge to data
state.map <- attr(nfhs2, "label.table")$v024
state.map <- data.frame(v024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name[state.map$state.name == "arunachalpradesh"] <- "arunachal pradesh"
rownames(state.map) <- NULL
nfhs2 <- left_join(nfhs2, state.map, by = "v024")

# Keep just the variables we need
nfhs2 %>%
  dplyr::select(caseid, v002, v003, v005, v006, v007, v008, v009, v010, v011,
                v012, v021, v024, v025, mom.age.mos.interview, mom.height.cm,
                mom.edu.level, mom.edu.years, mom.literate, mom.own.money,
                mom.healthcare.say, mom.lives.with.inlaws, state.name) %>%
  dplyr::rename(mom.caseid = caseid,
                hh.id = v002,
                mom.id = v003,
                sample.wt = v005,
                interview.mo = v006,
                interview.yr = v007,
                interview.cmc = v008,
                mom.birth.mo = v009,
                mom.birth.yr = v010,
                mom.birth.cmc = v011,
                mom.curr.age.yrs = v012,
                psu.id = v021,
                state = v024,
                urban = v025) %>%
  dplyr::mutate(round = "NFHS2")  -> nfhs2

# ##############################################################################
# NFHS3
# ##############################################################################
nfhs3 <- read.dta(paste0(nfhsdir, "/NFHS3/iair52dt/IAIR52FL.dta"),
                  convert.factors = FALSE)

# Mother's age at interview
nfhs3$mom.age.mos.interview <- nfhs3$v008 - nfhs3$v011

# Mother's height in cm
nfhs3$mom.height.cm <- nfhs3$v438

# Mother's education level and years, literacy
nfhs3$mom.edu.level <- nfhs3$v106
nfhs3$mom.edu.years <- nfhs3$v107
# whether mother can read all or part of a sentence
nfhs3$mom.literate <- as.numeric(nfhs3$v155 == 1 | nfhs3$v155 == 2)
# missings for "no card w/ required language", "blind/visually impaired",
# and "missing"
nfhs3$mom.literate[nfhs3$v155 == 3 | nfhs3$v155 == 4 | nfhs3$v155 == 9] <- NA

# Mother has own money
nfhs3$mom.own.money <- nfhs3$w124
nfhs3$mom.own.money[nfhs3$w124 == 9] <- NA

# Mother has (own) healthcare say
nfhs3$mom.healthcare.say <- as.numeric(nfhs3$v743a %in% c(1:3))
nfhs3$mom.healthcare.say[nfhs3$v743a == 9 | is.na(nfhs3$v743a)] <- NA

# Mother lives with husband's parents (actually this is for whether the HH is
# a nuclear one or not)
nfhs3$mom.lives.with.inlaws <- as.numeric(nfhs3$sstruc == 2)
nfhs3$mom.lives.with.inlaws[nfhs3$sstruc == 7] <- NA

# Make string version of state UGH and merge to data
state.map <- attr(nfhs3, "label.table")$LABD
state.map <- data.frame(v024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name <- substr(state.map$state.name, 6, nchar(state.map$state.name))
state.map$state.name[state.map$state.name == "jammu and kashmir"] <- "jammu"
state.map$state.name[state.map$state.name == "delhi"] <- "new delhi"
rownames(state.map) <- NULL
nfhs3 <- left_join(nfhs3, state.map, by = "v024")

# Keep just the variables we need
nfhs3 %>%
  dplyr::select(caseid, v002, v003, v005, v006, v007, v008, v009, v010, v011,
                v012, v021, v024, v025, mom.age.mos.interview, mom.height.cm,
                mom.edu.level, mom.edu.years, mom.literate, mom.own.money,
                mom.healthcare.say, mom.lives.with.inlaws, state.name) %>%
  dplyr::rename(mom.caseid = caseid,
                hh.id = v002,
                mom.id = v003,
                sample.wt = v005,
                interview.mo = v006,
                interview.yr = v007,
                interview.cmc = v008,
                mom.birth.mo = v009,
                mom.birth.yr = v010,
                mom.birth.cmc = v011,
                mom.curr.age.yrs = v012,
                psu.id = v021,
                state = v024,
                urban = v025) %>%
  dplyr::mutate(round = "NFHS3") -> nfhs3

# ##############################################################################
# Combine all rounds
# ##############################################################################
nfhs <- rbind(nfhs1, nfhs2, nfhs3)
