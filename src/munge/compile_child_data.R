# Code to compile child-level information from NFHS 1-3
# NOTE: use v001, v002, v003 to merge with mother data
# ##############################################################################
# Setup of libraries and directories
# ##############################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(foreign)

nfhsdir <- "/Users/susanna/Documents/NFHS/"

# rounding fn from igrowup_standard.R
rounde <- function(x, digits = 0) {
  expo <- 10^digits
  x.ret <- ifelse(abs(x * expo) - floor(abs(x * expo)) < 0.5,
                  sign(x * expo) * floor(abs(x * expo)),
                  sign(x * expo) * (floor(abs(x * expo)) + 1)) / expo
  return(x.ret)
}

# ##############################################################################
# NFHS1
# ##############################################################################
nfhs1 <- read.dta(paste0(nfhsdir, "/NFHS1/iakr23dt/IAKR23FL.dta"),
                  convert.factors = FALSE)

# Check for duplicates
nfhs1 %>%
  dplyr::group_by(caseid, bord, v024) %>%
  dplyr::summarise(num.obs = n()) %>%
  dplyr::filter(num.obs > 1) -> dupcheck
dim(dupcheck)


# Make string version of state name
state.map <- attr(nfhs1, "label.table")$LABD
state.map <- data.frame(v024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name[state.map$state.name == "arunachalpradesh"] <- "arunachal pradesh"
rownames(state.map) <- NULL
nfhs1 <- left_join(nfhs1, state.map, by = "v024")

nfhs1$child.birth.order <- nfhs1$bord
nfhs1$child.multiple.birth <- nfhs1$b0
nfhs1$child.mom.age.birth <- (nfhs1$b3 - nfhs1$v011)/12 # child dob cmc - mom dob cmc
nfhs1$child.age.mos <- nfhs1$v008 - nfhs1$b3 # interview cmc - dob cmc
nfhs1$child.age.days <- rounde(nfhs1$child.age.mos*30.4375)
nfhs1$child.female <- as.numeric(nfhs1$b4 == 2) # 1 = male, 2 = female
nfhs1$child.alive <- nfhs1$b5
nfhs1$child.breastfed.mos <- nfhs1$m4
nfhs1$child.breastfed.mos[nfhs1$m4 == 94] <- 0 # 94 = never bf'ed
nfhs1$child.breastfed.mos[nfhs1$m4 == 95] <- nfhs1$child.age.mos[nfhs1$m4 == 95] # 95 = still bf'ing
nfhs1$child.breastfed.mos[nfhs1$m4 >= 97] <- NA # inconsistent/don't know/missing
nfhs1$child.inst.birth <- NA
nfhs1$child.inst.birth[nfhs1$m15 < 20] <- 0
nfhs1$child.inst.birth[nfhs1$m15 >= 20 & nfhs1$m15 <= 50] <- 1
nfhs1$child.size.at.birth <- nfhs1$m18
nfhs1$child.size.at.birth[nfhs1$m18 >= 8] <- NA
nfhs1$child.has.health.card <- as.numeric(nfhs1$h1 == 1 | nfhs1$h1 == 2)
nfhs1$child.has.health.card[nfhs1$h1 == 9 | is.na(nfhs1$h1)] <- NA
nfhs1$child.ever.vaccinated <- nfhs1$h10
nfhs1$child.ever.vaccinated[!(nfhs1$h10 %in% c(0, 1))] <- NA
nfhs1$child.wt.kg <- nfhs1$hw2 / 10
nfhs1$child.wt.kg[nfhs1$hw2 == 9999] <- NA
nfhs1$child.ht.cm <- nfhs1$hw3 / 10
nfhs1$child.ht.cm[nfhs1$hw3 == 9999] <- NA
nfhs1$child.how.meas[nfhs1$hw15 == 1] <- "Lying"
nfhs1$child.how.meas[nfhs1$hw15 == 2] <- "Standing"
nfhs1$child.how.meas[nfhs1$hw15 == 9 | is.na(nfhs1$hw15)] <- NA

# Calculate height-for-age z scores using code from WHO's igrowup_standard.R
lenanthro <- read.table("/Users/susanna/projects/sanitation/data/lenanthro.txt",
                        header = TRUE, skip = 0)
lenanthro %>%
  dplyr::rename(b4 = sex, child.age.days = age) -> lenanthro # to match nfhs1 names

nfhs1 <- left_join(nfhs1, lenanthro, by = c("b4", "child.age.days"))

# Adjust length/height measurement depending on whether child was measured
# lying down or standing -- if kids under 24 months (731 days) were measured
# standing up, add 0.7cm to their height. if kids over 24 months were measured
# lying down, subtract 0.7cm to their height. if the mode of measurement is
# missing, leave child length/height unchanged
nfhs1$child.ht.cm.adj <- ifelse(nfhs1$child.how.meas == "Lying" & nfhs1$child.age.days < 731,
                                nfhs1$child.ht.cm + 0.7,
                                ifelse(nfhs1$child.how.meas == "Standing" & nfhs1$child.age.days >= 731,
                                       nfhs1$child.ht.cm - 0.7, nfhs1$child.ht.cm))

nfhs1$zlen <- (((nfhs1$child.ht.cm.adj / nfhs1$m) ^ (nfhs1$l)) - 1) / (nfhs1$s * nfhs1$l)
nfhs1$flag.zlen <- as.numeric(abs(nfhs1$zlen) > 6)

nfhs1 %>%
  dplyr::filter(child.alive == 1) %>%
  dplyr::select(caseid, midx, v001, v002, v003, v005, v024, v025, state.name,
                contains("child."), zlen, flag.zlen) %>%
  dplyr::rename(child.caseid = caseid,
                birth.history.idx = midx,
                psu.id = v001,
                hh.id = v002,
                mom.id = v003,
                mom.sample.wt = v005,
                state = v024,
                rural.urban = v025) %>%
  dplyr::mutate(round = "NFHS1") -> nfhs1

# ##############################################################################
# NFHS2
# ##############################################################################
nfhs2 <- read.dta(paste0(nfhsdir, "/NFHS2/iakr42dt/IAKR42FL.dta"),
                  convert.factors = FALSE)

# Make string version of state name
state.map <- attr(nfhs2, "label.table")$v024
state.map <- data.frame(v024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name[state.map$state.name == "jammu and kashmir"] <- "jammu"
rownames(state.map) <- NULL
nfhs2 <- left_join(nfhs2, state.map, by = "v024")

nfhs2$child.birth.order <- nfhs2$bord
nfhs2$child.multiple.birth <- nfhs2$b0
nfhs2$child.mom.age.birth <- (nfhs2$b3 - nfhs2$v011)/12 # child dob cmc - mom dob cmc
nfhs2$child.age.mos <- nfhs2$v008 - nfhs2$b3 # interview cmc - dob cmc
nfhs2$child.age.days <- rounde(nfhs2$child.age.mos*30.4375)
nfhs2$child.female <- as.numeric(nfhs2$b4 == 2) # 1 = male, 2 = female
nfhs2$child.alive <- nfhs2$b5
nfhs2$child.breastfed.mos <- nfhs2$m4
nfhs2$child.breastfed.mos[nfhs2$m4 == 94] <- 0 # 94 = never bf'ed
inds <- nfhs2$m4 == 95 & !is.na(nfhs2$m4)
nfhs2$child.breastfed.mos[inds] <- nfhs2$child.age.mos[inds] # 95 = still bf'ing
nfhs2$child.breastfed.mos[nfhs2$m4 >= 97] <- NA # inconsistent/don't know/missing
nfhs2$child.inst.birth <- NA
nfhs2$child.inst.birth[nfhs2$m15 < 20] <- 0
nfhs2$child.inst.birth[nfhs2$m15 >= 20 & nfhs2$m15 <= 50] <- 1
nfhs2$child.size.at.birth <- nfhs2$m18
nfhs2$child.size.at.birth[nfhs2$m18 >= 8] <- NA
nfhs2$child.has.health.card <- as.numeric(nfhs2$h1 == 1 | nfhs2$h1 == 2)
nfhs2$child.has.health.card[nfhs2$h1 == 9 | is.na(nfhs2$h1)] <- NA
nfhs2$child.ever.vaccinated <- nfhs2$h10
nfhs2$child.ever.vaccinated[!(nfhs2$h10 %in% c(0, 1))] <- NA
nfhs2$child.wt.kg <- nfhs2$hw2 / 10
nfhs2$child.wt.kg[nfhs2$hw2 == 9999] <- NA
nfhs2$child.ht.cm <- nfhs2$hw3 / 10
nfhs2$child.ht.cm[nfhs2$hw3 == 9999] <- NA
nfhs2$child.how.meas[nfhs2$hw15 == 1] <- "Lying"
nfhs2$child.how.meas[nfhs2$hw15 == 2] <- "Standing"
nfhs2$child.how.meas[nfhs2$hw15 == 9 | is.na(nfhs2$hw15)] <- NA

# Calculate height-for-age z scores using code from WHO's igrowup_standard.R
lenanthro <- read.table("/Users/susanna/projects/sanitation/data/lenanthro.txt",
                        header = TRUE, skip = 0)
lenanthro %>%
  dplyr::rename(b4 = sex, child.age.days = age) -> lenanthro # to match nfhs2 names

nfhs2 <- left_join(nfhs2, lenanthro, by = c("b4", "child.age.days"))

# Adjust length/height measurement depending on whether child was measured
# lying down or standing -- if kids under 24 months (731 days) were measured
# standing up, add 0.7cm to their height. if kids over 24 months were measured
# lying down, subtract 0.7cm to their height. if the mode of measurement is
# missing, leave child length/height unchanged
nfhs2$child.ht.cm.adj <- ifelse(nfhs2$child.how.meas == "Lying" & nfhs2$child.age.days < 731,
                                nfhs2$child.ht.cm + 0.7,
                                ifelse(nfhs2$child.how.meas == "Standing" & nfhs2$child.age.days >= 731,
                                       nfhs2$child.ht.cm - 0.7, nfhs2$child.ht.cm))

nfhs2$zlen <- (((nfhs2$child.ht.cm.adj / nfhs2$m) ^ (nfhs2$l)) - 1) / (nfhs2$s * nfhs2$l)
nfhs2$flag.zlen <- as.numeric(abs(nfhs2$zlen) > 6)

nfhs2 %>%
  dplyr::filter(child.alive == 1) %>%
  dplyr::select(caseid, midx, v001, v002, v003, v005, v024, v025, state.name,
                contains("child."), zlen, flag.zlen) %>%
  dplyr::rename(child.caseid = caseid,
                birth.history.idx = midx,
                psu.id = v001,
                hh.id = v002,
                mom.id = v003,
                mom.sample.wt = v005,
                state = v024,
                rural.urban = v025) %>%
  dplyr::mutate(round = "NFHS2") -> nfhs2

# ##############################################################################
# NFHS3
# ##############################################################################
nfhs3 <- read.dta(paste0(nfhsdir, "/NFHS3/iakr52dt/IAKR52FL.dta"),
                  convert.factors = FALSE)

# Make string version of state name
state.map <- attr(nfhs3, "label.table")$LABD
state.map <- data.frame(v024 = state.map, state.name = attr(state.map, "names"))
state.map$state.name <- as.character(state.map$state.name)
state.map$state.name <- substr(state.map$state.name, 6, nchar(state.map$state.name))
state.map$state.name[state.map$state.name == "jammu and kashmir"] <- "jammu"
state.map$state.name[state.map$state.name == "delhi"] <- "new delhi"
rownames(state.map) <- NULL
nfhs3 <- left_join(nfhs3, state.map, by = "v024")

nfhs3$child.birth.order <- nfhs3$bord
nfhs3$child.multiple.birth <- nfhs3$b0
nfhs3$child.mom.age.birth <- (nfhs3$b3 - nfhs3$v011)/12 # child dob cmc - mom dob cmc
nfhs3$child.age.mos <- nfhs3$v008 - nfhs3$b3 # interview cmc - dob cmc
nfhs3$child.age.days <- rounde(nfhs3$child.age.mos*30.4375)
nfhs3$child.female <- as.numeric(nfhs3$b4 == 2) # 1 = male, 2 = female
nfhs3$child.alive <- nfhs3$b5
nfhs3$child.breastfed.mos <- nfhs3$m4
nfhs3$child.breastfed.mos[nfhs3$m4 == 94] <- 0 # 94 = never bf'ed
inds <- nfhs3$m4 == 95 & !is.na(nfhs3$m4)
nfhs3$child.breastfed.mos[inds] <- nfhs3$child.age.mos[inds] # 95 = still bf'ing
nfhs3$child.breastfed.mos[nfhs3$m4 >= 97] <- NA # inconsistent/don't know/missing
nfhs3$child.inst.birth <- NA
nfhs3$child.inst.birth[nfhs3$m15 < 20] <- 0
nfhs3$child.inst.birth[nfhs3$m15 >= 20 & nfhs3$m15 <= 50] <- 1
nfhs3$child.size.at.birth <- nfhs3$m18
nfhs3$child.size.at.birth[nfhs3$m18 >= 8] <- NA
nfhs3$child.has.health.card <- as.numeric(nfhs3$h1 == 1 | nfhs3$h1 == 2)
nfhs3$child.has.health.card[nfhs3$h1 == 9 | is.na(nfhs3$h1)] <- NA
nfhs3$child.ever.vaccinated <- nfhs3$h10
nfhs3$child.ever.vaccinated[!(nfhs3$h10 %in% c(0, 1))] <- NA
nfhs3$child.wt.kg <- nfhs3$hw2 / 10
nfhs3$child.wt.kg[nfhs3$hw2 == 9999] <- NA
nfhs3$child.ht.cm <- nfhs3$hw3 / 10
nfhs3$child.ht.cm[nfhs3$hw3 == 9999] <- NA
nfhs3$child.how.meas[nfhs3$hw15 == 1] <- "Lying"
nfhs3$child.how.meas[nfhs3$hw15 == 2] <- "Standing"
nfhs3$child.how.meas[nfhs3$hw15 == 9 | is.na(nfhs3$hw15)] <- NA

# Calculate height-for-age z scores using code from WHO's igrowup_standard.R
lenanthro <- read.table("/Users/susanna/projects/sanitation/data/lenanthro.txt",
                        header = TRUE, skip = 0)
lenanthro %>%
  dplyr::rename(b4 = sex, child.age.days = age) -> lenanthro # to match nfhs3 names

nfhs3 <- left_join(nfhs3, lenanthro, by = c("b4", "child.age.days"))

# Adjust length/height measurement depending on whether child was measured
# lying down or standing -- if kids under 24 months (731 days) were measured
# standing up, add 0.7cm to their height. if kids over 24 months were measured
# lying down, subtract 0.7cm to their height. if the mode of measurement is
# missing, leave child length/height unchanged
nfhs3$child.ht.cm.adj <- ifelse(nfhs3$child.how.meas == "Lying" & nfhs3$child.age.days < 731,
                                nfhs3$child.ht.cm + 0.7,
                                ifelse(nfhs3$child.how.meas == "Standing" & nfhs3$child.age.days >= 731,
                                       nfhs3$child.ht.cm - 0.7, nfhs3$child.ht.cm))

nfhs3$zlen <- (((nfhs3$child.ht.cm.adj / nfhs3$m) ^ (nfhs3$l)) - 1) / (nfhs3$s * nfhs3$l)
nfhs3$flag.zlen <- as.numeric(abs(nfhs3$zlen) > 6)

nfhs3 %>%
  dplyr::filter(child.alive == 1) %>%
  dplyr::select(caseid, midx, v001, v002, v003, v005, v024, v025, state.name,
                contains("child."), zlen, flag.zlen) %>%
  dplyr::rename(child.caseid = caseid,
                birth.history.idx = midx,
                psu.id = v001,
                hh.id = v002,
                mom.id = v003,
                mom.sample.wt = v005,
                state = v024,
                rural.urban = v025) %>%
  dplyr::mutate(round = "NFHS3") -> nfhs3

# ##############################################################################
# Combine all rounds
# ##############################################################################
nfhs <- rbind(nfhs1, nfhs2, nfhs3)
nfhs$round <- rep(c("NFHS1", "NFHS2", "NFHS3"),
                  times = c(nrow(nfhs1), nrow(nfhs2), nrow(nfhs3)))
saveRDS(nfhs, "/Users/susanna/projects/sanitation/data/nfhs/child_data.rds")

# Calculate PSU-level characteristics -- prop children born institutionally,
# prop children w/ any vaccinations, prop children w/ health card
child_psu_data <- nfhs %>%
  dplyr::group_by(round, state.name, psu.id) %>%
  dplyr::summarise(prop_child_instbirth = mean(child.inst.birth, na.rm = TRUE),
                   prop_child_anyvacc = mean(child.ever.vaccinated, na.rm = TRUE),
                   prop_child_hcard = mean(child.has.health.card, na.rm = TRUE),
                   ss_child_instbirth = sum(!is.na(child.inst.birth)),
                   ss_child_anyvacc = sum(!is.na(child.ever.vaccinated)),
                   ss_child_hcard = sum(!is.na(child.has.health.card)))

saveRDS(child_psu_data,
        "/Users/susanna/projects/sanitation/data/nfhs/child_psu_data.rds")
