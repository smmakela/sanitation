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


nfhs1$child.age.mos <- nfhs1$v008 - nfhs1$b3 # interview cmc - dob cmc
nfhs1$child.age.days <- rounde(nfhs1$child.age.mos*30.4375)
nfhs1$child.female <- as.numeric(nfhs1$b4 == 2) # 1 = male, 2 = female
nfhs1$child.alive <- nfhs1$b5
nfhs1$child.breastfeed.duration <- nfhs1$m6 # mark 94+ as NA
nfhs1$child.size.at.birth <- nfhs1$m18
nfhs1$child.size.at.birth[nfhs1$m18 >= 8] <- NA
nfhs1$child.has.health.card <- as.numeric(nfhs1$h1 == 1 | nfhs1$h1 == 2)
nfhs1$child.has.health.card[nfhs1$h1 == 9 | is.na(nfhs1$h1)] <- NA
nfhs1$child.ever.vaccinated <- nfhs1$h10
nfhs1$child.ever.vaccinated[!(nfhs1$h10 %in% c(0, 1))] <- NA
nfhs1$child.wt.kg <- nfhs1$hw2 / 10
nfhs1$child.ht.cm <- nfhs1$hw3 / 10
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
  dplyr::select(caseid, midx, v001, v002, v003)
