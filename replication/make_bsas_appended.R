setwd("D:/User/Files/Academics/Spring 2021/GOV 52/Replication Project A/replication")

library(dplyr)
library(haven)
library(DescTools) # Quantile() function
library(labelled) # to remove attributes from tibble returned by read_dta()

omitted_data_years <- c(1988, 1992, 1996)
data_years <- setdiff(1983:2018, omitted_data_years)

for (i in 1:length(data_years)) {
 one_year <- data_years[i]
 one_dataset <- read_dta(file=paste0("bsas/bsas",
                                one_year,
                                ".dta")) %>%
                as.data.frame() %>%
                remove_attributes(attributes=c("label", "labels", "format.stata"))
 
 
 one_dataset$year <- one_year
 names(one_dataset) <- tolower(names(one_dataset))
 
 
 # creating income variable
 if (one_year < 2010) {
   income_var <- "hhincome"
 } else if (one_year > 2009) {
   income_var <- "hhincd"
 }
 one_dataset[one_dataset[, income_var] %in% c(-1, 97, 98, 99),
             income_var] <- NA ## possible codings of missing HH income variable
 income <- as.numeric(unlist(one_dataset[, income_var]))
  
 # weighted income terciles
 weighted_income_terciles <- Quantile(x=income[!is.na(income)],
                                      weights=one_dataset$wtfactor[!is.na(income)],
                                      probs=c(0, 1/3, 2/3))
 income_tercile <- rep(NA, length(income))
 income_tercile[income >= weighted_income_terciles[1] & 
                income < weighted_income_terciles[2]] <- 1 ## corresponds to "low" in original fig 5 code
 income_tercile[income >= weighted_income_terciles[2] & 
                income < weighted_income_terciles[3]] <- 2 ## corresponds to "middle"
 income_tercile[income >= weighted_income_terciles[3]] <- 3  ## corresponds to "high"
 income_tercile[is.na(income)] <- NA
 stopifnot(sum(is.na(income_tercile[!is.na(income)])) == 0)
 one_dataset$income_tercile <- income_tercile

 # weighted income quintiles
 weighted_income_quintiles <- Quantile(x=income[!is.na(income)],
                                      weights=one_dataset$wtfactor[!is.na(income)],
                                      probs=c(0, 1/5, 2/5, 3/5, 4/5))
 income_quintile <- rep(NA, length(income))
 income_quintile[income >= weighted_income_quintiles[1] & 
                 income < weighted_income_quintiles[2]] <- 1 ## low
 income_quintile[income >= weighted_income_quintiles[2] & 
                 income < weighted_income_quintiles[3]] <- 2 
 income_quintile[income >= weighted_income_quintiles[3] & 
                 income < weighted_income_quintiles[4]] <- 3 
 income_quintile[income >= weighted_income_quintiles[4] & 
                 income < weighted_income_quintiles[5]] <- 4 
 income_quintile[income >= weighted_income_quintiles[5]] <- 5  ## "high"
 income_quintile[is.na(income)] <- NA
 stopifnot(sum(is.na(income_quintile[!is.na(income)])) == 0)
 one_dataset$income_quintile <- income_quintile
 
 # weighted indicator of being above or below median
 weighted_income_median <- Quantile(x=income[!is.na(income)],
                                       weights=one_dataset$wtfactor[!is.na(income)],
                                       probs=0.5)
 income_median <- rep(NA, length(income))
 income_median[income >= weighted_income_median] <- 1
 income_median[income < weighted_income_median] <- 0
 income_median[is.na(income)] <- NA
 stopifnot(sum(is.na(income_median[!is.na(income)])) == 0)
 one_dataset$income_median <- income_median
 
 
 # store variable names we want to keep to prevent errors when stacking different column names with rbind() 
 ## SES group / class variable
 if (one_year < 2001) {
   one_dataset$orig_seg_group <-  one_dataset[, "rseggrp"]
 } else if (one_year >= 2001) {
   one_dataset$orig_seg_group <-  one_dataset[, "rnseggrp"]
 }
 ## homeowner variable
 one_dataset$owner <- NA
 if (one_year %in% c(1983, 1984)) {
   one_dataset$owner[one_dataset$tenure1 %in% c(1)] <- 1
   one_dataset$owner[one_dataset$tenure1 %in% c(2, 3, 4, 5, 6, 7, 8, 9)] <- 0
 } else if (one_year > 1984 & one_year < 2010 & one_year != 2004) {
   one_dataset$owner[one_dataset$tenure1 %in% c(1, 2)] <- 1
   one_dataset$owner[one_dataset$tenure1 %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 97)] <- 0
 } else if (one_year == 2004) {
   one_dataset$owner[one_dataset$tenure6 %in% c(1, 2, 13)] <- 1
   one_dataset$owner[one_dataset$tenure6 %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 97)] <- 0
 } else if (one_year > 2009 & one_year < 2017) {
   one_dataset$owner[one_dataset$tenure7 %in% c(1, 2, 3)] <- 1
   one_dataset$owner[one_dataset$tenure7 %in% c(4, 5, 6, 7, 8, 9, 10, 11, 97)] <- 0
 } else if (one_year > 2016 & !is.na(one_year) ) {
   one_dataset$owner[one_dataset$tenure2e %in% c(1)] <- 1
   one_dataset$owner[one_dataset$tenure2e %in% c(2, 3, 4)] <- 0
 }
 ## london variable
 one_dataset$london <- NA
 if (one_year < 2006) {
   one_dataset$london[one_dataset$stregion %in% c(10)] <- 1
   one_dataset$london[one_dataset$stregion %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12)] <- 0
 } else if (one_year > 2005 & one_year < 2015) {
   one_dataset$london[one_dataset$gor2 %in% c(8, 9)] <- 1
   one_dataset$london[one_dataset$gor2 %in% c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12)] <- 0
 } else if (one_year == 2015) {
   one_dataset$london[one_dataset$revised_gor2 %in% c(8, 9)] <- 1
   one_dataset$london[one_dataset$revised_gor2 %in% c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12)] <- 0
 } else if (one_year > 2015 & !is.na(one_year)) {
   one_dataset$london[one_dataset$gor_id %in% c(7)] <- 1
   one_dataset$london[one_dataset$gor_id %in% c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11)] <- 0
 } 
 ## labour variable
 one_dataset$labour <- NA
 one_dataset$labour[one_dataset$partyid1 %in% c(2)] <- 1
 one_dataset$labour[one_dataset$partyid1 %in% c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 20, 21, 22, 23, 24, 25, 26, 27, 95, 98)] <- 0
 
 ## conservative variable
 one_dataset$conservative <- NA
 one_dataset$conservative[one_dataset$partyid1 %in% c(1)] <- 1
 one_dataset$conservative[one_dataset$partyid1 %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 20, 21, 22, 23, 24, 25, 26, 27, 95, 98)] <- 0
 
 ## decades variable
 one_dataset$decades <- NA
 if (one_year < 1990) {
   one_dataset$decades <- "1980s"
 } else if (one_year > 1989 & one_year < 2000) {
   one_dataset$decades <- "1990s"
 } else if (one_year > 1999 & one_year < 2010) {
   one_dataset$decades <- "2000s"
 } else if (one_year > 2009 & !is.na(one_year)) {
   one_dataset$decades <- "2010s"
 }
 
 ## city variables
 ### location (1993 moved to respres variable from Stata code)
 if (one_year %in% c(1983, 1984)) {
   one_dataset$location <- one_dataset$areatype
 } else if (one_year %in% c(1985, 1986, 1987, 1990, 1991, 1992)) {
   one_dataset$location <- one_dataset$arealive
 } else if (one_year %in% c(1993, 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) {
   one_dataset$location <- one_dataset$respres
 } else if ((one_year %in% c(1989, 1994)) | is.na(one_year)) {
   one_dataset$location <- NA
 }
 ### big city
 one_dataset$big_city <- NA
 one_dataset$big_city[one_dataset$location %in% c(1)] <- 1 
 one_dataset$big_city[one_dataset$location %in% c(2, 3, 4, 5)] <- 0
 
 ### big or small city
 one_dataset$big_or_small_city <- NA
 one_dataset$big_or_small_city[one_dataset$location %in% c(1, 3)] <- 1 
 one_dataset$big_or_small_city[one_dataset$location %in% c(2, 4, 5)] <- 0
 
 
 variables_to_keep <- c("year", "wtfactor", 
                        "income_tercile", "income_quintile", "income_median",
                        "orig_seg_group",
                        "owner",
                        "london",
                        "labour", "conservative", 
                        "decades",
                        "location", "big_city", "big_or_small_city")
 if (i == 1) {
   appended_dataset <- one_dataset[, variables_to_keep]
 } else {
   appended_dataset <- rbind(appended_dataset,
                             one_dataset[, variables_to_keep])
 }
 print(one_year)
}

# create additional SEG variables in appended dataset
## employers and managers
appended_dataset$profempmanag <- NA
appended_dataset$profempmanag[appended_dataset$orig_seg_group %in% c(1, 2) &
                              appended_dataset$year < 2001] <- 1
appended_dataset$profempmanag[appended_dataset$orig_seg_group %in% c(3, 4, 5, 6, 7, 8) &
                                appended_dataset$year < 2001] <- 0

appended_dataset$profempmanag[appended_dataset$orig_seg_group %in% c(1) &
                                appended_dataset$year > 2000] <- 1
appended_dataset$profempmanag[appended_dataset$orig_seg_group %in% c(2, 3, 4, 5, 6, 7) &
                              appended_dataset$year > 2000 &
                              !is.na(appended_dataset$year)] <- 0

## intermediate non-manual
appended_dataset$intnonman <- NA
appended_dataset$intnonman[appended_dataset$orig_seg_group %in% c(3) &
                                appended_dataset$year < 2001] <- 1
appended_dataset$intnonman[appended_dataset$orig_seg_group %in% c(1, 2, 4, 5, 6, 7, 8) &
                                appended_dataset$year < 2001] <- 0

appended_dataset$intnonman[appended_dataset$orig_seg_group %in% c(2) &
                                appended_dataset$year > 2000] <- 1
appended_dataset$intnonman[appended_dataset$orig_seg_group %in% c(1, 3, 4, 5, 6, 7) &
                                appended_dataset$year > 2000 &
                                !is.na(appended_dataset$year)] <- 0

## junior non-manual
appended_dataset$jrnonman <- NA
appended_dataset$jrnonman[appended_dataset$orig_seg_group %in% c(4) &
                             appended_dataset$year < 2001] <- 1
appended_dataset$jrnonman[appended_dataset$orig_seg_group %in% c(1, 2, 3, 5, 6, 7, 8) &
                             appended_dataset$year < 2001] <- 0

appended_dataset$jrnonman[appended_dataset$orig_seg_group %in% c(3) &
                             appended_dataset$year > 2000] <- 1
appended_dataset$jrnonman[appended_dataset$orig_seg_group %in% c(1, 2, 4, 5, 6, 7) &
                             appended_dataset$year > 2000 &
                             !is.na(appended_dataset$year)] <- 0

## skilled manual
appended_dataset$sklman <- NA
appended_dataset$sklman[appended_dataset$orig_seg_group %in% c(5) &
                            appended_dataset$year < 2001] <- 1
appended_dataset$sklman[appended_dataset$orig_seg_group %in% c(1, 2, 3, 4, 6, 7, 8) &
                            appended_dataset$year < 2001] <- 0

appended_dataset$sklman[appended_dataset$orig_seg_group %in% c(4) &
                            appended_dataset$year > 2000] <- 1
appended_dataset$sklman[appended_dataset$orig_seg_group %in% c(1, 2, 3, 5, 6, 7) &
                            appended_dataset$year > 2000 &
                            !is.na(appended_dataset$year)] <- 0

## semi-skilled
### note: compatibilty issue: starting in 2001 this includes personal services, i.e. 
###       not  just manual
appended_dataset$semiskl <- NA
appended_dataset$semiskl[appended_dataset$orig_seg_group %in% c(6) &
                          appended_dataset$year < 2001] <- 1
appended_dataset$semiskl[appended_dataset$orig_seg_group %in% c(1, 2, 3, 4, 5, 7, 8) &
                          appended_dataset$year < 2001] <- 0

appended_dataset$semiskl[appended_dataset$orig_seg_group %in% c(5) &
                          appended_dataset$year > 2000] <- 1
appended_dataset$semiskl[appended_dataset$orig_seg_group %in% c(1, 2, 3, 4, 6, 7) &
                          appended_dataset$year > 2000 &
                          !is.na(appended_dataset$year)] <- 0
  


## semi-skilled
appended_dataset$unsklman <- NA
appended_dataset$unsklman[appended_dataset$orig_seg_group %in% c(7) &
                           appended_dataset$year < 2001] <- 1
appended_dataset$unsklman[appended_dataset$orig_seg_group %in% c(1, 2, 3, 4, 5, 6, 8) &
                           appended_dataset$year < 2001] <- 0

appended_dataset$unsklman[appended_dataset$orig_seg_group %in% c(6) &
                           appended_dataset$year > 2000] <- 1
appended_dataset$unsklman[appended_dataset$orig_seg_group %in% c(1, 2, 3, 4, 5, 7) &
                           appended_dataset$year > 2000 &
                           !is.na(appended_dataset$year)] <- 0

##aggregated
appended_dataset$ses_grp <- NA
appended_dataset$ses_grp[appended_dataset$profempmanag == 1] <- "professional, employer, manager"
appended_dataset$ses_grp[appended_dataset$intnonman == 1] <- "intermediate non-manual"
appended_dataset$ses_grp[appended_dataset$jrnonman == 1] <- "junior non-manual"
appended_dataset$ses_grp[appended_dataset$sklman == 1] <- "skilled manual"
appended_dataset$ses_grp[appended_dataset$semiskl == 1] <- "semi-skilled"
appended_dataset$ses_grp[appended_dataset$unsklman == 1] <- "unskilled manual"

appended_dataset$ses_grp <- factor(appended_dataset$ses_grp,
                                   levels=c("professional, employer, manager",
                                            "intermediate non-manual",
                                            "junior non-manual",
                                            "skilled manual",
                                            "semi-skilled",
                                            "unskilled manual"))

# format decades variable as factor
appended_dataset$decades <- factor(appended_dataset$decades,
                                   levels=c("1980s", 
                                            "1990s",
                                            "2000s",
                                            "2010s"))

appended_dataset <- appended_dataset %>%
                    select(year, wtfactor, 
                           decades,
                           income_tercile, income_quintile, income_median,
                           profempmanag, intnonman, jrnonman, sklman, semiskl, unsklman, 
                           ses_grp, 
                           owner, 
                           london, 
                           labour, conservative, 
                           location, big_city, big_or_small_city) %>%
                    as.data.frame()

write.csv(appended_dataset, "bsas/bsas_appended.csv", row.names=FALSE)
