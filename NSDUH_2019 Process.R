# NOTE: This code is provided solely to create example datasets used in Introduction to Regression Methods for Public Health using R
#       No warranty is provided regarding the correctness of this code or regarding any analyses carried out with the
#       datasets produced by this code.

#---
## Program Name:    NSDUH_2019 Process.R
## Analyst:         Ramzi W. Nahhas
## Date:            October 3, 2021
## Contents:        Process NSDUH 2019 and create subsample for teaching
#---

#---
# Load data ####
#---

# install.packages("tidyverse", "Hmisc")
library(tidyverse)

# Download main page
# https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2019-nsduh-2019-ds0001
# R dataset
# https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/NSDUH-2019/NSDUH-2019-datasets/NSDUH-2019-DS0001/NSDUH-2019-DS0001-bundles-with-study-info/NSDUH-2019-DS0001-bndl-data-r.zip
# Codebook
# https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/NSDUH-2019/NSDUH-2019-datasets/NSDUH-2019-DS0001/NSDUH-2019-DS0001-info/NSDUH-2019-DS0001-info-codebook.pdf

load("NSDUH_2019.RData")

dim(PUF2019_100920) # 56136  2741

#---
# Examine design variables ####
#---
# STRATA vestr;
# CLUSTER verep;
# WEIGHT ANALWT_C

table(PUF2019_100920$vestr)
length(unique(PUF2019_100920$vestr)) # 50 strata
table(PUF2019_100920$verep)
length(unique(PUF2019_100920$verep)) # 2 clusters per strata
summary(PUF2019_100920$ANALWT_C)

#---
# Examine variables prior to processing ####
#---

table(PUF2019_100920$cigflag, exclude = NULL)
summary(PUF2019_100920$ircigage)
table(PUF2019_100920$cigflag[PUF2019_100920$ircigage == 991], exclude = NULL)
summary(PUF2019_100920$ircigage[PUF2019_100920$cigflag == 0])
# ircigage = 991 (Never used) corresponds exactly to cigflag = 0 (No)
table(PUF2019_100920$ircigrc, exclude = NULL)
summary(PUF2019_100920$ircigfm)
table(PUF2019_100920$CIG100LF, exclude = NULL)

table(PUF2019_100920$alcflag, exclude = NULL)
summary(PUF2019_100920$iralcage)
table(PUF2019_100920$alcflag[PUF2019_100920$iralcage == 991], exclude = NULL)
summary(PUF2019_100920$iralcage[PUF2019_100920$alcflag == 0])
# iralcage = 991 (Never used) corresponds exactly to alcflag = 0 (No)
table(PUF2019_100920$iralcrc, exclude = NULL)
summary(PUF2019_100920$iralcfm)
table(PUF2019_100920$IRALCBNG30D, exclude = NULL)
summary(PUF2019_100920$iralcfy)

table(PUF2019_100920$mrjflag, exclude = NULL)
summary(PUF2019_100920$irmjage)
table(PUF2019_100920$mrjflag[PUF2019_100920$irmjage == 991], exclude = NULL)
summary(PUF2019_100920$irmjage[PUF2019_100920$mrjflag == 0])
# irmjage = 991 (Never used) corresponds exactly to mrjflag = 0 (No)
table(PUF2019_100920$irmjrc, exclude = NULL)
summary(PUF2019_100920$irmjfy)
summary(PUF2019_100920$irmjfm)

table(PUF2019_100920$herflag, exclude = NULL)
summary(PUF2019_100920$irherage)
table(PUF2019_100920$herflag[PUF2019_100920$irherage == 991], exclude = NULL)
summary(PUF2019_100920$irherage[PUF2019_100920$herflag == 0])
# irherage = 991 (Never used) corresponds exactly to herflag = 0 (No)
table(PUF2019_100920$irherrc, exclude = NULL)
summary(PUF2019_100920$irherfy)
summary(PUF2019_100920$irherfm)

table(PUF2019_100920$methamflag, exclude = NULL)
summary(PUF2019_100920$irmethamage)
table(PUF2019_100920$methamflag[PUF2019_100920$irmethamage == 991], exclude = NULL)
summary(PUF2019_100920$irmethamage[PUF2019_100920$methamflag == 0])
# irmethamage = 991 (Never used) corresponds exactly to methamflag = 0 (No)
table(PUF2019_100920$irmethamrec, exclude = NULL)
summary(PUF2019_100920$irmethamyfq)
summary(PUF2019_100920$IRMETHAM30N)

table(PUF2019_100920$pnrnmflag, exclude = NULL)
table(PUF2019_100920$irpnrnmrec, exclude = NULL)
summary(PUF2019_100920$IRPNRNM30FQ, exclude = NULL)
table(PUF2019_100920$irsex, exclude = NULL)
table(PUF2019_100920$CATAG6, exclude = NULL)
table(PUF2019_100920$NEWRACE2, exclude = NULL)

#---
# Process data ####
#---

# Convert integers to double (otherwise have to use 0L instead of 0, for example, when setting a value)
INTEGER <- names(PUF2019_100920)[unlist(lapply(PUF2019_100920, class)) == "integer"]
for(i in 1:length(INTEGER)) {
  PUF2019_100920[[INTEGER[i]]] <- as.double(PUF2019_100920[[INTEGER[i]]])
}
names(PUF2019_100920)[unlist(lapply(PUF2019_100920, class)) == "integer"]

nsduh <- PUF2019_100920 %>% 
  mutate(cig_lifetime     = factor(cigflag, # RC-CIGARETTES - EVER USED
                                   levels = 0:1,
                                   labels = c("No", "Yes")),
         cig_agefirst     = na_if(ircigage, 991), # CIGARETTE AGE OF FIRST USE - IMPUTATION REVISED
         cig_last         = factor(ircigrc,       # CIGARETTE RECENCY - IMPUTATION REVISED
                                   levels = 1:4,
                                   labels = c("t <= 30d",
                                              "30d < t <= 1y",
                                              "1y < t <= 3y",
                                              "t > 3y")),
         cig_past30d      = case_when(ircigfm == 91 ~ 0, # CIG FREQUENCY PAST MONTH - IMPUTATION REVISED
                                      ircigfm == 93 ~ 0,
                                      TRUE          ~ ircigfm),
         cig_100_lifetime = case_when(CIG100LF %in% c(2, 91)   ~ 1,  # HAVE YOU SMOKED 100 CIGS IN YOUR LIFE
                                      CIG100LF %in% c(1, 3, 5) ~ 2),
         cig_100_lifetime = factor(cig_100_lifetime,
                                   levels = 1:2,
                                   labels = c("No", "Yes")),
         
         alc_lifetime      = factor(alcflag, # RC-ALCOHOL - EVER USED
                                    levels = 0:1,
                                    labels = c("No", "Yes")),
         alc_agefirst      = na_if(iralcage, 991), # ALCOHOL AGE OF FIRST USE - IMPUTATION REVISED
         alc_last          = factor(iralcrc, # ALCOHOL RECENCY - IMPUTATION REVISED
                                    levels = 1:3,
                                    labels = c("t <= 30d",
                                               "30d < t <= 1y",
                                               "t > 1y")),
         alc_past30d       = case_when(iralcfm == 91 ~ 0, # ALCOHOL FREQUENCY PAST MONTH - IMPUTATION REVISED
                                       iralcfm == 93 ~ 0,
                                       TRUE          ~ iralcfm),
         alc_binge_past30d = case_when(IRALCBNG30D == 91 ~ 0, # BINGE ALCOHOL FREQUENCY PAST MONTH - IMPUTATION REVISED
                                       IRALCBNG30D == 93 ~ 0,
                                       TRUE              ~ IRALCBNG30D),
         alc_past1y        = case_when(iralcfy == 991 ~ 0, # ALCOHOL FREQUENCY PAST YEAR - IMPUTATION REVISED
                                       iralcfy == 993 ~ 0,
                                       TRUE           ~ iralcfy),

         mj_lifetime = factor(mrjflag, # RC-MARIJUANA - EVER USED
                              levels = 0:1,
                              labels = c("No", "Yes")),
         mj_agefirst = na_if(irmjage, 991), # MARIJUANA AGE OF FIRST USE - IMPUTATION REVISED
         mj_last     = factor(irmjrc, # MARIJUANA RECENCY - IMPUTATION REVISED
                              levels = 1:3,
                              labels = c("t <= 30d",
                                         "30d < t <= 1y",
                                         "t > 1y")),
         mj_past30d  = case_when(irmjfm == 91 ~ 0, # MARIJUANA FREQUENCY PAST MONTH - IMPUTATION REVISED
                                 irmjfm == 93 ~ 0,
                                 TRUE          ~ irmjfm),
         mj_past1y   = case_when(irmjfy == 991 ~ 0, # MARIJUANA FREQUENCY PAST YEAR - IMPUTATION REVISED
                                 irmjfy == 993 ~ 0,
                                 TRUE          ~ irmjfy),
         
         her_lifetime = factor(herflag, # RC-HEROIN - EVER USED
                               levels = 0:1,
                               labels = c("No", "Yes")),
         her_agefirst = na_if(irherage, 991), # HEROIN AGE OF FIRST USE - IMPUTATION REVISED
         her_last     = factor(irherrc, # HEROIN RECENCY - IMPUTATION REVISED
                               levels = 1:3,
                               labels = c("t <= 30d",
                                          "30d < t <= 1y",
                                          "t > 1y")),
         her_past30d  = case_when(irherfm == 91 ~ 0, # HEROIN FREQUENCY PAST MONTH - IMPUTATION REVISED
                                  irherfm == 93 ~ 0,
                                  TRUE          ~ irherfm),
         her_past1y        = case_when(irherfy == 991 ~ 0, # HEROIN FREQUENCY PAST YEAR - IMPUTATION REVISED
                                       irherfy == 993 ~ 0,
                                       TRUE           ~ irherfy),
         
         metham_lifetime = factor(methamflag, # RC-METHAMPHETAMINE - EVER USED
                                  levels = 0:1,
                                  labels = c("No", "Yes")),
         metham_agefirst = na_if(irmethamage, 991), # METHAMPHETAMINE AGE OF FIRST USE - IMPUTATION REVISED
         metham_last     = factor(irmethamrec, # METHAMPHETAMINE RECENCY - IMPUTATION REVISED
                                  levels = 1:3,
                                  labels = c("t <= 30d",
                                             "30d < t <= 1y",
                                             "t > 1y")),
         metham_past30d  = case_when(IRMETHAM30N == 91 ~ 0, # METHAMPHETAMINE FREQUENCY PAST MONTH - IMPUTATION REVISED
                                     IRMETHAM30N == 93 ~ 0,
                                     TRUE              ~ IRMETHAM30N),
         metham_past1y   = case_when(irmethamyfq == 991 ~ 0, # METH FREQUENCY PAST YEAR - IMPUTATION REVISED
                                     irmethamyfq == 993 ~ 0,
                                     TRUE               ~ irmethamyfq),

         painpill_lifetime = factor(pnrnmflag, # RC-PAIN RELIEVERS - EVER MISUSED
                                  levels = 0:1,
                                  labels = c("No", "Yes")),
         # painmedmisuse_agefirst = SKIP (irpnrnmage is only for past year initiates)
         painpill_last     = factor(irpnrnmrec, # PAIN RELIEVER MISUSE RECENCY - IMPUTATION REVISED
                                 levels = 1:3,
                                 labels = c("t <= 30d",
                                            "30d < t <= 1y",
                                            "t > 1y")),
         # painmedmisuse_past30d  = SKIP (IRPNRNM30FQ is only for past year initiates)
         
         # Opioid = heroin or misuse of pain pills
         her_painpill_past30d_01  = factor(opinmmon, # RC-OPIOIDS - PAST MONTH MISUSE
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         her_painpill_past1y_01  = factor(opinmyr, # RC-OPIOIDS - PAST YEAR MISUSE
                                       levels = 0:1,
                                       labels = c("No", "Yes")),
         her_painpill_past1y_cat = factor(herpnryr, # RC-HEROIN USE AND/OR PAIN RELIEVER MISUSE - PAST YEAR
                                       levels = c(4,1,2,3),
                                       labels = c("Neither",
                                                  "Heroin only",
                                                  "Pain reliever misuse only",
                                                  "Both")),
         
         illicit_lifetime   = factor(illflag, # RC-ANY ILLICIT DRUG - EVER USED
                                     levels = 0:1,
                                     labels = c("No", "Yes")),
         illicit_past30d_01 = factor(illmon,  # RC-ANY ILLICIT DRUG - PAST MONTH USE
                                     levels = 0:1,
                                     labels = c("No", "Yes")),
         illicit_past1y_01  = factor(illyr,  # RC-ANY ILLICIT DRUG - PAST YEAR USE
                                     levels = 0:1,
                                     labels = c("No", "Yes")),
         
         tx_substance_lifetime = factor(TXEVRRCVD2, # RC-RCVD TRT ANY LOC FOR ILL DRG OR ALC USE IN LIFETIME
                                        levels = 0:1,
                                        labels = c("No", "Yes")),
         tx_adult_mh_past1y = factor(AMHTXRC3, # RC-RCVD ANY MENTAL HEALTH TRT IN PST YR
                                     levels = 2:1, # Reversed
                                     labels = c("No", "Yes")),

         bmi = BMI2, # RC-BODY MASS INDEX (BMI)
         
         psych_adult_distress_past30d = K6SCMON, # RC-K6 TOTAL SCORE IN PAST MONTH
         psych_adult_distress_past1y  = K6SCYR,  # RC-K6 TOTAL SCORE IN WORST MONTH OF PAST YEAR
         psych_adult_suicidal_thoughs_past1y = factor(mhsuithk, # RC-SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR
                                                      levels = 0:1,
                                                      labels = c("No", "Yes")),
         psych_adult_whodas = WHODASC2, # RC-WHODAS TOTAL SCORE
         psych_adult_mdd_lifetime = factor(amdelt, # RC-ADULT: LIFETIME MAJOR DEPRESSIVE EPISODE (MDE)
                                           levels = 2:1, # Reversed
                                           labels = c("No", "Yes")),
         psych_adult_mdd_past1y = factor(amdeyr, # RC-ADULT: PAST YEAR MAJOR DEPRESSIVE EPISODE (MDE)
                                         levels = 2:1, # Reversed
                                         labels = c("No", "Yes")),
         
         demog_sex      = factor(irsex, levels = 1:2, labels = c("Male", "Female")), # IMPUTATION REVISED GENDER
         demog_age_cat  = factor(AGE2, # RECODE - FINAL EDITED AGE
                                 levels = 1:17,
                                 labels = c(as.character(12:21),
                                            "22-23",
                                            "24-25",
                                            "26-29",
                                            "30-34",
                                            "35-49",
                                            "50-64",
                                            "65+")),
         demog_age_cat6 = factor(CATAG6, # RC-AGE CATEGORY RECODE (6 LEVELS)
                                 levels = 1:6,
                                 labels = c("12-17",
                                            "18-25",
                                            "26-34",
                                            "35-49",
                                            "50-64",
                                            "65+")),
         demog_race = factor(NEWRACE2, # RC-RACE/HISPANICITY RECODE (7 LEVELS)
                             levels = 1:7,
                             labels = c("NonHisp White",
                                        "NonHisp Black/Afr Am",
                                        "NonHisp Native Am/AK Native",
                                        "NonHisp Native HI/Other Pac Isl",
                                        "NonHisp Asian",
                                        "NonHisp more than one race",
                                        "Hispanic")),
         demog_health = factor(HEALTH2, # RC-OVERALL HEALTH RECODE
                               levels = 4:1, # Reversed
                               labels = c("Poor/Fair", "Good", "Very Good", "Excellent")),
         demog_marital = factor(irmarit, # IMPUTATION REVISED MARITAL STATUS
                                levels = 1:4,
                                labels = c("Married",
                                           "Widowed",
                                           "Divorced or Separated",
                                           "Never Been Married")),
         demog_educ    = factor(IREDUHIGHST2, # EDUCATION - RECODED IMPUTATION REVISED
                                levels = 1:11,
                                labels = c("<=5th", "6th", "7th", "8th", "9th", "10th", "11th",
                                           "HS/GED", "Some college", "Associate", "College degree")),
         demog_educ_cat4 = factor(eduhighcat, # RC-EDUCATION CATEGORIES
                                   levels = 1:4,
                                   labels = c("< HS/GED", "HS/GED", "Some college/Associate", "College degree")),
         demog_employ = factor(IRWRKSTAT18, # EMPLOYMENT STATUS 18+ - IMPUTATION REVISED
                                     levels = 1:4,
                                     labels = c("Full", "Part", "Unemployed", "Other")),
         demog_income = factor(income, # RC-TOTAL FAMILY INCOME RECODE
                                      levels = 1:4,
                                      labels = c("Less than $20,000",
                                                 "$20,000 - $49,999",
                                                 "$50,000 - $74,999",
                                                 "$75,000 or more")),
         demog_urban = factor(COUTYP4, # COUNTY METRO/NONMETRO STATUS (2013 3-LEVEL)
                              levels = 3:1, # Reversed
                              labels = c("Nonmetro", "Small Metro", "Large Metro"))) %>% 
  
  select(starts_with("demog_"),
         starts_with("cig_"),
         starts_with("alc_"),
         starts_with("mj_"),
         starts_with("her_"),
         starts_with("metham_"),
         starts_with("painpill_"),
         starts_with("illicit_"),
         starts_with("tx_"),
         starts_with("psych_"),
         bmi,
         vestr, verep, ANALWT_C)
         
summary(nsduh)

#---
# Check derivations ####
#---

names(nsduh)

table(PUF2019_100920$irsex,    nsduh$demog_sex, exclude = NULL)
table(PUF2019_100920$AGE2,     nsduh$demog_age_cat,  exclude = NULL)
table(PUF2019_100920$CATAG6,   nsduh$demog_age_cat6, exclude = NULL)
table(nsduh$demog_age_cat, nsduh$demog_age_cat6, exclude = NULL)
table(PUF2019_100920$NEWRACE2, nsduh$demog_race, exclude = NULL)
table(PUF2019_100920$HEALTH2,  nsduh$demog_health, exclude = NULL)
table(PUF2019_100920$irmarit,  nsduh$demog_marital, exclude = NULL)
table(PUF2019_100920$IREDUHIGHST2, nsduh$demog_educ, exclude = NULL)
table(PUF2019_100920$eduhighcat,   nsduh$demog_educ_cat4, exclude = NULL)
table(nsduh$demog_educ, nsduh$demog_educ_cat4, exclude = NULL)
table(PUF2019_100920$IRWRKSTAT18,  nsduh$demog_employ, exclude = NULL)
table(PUF2019_100920$income,       nsduh$demog_income, exclude = NULL)
table(PUF2019_100920$COUTYP4,      nsduh$demog_urban, exclude = NULL)

table(PUF2019_100920$cigflag, nsduh$cig_lifetime, exclude = NULL)
summary(nsduh$cig_agefirst[PUF2019_100920$ircigage == 991])
summary(nsduh$cig_agefirst[PUF2019_100920$ircigage != 991])
table(PUF2019_100920$ircigrc, nsduh$cig_last, exclude = NULL)
summary(nsduh$cig_past30d[PUF2019_100920$ircigfm == 91])
summary(nsduh$cig_past30d[PUF2019_100920$ircigfm == 93])
summary(nsduh$cig_past30d[PUF2019_100920$ircigfm <= 90])
table(PUF2019_100920$CIG100LF, nsduh$cig_100_lifetime, exclude = NULL)
SUB <- nsduh$cig_lifetime == "No"
table(nsduh$cig_agefirst[SUB], exclude = NULL) # Age of first use missing if never used
# These were set to 0 if no lifetime use
table(nsduh$cig_last[SUB], exclude = NULL)
table(nsduh$cig_past30d[SUB], exclude = NULL)
table(nsduh$cig_100_lifetime[SUB], exclude = NULL)

table(PUF2019_100920$alcflag, nsduh$alc_lifetime, exclude = NULL)
summary(nsduh$alc_agefirst[PUF2019_100920$iralcage == 991])
summary(nsduh$alc_agefirst[PUF2019_100920$iralcage != 991])
table(PUF2019_100920$iralcrc, nsduh$alc_last, exclude = NULL)
summary(nsduh$alc_past30d[PUF2019_100920$iralcfm == 91])
summary(nsduh$alc_past30d[PUF2019_100920$iralcfm == 93])
summary(nsduh$alc_past30d[PUF2019_100920$iralcfm <= 90])
summary(nsduh$alc_binge_past30d[PUF2019_100920$IRALCBNG30D == 91])
summary(nsduh$alc_binge_past30d[PUF2019_100920$IRALCBNG30D == 93])
summary(nsduh$alc_binge_past30d[PUF2019_100920$IRALCBNG30D <= 90])
summary(nsduh$alc_past1y[PUF2019_100920$iralcfy == 991])
summary(nsduh$alc_past1y[PUF2019_100920$iralcfy == 993])
summary(nsduh$alc_past1y[PUF2019_100920$iralcfy <= 900])
SUB <- PUF2019_100920$ALCBNG30D %in% c(80, 91, 93)
summary(nsduh$alc_binge_past30d[SUB])
summary(nsduh$alc_binge_past30d[!SUB])
table(nsduh$alc_binge_past30d, PUF2019_100920$bngdrkmon, exclude = NULL)
SUB <- nsduh$alc_lifetime == "No"
table(nsduh$alc_agefirst[SUB], exclude = NULL) # Age of first use missing if never used
# These were set to 0 if no lifetime use
table(nsduh$alc_last[SUB], exclude = NULL)
table(nsduh$alc_past30d[SUB], exclude = NULL)
table(nsduh$alc_past1y[SUB], exclude = NULL)
table(nsduh$alc_binge_past30d[SUB], exclude = NULL)

table(PUF2019_100920$mrjflag, nsduh$mj_lifetime, exclude = NULL)
summary(nsduh$mj_agefirst[PUF2019_100920$irmjage == 991])
summary(nsduh$mj_agefirst[PUF2019_100920$irmjage != 991])
table(PUF2019_100920$irmjrc, nsduh$mj_last, exclude = NULL)
summary(nsduh$mj_past30d[PUF2019_100920$irmjfm == 91])
summary(nsduh$mj_past30d[PUF2019_100920$irmjfm == 93])
summary(nsduh$mj_past30d[PUF2019_100920$irmjfm <= 90])
summary(nsduh$mj_past1y[PUF2019_100920$irmjfy == 991])
summary(nsduh$mj_past1y[PUF2019_100920$irmjfy == 993])
summary(nsduh$mj_past1y[PUF2019_100920$irmjfy <= 900])
SUB <- nsduh$mj_lifetime == "No"
table(nsduh$mj_agefirst[SUB], exclude = NULL) # Age of first use missing if never used
# These were set to 0 if no lifetime use
table(nsduh$mj_last[SUB], exclude = NULL)
table(nsduh$mj_past30d[SUB], exclude = NULL)
table(nsduh$mj_past1y[SUB], exclude = NULL)

table(PUF2019_100920$herflag, nsduh$her_lifetime, exclude = NULL)
summary(nsduh$her_agefirst[PUF2019_100920$irherage == 991])
summary(nsduh$her_agefirst[PUF2019_100920$irherage != 991])
table(PUF2019_100920$irherrc, nsduh$her_last, exclude = NULL)
summary(nsduh$her_past30d[PUF2019_100920$irherfm == 91])
summary(nsduh$her_past30d[PUF2019_100920$irherfm == 93])
summary(nsduh$her_past30d[PUF2019_100920$irherfm <= 90])
summary(nsduh$her_past1y[PUF2019_100920$irherfy == 991])
summary(nsduh$her_past1y[PUF2019_100920$irherfy == 993])
summary(nsduh$her_past1y[PUF2019_100920$irherfy <= 900])
SUB <- nsduh$her_lifetime == "No"
table(nsduh$her_agefirst[SUB], exclude = NULL) # Age of first use missing if never used
# These were set to 0 if no lifetime use
table(nsduh$her_last[SUB], exclude = NULL)
table(nsduh$her_past30d[SUB], exclude = NULL)
table(nsduh$her_past1y[SUB], exclude = NULL)

table(PUF2019_100920$methamflag, nsduh$metham_lifetime, exclude = NULL)
summary(nsduh$metham_agefirst[PUF2019_100920$irmethamage == 991])
summary(nsduh$metham_agefirst[PUF2019_100920$irmethamage != 991])
table(PUF2019_100920$irmethamrec, nsduh$metham_last, exclude = NULL)
summary(nsduh$metham_past30d[PUF2019_100920$IRMETHAM30N == 91])
summary(nsduh$metham_past30d[PUF2019_100920$IRMETHAM30N == 93])
summary(nsduh$metham_past30d[PUF2019_100920$IRMETHAM30N <= 90])
summary(nsduh$metham_past1y[PUF2019_100920$irmethamyfq == 991])
summary(nsduh$metham_past1y[PUF2019_100920$irmethamyfq == 993])
summary(nsduh$metham_past1y[PUF2019_100920$irmethamyfq <= 900])
SUB <- nsduh$metham_lifetime == "No"
table(nsduh$metham_agefirst[SUB], exclude = NULL) # Age of first use missing if never used
# These were set to 0 if no lifetime use
table(nsduh$metham_last[SUB], exclude = NULL)
table(nsduh$metham_past30d[SUB], exclude = NULL)
table(nsduh$metham_past1y[SUB], exclude = NULL)

table(PUF2019_100920$pnrnmflag, nsduh$painpill_lifetime, exclude = NULL)
table(PUF2019_100920$irpnrnmrec, nsduh$painpill_last, exclude = NULL)
SUB <- nsduh$painpill_lifetime == "No"
# These were set to 0 if no lifetime use
table(nsduh$painpill_last[SUB], exclude = NULL)

table(PUF2019_100920$opinmmon, nsduh$her_painpill_past30d_01, exclude = NULL)
table(PUF2019_100920$opinmyr,  nsduh$her_painpill_past1y_01, exclude = NULL)
table(PUF2019_100920$herpnryr, nsduh$her_painpill_past1y_cat, exclude = NULL)
SUB <- nsduh$her_lifetime == "No" & nsduh$painpill_lifetime == "No"
table(nsduh$her_painpill_past30d_01[SUB], exclude = NULL)
table(nsduh$her_painpill_past1y_01[SUB], exclude = NULL)
table(nsduh$her_painpill_past1y_cat[SUB], exclude = NULL)

table(PUF2019_100920$illflag, nsduh$illicit_lifetime, exclude = NULL)
table(PUF2019_100920$illmon,  nsduh$illicit_past30d_01, exclude = NULL)
table(PUF2019_100920$illyr,   nsduh$illicit_past1y_01, exclude = NULL)
SUB <- nsduh$illicit_lifetime == "No"
table(nsduh$illicit_past30d_01[SUB], exclude = NULL)
table(nsduh$illicit_past1y_01[SUB], exclude = NULL)

table(PUF2019_100920$TXEVRRCVD2, nsduh$tx_substance_lifetime, exclude = NULL)
summary(PUF2019_100920$BMI2)
summary(nsduh$bmi)
summary(PUF2019_100920$BMI2 - nsduh$bmi)

summary(PUF2019_100920$K6SCMON)
summary(nsduh$psych_adult_distress_past30d)
summary(PUF2019_100920$K6SCMON - nsduh$psych_adult_distress_past30d)
summary(PUF2019_100920$K6SCYR)
summary(nsduh$psych_adult_distress_past1y)
summary(PUF2019_100920$K6SCYR - nsduh$psych_adult_distress_past1y)
table(PUF2019_100920$mhsuithk, nsduh$psych_adult_suicidal_thoughs_past1y, exclude = NULL)
summary(PUF2019_100920$WHODASC2)
summary(nsduh$psych_adult_whodas)
summary(PUF2019_100920$WHODASC2 - nsduh$psych_adult_whodas)
table(PUF2019_100920$amdelt, nsduh$psych_adult_mdd_lifetime, exclude = NULL)
table(PUF2019_100920$amdeyr, nsduh$psych_adult_mdd_past1y, exclude = NULL)

dim(nsduh)

#---
# Labels ####
#---

{
  Hmisc::label(nsduh$cig_lifetime) <- "cigflag: RC-CIGARETTES - EVER USED"
  Hmisc::label(nsduh$cig_agefirst) <- "ircigage: CIGARETTE AGE OF FIRST USE - IMPUTATION REVISED"
  Hmisc::label(nsduh$cig_last) <- "ircigrc: CIGARETTE RECENCY - IMPUTATION REVISED"
  Hmisc::label(nsduh$cig_past30d) <- "ircigfm: CIG FREQUENCY PAST MONTH - IMPUTATION REVISED"
  Hmisc::label(nsduh$cig_100_lifetime) <- "CIG100LF: HAVE YOU SMOKED 100 CIGS IN YOUR LIFE"
  Hmisc::label(nsduh$alc_lifetime) <- "alcflag: RC-ALCOHOL - EVER USED"
  Hmisc::label(nsduh$alc_agefirst) <- "iralcage: ALCOHOL AGE OF FIRST USE - IMPUTATION REVISED"
  Hmisc::label(nsduh$alc_last) <- "iralcrc: ALCOHOL RECENCY - IMPUTATION REVISED"
  Hmisc::label(nsduh$alc_past30d) <- "iralcfm: ALCOHOL FREQUENCY PAST MONTH - IMPUTATION REVISED"
  Hmisc::label(nsduh$alc_binge_past30d) <- "IRALCBNG30D: BINGE ALCOHOL FREQUENCY PAST MONTH - IMPUTATION REVISED"
  Hmisc::label(nsduh$alc_past1y) <- "iralcfy: ALCOHOL FREQUENCY PAST YEAR - IMPUTATION REVISED"
  Hmisc::label(nsduh$mj_lifetime) <- "mrjflag: RC-MARIJUANA - EVER USED"
  Hmisc::label(nsduh$mj_agefirst) <- "irmjage: MARIJUANA AGE OF FIRST USE - IMPUTATION REVISED"
  Hmisc::label(nsduh$mj_last) <- "irmjrc: MARIJUANA RECENCY - IMPUTATION REVISED"
  Hmisc::label(nsduh$mj_past30d) <- "irmjfm: MARIJUANA FREQUENCY PAST MONTH - IMPUTATION REVISED"
  Hmisc::label(nsduh$mj_past1y) <- "irmjfy: MARIJUANA FREQUENCY PAST YEAR - IMPUTATION REVISED"
  Hmisc::label(nsduh$her_lifetime) <- "herflag: RC-HEROIN - EVER USED"
  Hmisc::label(nsduh$her_agefirst) <- "irherage: HEROIN AGE OF FIRST USE - IMPUTATION REVISED"
  Hmisc::label(nsduh$her_last) <- "irherrc: HEROIN RECENCY - IMPUTATION REVISED"
  Hmisc::label(nsduh$her_past30d) <- "irherfm: HEROIN FREQUENCY PAST MONTH - IMPUTATION REVISED"
  Hmisc::label(nsduh$her_past1y) <- "irherfy: HEROIN FREQUENCY PAST YEAR - IMPUTATION REVISED"
  Hmisc::label(nsduh$metham_lifetime) <- "methamflag: RC-METHAMPHETAMINE - EVER USED"
  Hmisc::label(nsduh$metham_agefirst) <- "irmethamage: METHAMPHETAMINE AGE OF FIRST USE - IMPUTATION REVISED"
  Hmisc::label(nsduh$metham_last) <- "irmethamrec: METHAMPHETAMINE RECENCY - IMPUTATION REVISED"
  Hmisc::label(nsduh$metham_past30d) <- "IRMETHAM30N: METHAMPHETAMINE FREQUENCY PAST MONTH - IMPUTATION REVISED"
  Hmisc::label(nsduh$metham_past1y) <- "irmethamyfq: METH FREQUENCY PAST YEAR - IMPUTATION REVISED"
  Hmisc::label(nsduh$painpill_lifetime) <- "pnrnmflag: RC-PAIN RELIEVERS - EVER MISUSED"
  Hmisc::label(nsduh$painpill_last) <- "irpnrnmrec: PAIN RELIEVER MISUSE RECENCY - IMPUTATION REVISED"
  Hmisc::label(nsduh$her_painpill_past30d_01) <- "opinmmon: RC-OPIOIDS - PAST MONTH MISUSE"
  Hmisc::label(nsduh$her_painpill_past1y_01) <- "opinmyr: RC-OPIOIDS - PAST YEAR MISUSE"
  Hmisc::label(nsduh$her_painpill_past1y_cat) <- "herpnryr: RC-HEROIN USE AND/OR PAIN RELIEVER MISUSE - PAST YEAR"
  Hmisc::label(nsduh$illicit_lifetime) <- "illflag: RC-ANY ILLICIT DRUG - EVER USED"
  Hmisc::label(nsduh$illicit_past30d_01) <- "illmon: RC-ANY ILLICIT DRUG - PAST MONTH USE"
  Hmisc::label(nsduh$illicit_past1y_01) <- "illyr: RC-ANY ILLICIT DRUG - PAST YEAR USE"
  Hmisc::label(nsduh$tx_substance_lifetime) <- "TXEVRRCVD2: RC-RCVD TRT ANY LOC FOR ILL DRG OR ALC USE IN LIFETIME"
  Hmisc::label(nsduh$tx_adult_mh_past1y) <- "AMHTXRC3: RC-RCVD ANY MENTAL HEALTH TRT IN PST YR"
  Hmisc::label(nsduh$bmi) <- "BMI2: RC-BODY MASS INDEX (BMI)"
  Hmisc::label(nsduh$psych_adult_distress_past30d) <- "K6SCMON: RC-K6 TOTAL SCORE IN PAST MONTH"
  Hmisc::label(nsduh$psych_adult_distress_past1y) <- "K6SCYR: RC-K6 TOTAL SCORE IN WORST MONTH OF PAST YEAR"
  Hmisc::label(nsduh$psych_adult_suicidal_thoughs_past1y) <- "mhsuithk: RC-SERIOUSLY THOUGHT ABOUT KILLING SELF IN PAST YEAR"
  Hmisc::label(nsduh$psych_adult_whodas) <- "WHODASC2: RC-WHODAS TOTAL SCORE"
  Hmisc::label(nsduh$psych_adult_mdd_lifetime) <- "amdelt: RC-ADULT: LIFETIME MAJOR DEPRESSIVE EPISODE (MDE)"
  Hmisc::label(nsduh$psych_adult_mdd_past1y) <- "amdeyr: RC-ADULT: PAST YEAR MAJOR DEPRESSIVE EPISODE (MDE)"
  Hmisc::label(nsduh$demog_sex) <- "irsex: IMPUTATION REVISED GENDER"
  Hmisc::label(nsduh$demog_age_cat) <- "AGE2: RECODE - FINAL EDITED AGE"
  Hmisc::label(nsduh$demog_age_cat6) <- "CATAG6: RC-AGE CATEGORY RECODE (6 LEVELS)"
  Hmisc::label(nsduh$demog_race) <- "NEWRACE2: RC-RACE/HISPANICITY RECODE (7 LEVELS)"
  Hmisc::label(nsduh$demog_health) <- "HEALTH2: RC-OVERALL HEALTH RECODE"
  Hmisc::label(nsduh$demog_marital) <- "irmarit: IMPUTATION REVISED MARITAL STATUS"
  Hmisc::label(nsduh$demog_educ) <- "IREDUHIGHST2: EDUCATION - RECODED IMPUTATION REVISED"
  Hmisc::label(nsduh$demog_educ_cat4) <- "eduhighcat: RC-EDUCATION CATEGORIES"
  Hmisc::label(nsduh$demog_employ) <- "IRWRKSTAT18: EMPLOYMENT STATUS 18+ - IMPUTATION REVISED"
  Hmisc::label(nsduh$demog_income) <- "income: RC-TOTAL FAMILY INCOME RECODE"
  Hmisc::label(nsduh$demog_urban) <- "COUTYP4: COUNTY METRO/NONMETRO STATUS (2013 3-LEVEL)"
}

cbind(names(nsduh), Hmisc::label(nsduh))

#---
# Subsample with replacement using survey weights ####
#---

# ANALWT_C = person-level analysis weights

summary(nsduh$ANALWT_C) # All > 0

nsduh_adult <- nsduh %>% 
  filter(demog_age_cat6 %in% levels(nsduh$demog_age_cat6)[-1])
dim(nsduh_adult) # 42739  55

set.seed(753841)
SUB <- sample(x    = 1:nrow(nsduh_adult),
              size = 1000,
              prob = nsduh_adult$ANALWT_C/sum(nsduh_adult$ANALWT_C),
              replace = T)
nsduh_adult_sub <- nsduh_adult[SUB, ]
nrow(nsduh_adult)
nrow(nsduh_adult_sub)

cbind(round(100*prop.table(table(PUF2019_100920$NEWRACE2)), 2),
      round(100*prop.table(table(nsduh_adult_sub$demog_race)), 2))
#   Unwtd  Wtd
# 1 57.16 64.4
# 2 12.93 11.7
# 3  1.34  0.5
# 4  0.52  0.1
# 5  4.80  5.5
# 6  3.92  2.1
# 7 19.32 15.7

# Drop unused levels
nsduh_adult_sub = droplevels(nsduh_adult_sub)

# Drop variables
nsduh_adult_sub <- nsduh_adult_sub %>%
  select(-vestr, -verep, -ANALWT_C)

# Reapply labels (for some reason some are dropped during the subsetting)
for(i in 1:ncol(nsduh_adult_sub)) {
  NAME <- names(nsduh_adult_sub)[i]
  Hmisc::label(nsduh_adult_sub[[NAME]]) <- Hmisc::label(nsduh[[NAME]]) 
}

# cbind(names(nsduh), Hmisc::label(nsduh))
# cbind(names(nsduh_adult_sub), Hmisc::label(nsduh_adult_sub))
# 
# all(names(nsduh) == names(nsduh_adult_sub))
# cbind(names(nsduh), names(nsduh_adult_sub))

#---
# Summarize data ####
#---

write(Hmisc::html(Hmisc::describe(nsduh)),           "nsduh2019.html")
write(Hmisc::html(Hmisc::describe(nsduh_adult_sub)), "nsduh2019_adult_sub.html")

#---
# Save dataset ####
#---

# Full dataset
save(nsduh,           file="nsduh2019_rmph.RData")
# Resampling of 1000 adults
save(nsduh_adult_sub, file="nsduh2019_adult_sub_rmph.RData")


