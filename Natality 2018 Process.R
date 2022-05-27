# NOTE: This code is provided solely to create example datasets used in Introduction to Regression Methods for Public Health using R
#       No warranty is provided regarding the correctness of this code or regarding any analyses carried out with the
#       datasets produced by this code.

#---
# Program Name:    Natality 2018 Process.R
# Analyst:         Ramzi W. Nahhas
# Date:            August 31, 2021
# Contents:        Read 2018 U.S. Natality (births) data and create analysis dataset
#---

# https://www.cdc.gov/nchs/nvss/birth_methods.htm
# https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
# "Please be advised that the births files are massive flatfiles that cannot be opened using, for example, a text editor or spreadsheet application.  Using some type of statistical software, such as SAS, SPSS, and STATA, is your best choice to open and run the files. Since the birth files are flatfiles, you will need input code files. See the National Bureau of Economic Research (NBER) website (https://www.nber.org/research/data/vital-statistics-natality-birth-data) for SAS/SPSS/STATA input code files for the birth files, which are available (free of charge)."
# 
# https://www.nber.org/research/data/vital-statistics-natality-birth-data
# 2018 CSV file: https://data.nber.org/natality/2018/natl2018.csv.zip
# 2018 Documentation: https://data.nber.org/natality/2018/UserGuide2018-508.pdf

#---
# Load data ####
#---
library(tidyverse)
mydat0 <- read_csv("natl2018us.csv")
# Takes a long time to load
dim(mydat0)   # 3801534     240
names(mydat0) <- toupper(names(mydat0))
names(mydat0)

#---
# Check birth weight ####
#---
table(mydat0$BWTR12, mydat0$BWTR4,  exclude = NULL)
tapply(mydat0$DBWT,  mydat0$BWTR12, range, na.rm = T)

#---
# Subsample ####
#---
# Take subsample first - then the rest of the code will run faster

table(mydat0$COMBGEST, exclude = NULL)
table(mydat0$GESTREC3, exclude = NULL)
table(mydat0$BWTR4,    exclude = NULL)

# Drop those with missing gestational age, preterm status, or birth weight

nrow(mydat0)
mydat0 <- mydat0 %>% 
  filter(COMBGEST != 99 & GESTREC3 != 3 & BWTR4 != 4)
nrow(mydat0) # 3796815

set.seed(4504273)
SUB <- sample(1:nrow(mydat0), size = 2000, replace = FALSE)
# Checksum - both should be 0 if same sample as before
3849549788 - sum(SUB)
1109028    - round(sd(SUB))

mydat0_sub <- mydat0[SUB, ]
dim(mydat0_sub) # 2000  240

# Just to save typing
mytab  <- function(x) table(x, exclude = NULL)
myxtab <- function(x1, x2) table(x1, x2, exclude = NULL)

#---
# Process data ####
#---

# # Examine distributions
# mytab(mydat0_sub$BWTR12)
# mytab(mydat0_sub$BWTR4)
# hist(mydat0_sub$DBWT)
# hist(mydat0_sub$DBWT[mydat0_sub$DBWT < 9999])
# mytab(mydat0_sub$BFACIL)
# mytab(mydat0_sub$BFACIL3) # Too sparse
# hist(mydat0_sub$MAGER)
# mytab(mydat0_sub$MRACEHISP)
# mytab(mydat0_sub$FRACEHISP)
# mytab(mydat0_sub$DMAR)
# mytab(mydat0_sub$MEDUC)
# mytab(mydat0_sub$FEDUC)
# hist(mydat0_sub$FAGECOMB)
# mytab(mydat0_sub$PRIORLIVE)
# mytab(mydat0_sub$PRIORDEAD)
# mytab(mydat0_sub$PRIORTERM)
# mytab(mydat0_sub$LBO_REC) # = PRIORLIVE + PRIORDEAD
# mytab(mydat0_sub$TBO_REC) # = PRIORLIVE + PRIORDEAD + PRIORTERM
# myxtab(mydat0_sub$LBO_REC, mydat0_sub$PRIORLIVE + mydat0_sub$PRIORDEAD)
# myxtab(mydat0_sub$TBO_REC, mydat0_sub$PRIORLIVE + mydat0_sub$PRIORDEAD + mydat0_sub$PRIORTERM)
# mytab(mydat0_sub$ILLB_R)
# mytab(mydat0_sub$PRECARE)
# mytab(mydat0_sub$WIC)
# hist(mydat0_sub$CIG_0[mydat0_sub$CIG_0 < 99])
# mytab(mydat0_sub$CIG_0) # Better to use categorical
# mytab(mydat0_sub$CIG0_R)
# mytab(mydat0_sub$CIG1_R)
# mytab(mydat0_sub$CIG2_R)
# mytab(mydat0_sub$CIG3_R)
# mytab(mydat0_sub$CIG_REC)
# myxtab(mydat0_sub$CIG0_R, mydat0_sub$CIG_REC)
# myxtab(mydat0_sub$CIG1_R, mydat0_sub$CIG_REC)
# myxtab(mydat0_sub$CIG2_R, mydat0_sub$CIG_REC)
# myxtab(mydat0_sub$CIG3_R, mydat0_sub$CIG_REC) # CIG_REC is Y if any smoking during pregnancy
# hist(mydat0_sub$BMI[mydat0_sub$BMI < 99.9])
# # mytab(mydat0_sub$RF_PDIAB) # Too sparse
# mytab(mydat0_sub$RF_GDIAB)
# mytab(mydat0_sub$RF_PHYPE)
# mytab(mydat0_sub$RF_GHYPE)
# # mytab(mydat0_sub$RF_EHYPE) # Too sparse
# mytab(mydat0_sub$RF_PPTERM)
# mytab(mydat0_sub$RF_INFTR)
# # mytab(mydat0_sub$RF_FEDRG) # Too sparse
# # mytab(mydat0_sub$RF_ARTEC) # Too sparse
# mytab(mydat0_sub$RF_CESAR)
# mytab(mydat0_sub$NO_RISKS)
# mytab(mydat0_sub$LD_INDL)
# mytab(mydat0_sub$LD_AUGM)
# mytab(mydat0_sub$LD_STER)
# mytab(mydat0_sub$LD_ANTB)
# mytab(mydat0_sub$LD_CHOR) # Too sparse
# mytab(mydat0_sub$LD_ANES)
# mytab(mydat0_sub$APGAR5)
# mytab(mydat0_sub$DPLURAL)
# mytab(mydat0_sub$SEX)
# summary(mydat0_sub$COMBGEST)
# mytab(mydat0_sub$GESTREC3)
# mytab(mydat0_sub$AB_NICU)

# For the text:
# UPPER CASE means same as original data (possibly with missing data codes set to missing)
# lower case means derived here

mydat <- mydat0_sub %>%
  mutate(BWTR4 = factor(BWTR4,
                        levels = 1:3,
                        labels = c("VLBW", "LBW", "Normal")),
         DBWT = na_if(DBWT, 9999),
         MRACEHISP = factor(MRACEHISP,
                             levels = 1:7,
                             labels = c("NH White",
                                        "NH Black", 
                                        "NH Other",
                                        "NH Other",
                                        "NH Other",
                                        "NH Other",
                                        "Hispanic")),
         FRACEHISP = factor(FRACEHISP,
                                levels = 1:7,
                                labels = c("NH White",
                                           "NH Black", 
                                           "NH Other",
                                           "NH Other",
                                           "NH Other",
                                           "NH Other",
                                           "Hispanic")),         
         DMAR = factor(DMAR,
                       levels = 1:2,
                       labels = c("Married", "Unmarried")),
         MEDUC = factor(MEDUC,
                            levels = 1:8,
                            labels = c("<HS",
                                       "<HS",
                                       "HS",
                                       "Some college",
                                       "Some college",
                                       "Bachelor",
                                       "Adv Degree",
                                       "Adv Degree")),
         FEDUC = factor(FEDUC,
                            levels = 1:8,
                            labels = c("<HS",
                                       "<HS",
                                       "HS",
                                       "Some college",
                                       "Some college",
                                       "Bachelor",
                                       "Adv Degree",
                                       "Adv Degree")),
         FAGECOMB  = na_if(FAGECOMB, 99),
         PRIORLIVE = na_if(PRIORLIVE, 99),
         PRIORDEAD = na_if(PRIORDEAD, 99),
         PRIORTERM = na_if(PRIORTERM, 99),
         LBO_REC   = na_if(LBO_REC, 9),
         TBO_REC   = na_if(TBO_REC, 9),
         ILLB_R    = na_if(ILLB_R, 888),
         ILLB_R    = na_if(ILLB_R, 999),
         PRECARE = factor(PRECARE,
                               levels = c(1:9, 0),
                               labels = c("1", "2", "3", "4", "5", "6", "7",
                                          "8-9", "8-9", "None")),
         WIC       = factor(WIC,       levels = c("N", "Y"), labels = c("No", "Yes")),
         CIG_REC   = factor(CIG_REC,   levels = c("N", "Y"), labels = c("No", "Yes")),
         BMI       = na_if(BMI, 99.9),
         RF_GDIAB  = factor(RF_GDIAB,  levels = c("N", "Y"), labels = c("No", "Yes")),
         RF_PHYPE  = factor(RF_PHYPE,  levels = c("N", "Y"), labels = c("No", "Yes")),
         RF_GHYPE  = factor(RF_GHYPE,  levels = c("N", "Y"), labels = c("No", "Yes")),
         RF_PPTERM = factor(RF_PPTERM, levels = c("N", "Y"), labels = c("No", "Yes")), 
         RF_INFTR  = factor(RF_INFTR,  levels = c("N", "Y"), labels = c("No", "Yes")),
         RF_CESAR  = factor(RF_CESAR,  levels = c("N", "Y"), labels = c("No", "Yes")),
         risks     = factor(NO_RISKS,  levels = c(1, 0),     labels = c("No", "Yes")),
         LD_INDL   = factor(LD_INDL,   levels = c("N", "Y"), labels = c("No", "Yes")),
         LD_AUGM   = factor(LD_AUGM,   levels = c("N", "Y"), labels = c("No", "Yes")),
         LD_STER   = factor(LD_STER,   levels = c("N", "Y"), labels = c("No", "Yes")),
         LD_ANTB   = factor(LD_ANTB,   levels = c("N", "Y"), labels = c("No", "Yes")),
         LD_ANES   = factor(LD_ANES,   levels = c("N", "Y"), labels = c("No", "Yes")),
         APGAR5    = na_if(APGAR5, 99),
         DPLURAL   = factor(DPLURAL,
                            levels = 1:5,
                            labels = c("Single", "Twin", "Triplet", "Quadruplet", "Quintuplet or higher")),
         SEX       = factor(SEX, levels = c("M", "F"), labels = c("Male", "Female")),
         COMBGEST  = na_if(COMBGEST, 99),
         preterm   = factor(GESTREC3, levels = c(2, 1), labels = c("No", "Yes")),
         AB_NICU   = factor(AB_NICU, levels = c("N", "Y"), labels = c("No", "Yes"))) %>% 
  select(DBWT, BWTR4, MRACEHISP, FRACEHISP, DMAR, MEDUC, FEDUC, MAGER, FAGECOMB,
         PRIORLIVE, PRIORDEAD, PRIORTERM, LBO_REC, TBO_REC, ILLB_R, ILLB_R, PRECARE,
         WIC, CIG_REC, BMI,
         RF_GDIAB, RF_PHYPE, RF_GHYPE, RF_PPTERM, RF_INFTR, RF_CESAR,
         risks, LD_INDL, LD_AUGM, LD_STER, LD_ANTB, LD_ANES,
         APGAR5, DPLURAL, SEX, COMBGEST, preterm, AB_NICU)

# # Check derivations
# summary(mydat0_sub$DBWT[mydat0_sub$DBWT != 9999])
# summary(mydat$DBWT)
# myxtab(mydat0_sub$BWTR4, mydat$BWTR4)
# myxtab(mydat0_sub$MRACEHISP, mydat$MRACEHISP)
# myxtab(mydat0_sub$FRACEHISP, mydat$FRACEHISP)
# myxtab(mydat0_sub$DMAR, mydat$DMAR)
# myxtab(mydat0_sub$MEDUC, mydat$MEDUC)
# myxtab(mydat0_sub$FEDUC, mydat$FEDUC)
# summary(mydat$MAGER)
# summary(mydat0_sub$FAGECOMB[mydat0_sub$FAGECOMB != 99])
# summary(mydat$FAGECOMB)
# myxtab(mydat0_sub$PRIORLIVE, mydat$PRIORLIVE)
# myxtab(mydat0_sub$PRIORDEAD, mydat$PRIORDEAD)
# myxtab(mydat0_sub$PRIORTERM, mydat$PRIORTERM)
# myxtab(mydat0_sub$LBO_REC, mydat$LBO_REC)
# myxtab(mydat0_sub$TBO_REC, mydat$TBO_REC)
# summary(mydat0_sub$ILLB_R[!(mydat0_sub$ILLB_R %in% c(888,999))])
# summary(mydat$ILLB_R)
# myxtab(mydat0_sub$PRECARE, mydat$PRECARE)
# myxtab(mydat0_sub$WIC, mydat$WIC)
# myxtab(mydat0_sub$CIG_REC, mydat$CIG_REC)
# summary(mydat0_sub$BMI[mydat0_sub$BMI != 99.9])
# summary(mydat$BMI)
# myxtab(mydat0_sub$RF_GDIAB, mydat$RF_GDIAB)
# myxtab(mydat0_sub$RF_PHYPE, mydat$RF_PHYPE)
# myxtab(mydat0_sub$RF_GHYPE, mydat$RF_GHYPE)
# myxtab(mydat0_sub$RF_PPTERM, mydat$RF_PPTERM)
# myxtab(mydat0_sub$RF_INFTR, mydat$RF_INFTR)
# myxtab(mydat0_sub$RF_CESAR, mydat$RF_CESAR)
# myxtab(mydat0_sub$NO_RISKS, mydat$risks)
# myxtab(mydat0_sub$LD_ANES, mydat$LD_ANES)
# myxtab(mydat0_sub$APGAR5, mydat$APGAR5)
# myxtab(mydat0_sub$DPLURAL, mydat$DPLURAL)
# myxtab(mydat0_sub$SEX, mydat$SEX)
# summary(mydat0_sub$COMBGEST[mydat0_sub$COMBGEST != 99])
# summary(mydat$COMBGEST)
# myxtab(mydat0_sub$GESTREC3, mydat$preterm)
# tapply(mydat$COMBGEST, mydat$preterm, range, na.rm=T)
# myxtab(mydat0_sub$AB_NICU, mydat$AB_NICU)

#---
# Survival analysis variables ####
#---

table(mydat$COMBGEST, exclude = NULL)
table(mydat$preterm, exclude = NULL)
tapply(mydat$COMBGEST, mydat$preterm, range, na.rm=T)
# $No
# [1] 37 46
# $Yes
# [1] 17 36

natality <- mydat %>%
  # Create numeric preterm birth indicator
  # Create event time variable censored at 37 weeks
  mutate(preterm01 = as.numeric(preterm == "Yes"),
         gestage37 = case_when(preterm01 == 1 ~ COMBGEST,
                               preterm01 == 0 ~ 37))

table(natality$gestage37, exclude = NULL)
table(natality$preterm01, exclude = NULL)
tapply(natality$gestage37, natality$preterm01, range, na.rm=T)

Hmisc::label(natality$preterm01) <- "Preterm birth"
Hmisc::label(natality$gestage37) <- "Gestational Age (censored at 37 weeks)"

# No censored observations prior to 37 weeks
# Add some to make the dataset more interesting

set.seed(529134)
NCENS <- 40
DROPOUT <- sample(1:nrow(natality), NCENS)
# Checksum
39620 - sum(DROPOUT)
DROPOUT
# 1 censored time at 20, others from 21 to 33
natality$gestage37[DROPOUT] <- c(20, sample(21:33, NCENS-1, replace = T))
natality$preterm01[DROPOUT] <- 0

# Put a few specific rows at the top to make showing the first 5 rows more interesting

natality %>%
  select(gestage37, preterm01, MAGER, MRACEHISP, RF_PPTERM, RF_CESAR) %>%
  as.data.frame() %>% 
  head(50)

TOP  <- c(6, 30, 13, 1, 37)
REST <- (1:nrow(natality))[-TOP]

natality <- rbind(natality[TOP, ],
                  natality[REST,])

dim(natality)
summary(natality)

natality %>%
  select(gestage37, preterm01, MAGER, MRACEHISP, RF_PPTERM, RF_CESAR) %>%
  head(5)

{
  Hmisc::label(natality$DBWT) <- "Birth Weight (g)"
  Hmisc::label(natality$BWTR4) <- "Birth Weight category"
  Hmisc::label(natality$MRACEHISP) <- "Mother Race/Hispanic Origin"
  Hmisc::label(natality$FRACEHISP) <- "Father Race/Hispanic Origin"
  Hmisc::label(natality$DMAR) <- "Marital Status"
  Hmisc::label(natality$MEDUC) <- "Mother Education"
  Hmisc::label(natality$FEDUC) <- "Father Education"
  Hmisc::label(natality$MAGER) <- "Mother Age"
  Hmisc::label(natality$FAGECOMB) <- "Father Age"
  Hmisc::label(natality$PRIORLIVE) <- "Prior Births Now Living"
  Hmisc::label(natality$PRIORDEAD) <- "Prior Births Now Dead"
  Hmisc::label(natality$PRIORTERM) <- "Prior Other Terminations"
  Hmisc::label(natality$LBO_REC) <- "Live Birth Order"
  Hmisc::label(natality$TBO_REC) <- "Total Birth Order"
  Hmisc::label(natality$ILLB_R) <- "Time Since Last Live Birth (months)"
  Hmisc::label(natality$PRECARE) <- "Month Prenatal Care Began"
  Hmisc::label(natality$WIC) <- "WIC"
  Hmisc::label(natality$CIG_REC) <- "Smoked during pregnancy"
  Hmisc::label(natality$BMI) <- "Mother BMI"
  Hmisc::label(natality$RF_GDIAB) <- "Gestational Diabetes"
  Hmisc::label(natality$RF_PHYPE) <- "Pre-pregnancy Hypertension"
  Hmisc::label(natality$RF_GHYPE) <- "Gestational Hypertension"
  Hmisc::label(natality$RF_PPTERM) <- "Previous Preterm Birth"
  Hmisc::label(natality$RF_INFTR) <- "Infertility Treatment Used"
  Hmisc::label(natality$RF_CESAR) <- "Previous Cesarean"
  Hmisc::label(natality$risks) <- "Risk Factors Reported"
  Hmisc::label(natality$LD_INDL) <- "Induction of Labor"
  Hmisc::label(natality$LD_AUGM) <- "Augmentation of Labor"
  Hmisc::label(natality$LD_STER) <- "Steroids"
  Hmisc::label(natality$LD_ANTB) <- "Antibiotics"
  Hmisc::label(natality$LD_ANES) <- "Anesthesia"
  Hmisc::label(natality$APGAR5) <- "Five Minute APGAR Score"
  Hmisc::label(natality$DPLURAL) <- "Plurality"
  Hmisc::label(natality$SEX) <- "Sex of Infant"
  Hmisc::label(natality$COMBGEST) <- "Gestational Age"
  Hmisc::label(natality$preterm) <- "Preterm birth"
  Hmisc::label(natality$AB_NICU) <- "Admission to NICU"
}

#---
# Save final dataset ####
#---

save(natality, file = "natality2018_rmph.Rdata")

#---
# Summarize data ####
#---

# write(Hmisc::html(Hmisc::describe(natality)), "natality2018.html")

# View the HTML file to examine the distribution of each variable

#---
# Create matched case-control data example ####
#---

load("natality2018_rmph.Rdata")
library(tidyverse)

# https://cran.r-project.org/web/packages/ccoptimalmatch/vignettes/ccoptimalmatching_vignette.html
table(natality$AB_NICU, exclude = NULL)

create_subset <- natality %>%
  filter(AB_NICU == "Yes") %>%
  arrange(MEDUC, MAGER) %>%
  distinct(MEDUC, MAGER, .keep_all = TRUE) %>%
  mutate(subset = 1:n()) %>%
  select(MEDUC, MAGER, subset)

nrow(create_subset)
# Unique combinations of mother's education and age
table(create_subset$MEDUC, create_subset$MAGER, exclude = NULL)

# Merge to create case dataset
# Keep all rows from both create_subset and cases
case_with_subset <- natality %>%
  filter(AB_NICU == "Yes") %>%
  full_join(create_subset, by = c("MEDUC", "MAGER"))

# All the cases
nrow(case_with_subset)

# Merge to create control dataset
# Keep all controls that match
control_with_subset <- natality %>%
  filter(AB_NICU == "No") %>%
  right_join(create_subset, by = c("MEDUC", "MAGER"))

# Controls match on age and education
nrow(control_with_subset)

CCdat <- rbind(case_with_subset, control_with_subset)

table(CCdat$subset, CCdat$AB_NICU, exclude = NULL)
table(CCdat$AB_NICU, exclude = NULL)

# Missing value for AB_NICU if no matching control.
CCdat %>%
  filter(is.na(AB_NICU)) %>% 
  select(subset, AB_NICU, MEDUC, MAGER)

CCdat %>%
  filter(subset == 38) %>% 
  select(subset, AB_NICU, MEDUC, MAGER)

NoMatch <- CCdat %>% 
  filter(is.na(AB_NICU)) %>% 
  select(subset)
nrow(NoMatch)

CCdat <- CCdat %>% 
  anti_join(NoMatch)
nrow(CCdat)
table(CCdat$AB_NICU, exclude = NULL)

levels(CCdat$AB_NICU)
levels(CCdat$BWTR4)
natality_CC <- CCdat %>% 
  mutate(BWTR4   = factor(BWTR4,
                          levels = rev(levels(CCdat$BWTR4)))) %>% 
  select(AB_NICU, BWTR4, MEDUC, MAGER)
table(CCdat$AB_NICU, natality_CC$AB_NICU, exclude = NULL)
table(CCdat$BWTR4,   natality_CC$BWTR4,   exclude = NULL)

natality_CC <- natality_CC %>% 
  drop_na()

save(natality_CC, file = "natality_CC_rmph.Rdata")


