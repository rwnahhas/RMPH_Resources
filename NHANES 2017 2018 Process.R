# NOTE: This code is provided solely to create example datasets used in Introduction to Regression Methods for Public Health using R
#       No warranty is provided regarding the correctness of this code or regarding any analyses carried out with the
#       datasets produced by this code.

#---
# Program Name:    NHANES 2017 2018 Process.R
# Analyst:         Ramzi W. Nahhas
# Date:            September 1, 2021
# Updated:         March 11, 2024
# Contents:        Download and process NHANES data to create example datasets
#---

# install.packages("tidyverse", "nhanesA", "Hmisc", "survey")
library(tidyverse)
library(nhanesA) # To import NHANES files

# Just to save typing
mytab  <- function(x) table(x, exclude = NULL)
myxtab <- function(x1, x2) table(x1, x2, exclude = NULL)

#---
# Load NHANES 2017-2018 data ####
#---

# Use SI units!
# Conversions from conventional to SI are not simple. https://www.ncbi.nlm.nih.gov/books/NBK33478/
# Just use variables that have SI versions.

# summary(hdl$LBDHDDSI / hdl$LBDHDD) # 0.02586 according to https://www.ncbi.nlm.nih.gov/books/NBK33478/
# summary(trigly$LBDTRSI / trigly$LBXTR) # 0.01129 according to https://www.ncbi.nlm.nih.gov/books/NBK33478/
# summary(trigly$LBDLDMSI / trigly$LBDLDLM) # 0.02586 - Same as HDL
# # The conversions different for different measures

#---
# Demographics ####
#---

# UPDATE: 3/11/2024. nhanes() now reads in the variables as factors, so the old code that converted numeric
#                    to factors results in missing values. Updated the code to remove coding as factors.
#                    cleanse_numeric=T sets missing value codes to NA.

demo    <- nhanes("DEMO_J", cleanse_numeric=T) %>% # Demographics: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.htm
  select(SEQN, WTINT2YR, WTMEC2YR, SDMVPSU, SDMVSTRA, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, INDHHIN2)
# demo    <- nhanesTranslate("DEMO_J", names(demo), data = demo)
# Works but creates labels that are too long

# The NAs used to be 0 in the old version.
# Set to 0 to match the book (and the online codebook!)
summary(demo$WTMEC2YR)
demo$WTMEC2YR[is.na(demo$WTMEC2YR)] <- 0
summary(demo$WTMEC2YR)

#---
# Examination data ####
#---
bpx     <- nhanes("BPX_J", cleanse_numeric=T) %>% # Blood Pressure: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPX_J.htm
  select(SEQN, PEASCCT1, BPXPLS, BPXSY1, BPXSY2, BPXSY3, BPXDI1, BPXDI2, BPXDI3)
summary(bpx) # No missing value codes
bmx     <- nhanes("BMX_J", cleanse_numeric=T) %>% #  Body Measures: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.htm
  select(SEQN, BMXWT, BMXHT, BMXBMI, BMXARMC, BMXWAIST)
summary(bmx) # No missing value codes
dxx     <- nhanes("DXX_J", cleanse_numeric=T) %>% #  Dual-Energy X-ray Absorptiometry - Whole Body: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXX_J.htm
  select(SEQN, DXXTRFAT, DXDTRPF, DXDTOBMC, DXDTOBMD, DXDTOFAT, DXDTOPF)
summary(dxx) # No missing value codes

#---
# Laboratory data ####
#---

alb_cr <- nhanes("ALB_CR_J", cleanse_numeric=T) %>% #  Albumin & Creatinine - Urine: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/ALB_CR_J.htm
  select(SEQN, URXCRS)
# No SI version for albumin
summary(alb_cr) # No missing value codes
hdl    <- nhanes("HDL_J", cleanse_numeric=T)    %>% #  Cholesterol - High - Density Lipoprotein (HDL): https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HDL_J.htm
  select(SEQN, LBDHDDSI)
summary(hdl) # No missing value codes
trigly <- nhanes("TRIGLY_J", cleanse_numeric=T) %>% #  Cholesterol - Low-Density Lipoproteins (LDL) & Triglycerides: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/TRIGLY_J.htm
  select(SEQN, WTSAF2YR, LBDTRSI, LBDLDMSI)
summary(trigly) # No missing value codes
tchol  <- nhanes("TCHOL_J", cleanse_numeric=T)  %>% #  Cholesterol - Total: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/TCHOL_J.htm
  select(SEQN, LBDTCSI)
summary(tchol) # No missing value codes
fertin <- nhanes("FERTIN_J", cleanse_numeric=T) %>% #  Ferritin: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/FERTIN_J.htm
  select(SEQN, LBDFERSI)
summary(fertin) # No missing value codes
ghb    <- nhanes("GHB_J", cleanse_numeric=T)    %>% #  Glycohemoglobin: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/GHB_J.htm
  select(SEQN, LBXGH)
summary(ghb) # No missing value codes
ins    <- nhanes("INS_J", cleanse_numeric=T)    %>% #  Insulin: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/INS_J.htm
  select(SEQN, LBDINSI)
summary(ins) # No missing value codes
glu    <- nhanes("GLU_J", cleanse_numeric=T)    %>% #  Plasma Fasting Glucose: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/GLU_J.htm
  select(SEQN, LBDGLUSI)
summary(glu) # No missing value codes

#---
# Questionnaire data ####
#---

alq    <- nhanes("ALQ_J", cleanse_numeric=T)    %>% #  Alcohol Use: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/ALQ_J.htm
  select(SEQN, ALQ111, ALQ121, ALQ130, ALQ151)
# # Skip pattern
# table(alq$ALQ111, alq$ALQ130, exclude = NULL) # If 2, then skip
# table(alq$ALQ121, alq$ALQ130, exclude = NULL) # If 0, then skip
# table(alq$ALQ111, alq$ALQ151, exclude = NULL) # If 2, then skip
# table(alq$ALQ121, alq$ALQ151, exclude = NULL) # Not skipped
bpq    <- nhanes("BPQ_J", cleanse_numeric=T)    %>% #  Blood Pressure & Cholesterol: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPQ_J.htm
  select(SEQN, BPQ020, BPD035, BPQ080)
diq    <- nhanes("DIQ_J", cleanse_numeric=T)    %>% #  Diabetes: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.htm
  select(SEQN, DIQ010, DID040)
dlq    <- nhanes("DLQ_J", cleanse_numeric=T)    %>% #  Disability: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DLQ_J.htm
  select(SEQN, DLQ010, DLQ020)
duq    <- nhanes("DUQ_J", cleanse_numeric=T)    %>% #  Drug Use: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DUQ_J.htm
  select(SEQN, DUQ200, DUQ210, DUQ213, DUQ230, DUQ240)
# # Skip pattern
# table(duq$DUQ200, duq$DUQ230, exclude = NULL) # If 2, then skip
hiq    <- nhanes("HIQ_J", cleanse_numeric=T)   %>% # Health Insurance: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HIQ_J.htm
  select(SEQN, HIQ011) 
mcq    <- nhanes("MCQ_J", cleanse_numeric=T)   %>% # Medical conditions: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.htm
  select(SEQN, MCQ010, MCQ025,
         MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F,
         MCD180B, MCD180C, MCD180D, MCD180E, MCD180F,
         MCQ220,  MCD240A)
dpq    <- nhanes("DPQ_J", cleanse_numeric=T)   %>% # Mental Health - Depression Screener: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DPQ_J.htm
  select(SEQN, DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, DPQ060, DPQ070, DPQ080, DPQ090)
paq    <- nhanes("PAQ_J", cleanse_numeric=T)   %>% # Physical Activity - https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/PAQ_J.htm
    select(SEQN, PAQ605, PAQ610, PAD615, PAQ635, PAQ640, PAD645, PAQ650, PAQ655, PAD660)
# # Skip pattern
# summary(paq$PAQ610[paq$PAQ605 == 2]) # Set these to 0
# summary(paq$PAD615[paq$PAQ605 == 2]) # Set these to 0
# summary(paq$PAQ640[paq$PAQ635 == 2]) # Set these to 0
# summary(paq$PAD645[paq$PAQ635 == 2]) # Set these to 0
# summary(paq$PAQ655[paq$PAQ650 == 2]) # Set these to 0
# summary(paq$PAD660[paq$PAQ650 == 2]) # Set these to 0
slq    <- nhanes("SLQ_J", cleanse_numeric=T)   %>% # Sleep Disorders: https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SLQ_J.htm
  select(SEQN, SLD012, SLD013, SLQ030, SLQ040, SLQ050, SLQ120)
smq    <- nhanes("SMQ_J", cleanse_numeric=T)   %>% # Smoking - Cigarette Use - https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.htm
  select(SEQN, SMQ020, SMQ040)

#---
# Merge Files ####
#---

# # Count observations and variables before merging
# NOBS <- NVAR <- rep(NA, length(FILES))
# for(i in 1:length(FILES)) {
#   NOBS[i] <- nrow(get(FILES[i]))
#   NVAR[i] <- ncol(get(FILES[i]))
# }
# cbind(FILES, NOBS, NVAR)
# 
# # Check for names in common before merging
# CHECK <- array(NA, dim = rep(length(FILES), 2))
# for(i in 1:dim(CHECK)[1]) {
#   for(j in 1:dim(CHECK)[2]) {
#     if (i != j) {
#       COMMON <- intersect(names(get(FILES[i])), names(get(FILES[j])))
#       if (length(COMMON) > 1) {
#         print(FILES[i])
#         print(FILES[j])
#         print(COMMON)
#       } else {
#         CHECK[i,j] <- intersect(names(get(FILES[i])), names(get(FILES[j])))
#       }
#       rm(COMMON)
#     }
#   }
# }
# # If anything printed here, then there are variables in common
# CHECK
# # Should be all SEQN except for NAs on the diagnonal
# table(CHECK, exclude = NULL)
# # Should be all SEQN, with # of NAs equal to # of datasets
# # If not, go above and remove common variables from at least one dataset
# 
# # Check # of observations in commmon
# rm(CHECK)
# CHECK <- array(NA, dim = rep(length(FILES), 2))
# for(i in 1:dim(CHECK)[1]) {
#   for(j in 1:dim(CHECK)[2]) {
#     if (i != j) {
#       CHECK[i,j] <- nrow(merge(get(FILES[i]), get(FILES[j])))
#       if(CHECK[i,j] == 0) {
#         print(FILES[i])
#         print(FILES[j])
#         cat("\n\n")
#       }
#     }
#   }
# }
# CHECK
# sum(CHECK == 0, na.rm = T)
# # Even if there are some zeros you could merge
# # Some were only measured in subsamples, some of which do not overlap
# # OK to merge if you use all.x = T and merge them all with the demographics since that was on everyone.

# Merge
ls()
FILES <- c("alb_cr", "alq", "bmx", "bpq", "bpx", "demo", "diq", "dlq", "dpq", "duq", "dxx", "fertin", "ghb",
           "glu", "hdl", "hiq", "ins", "mcq", "paq", "slq", "smq", "tchol", "trigly")
FILES     <- FILES[-(which(FILES == "demo"))]
nhanes0   <- merge(get("demo"), get(FILES[1]), all.x = T, by = "SEQN")
for(i in 2:length(FILES)){
  nhanes0 <- merge(nhanes0,     get(FILES[i]), all.x = T, by = "SEQN")
}

dim(demo)
# 9254   10
dim(nhanes0)
# 9254  96

# How many missing values?
myfun <- function(x) sum(is.na(x))
unlist(lapply(nhanes0, myfun))

# DXA note
# I checked the trunk fat invalidity code... Trunk fat was already set to missing for that
# Assume all are OK
# table(nhanes1$DXATRTV, exclude = NULL)
# summary(nhanes1[nhanes1$DXATRTV > 0, ])

# NOTE: Variables are already labeled
# Can only see using Hmisc
# Hmisc::describe(nhanes0)

#---
# Process ####
#---

# For the text:
# UPPER CASE means same as original data (possibly with missing data codes set to missing)
# lower case means derived here

# # Compute SBP and DBP correctly
# # "The NHANES analytic and reporting guidelines recommend using the mean of the second and third blood pressures."
# # From <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3491581/> 
# # "In determining mean BP for individuals, the first BP was used if only 1 measurement was obtained.
# # The second BP was used if 2 readings were taken; second and third values were averaged when available."
# # https://jamanetwork.com/journals/jama/fullarticle/185953
# # This doesnt capture all the possibilities!
# 
# tmp1 <- as.numeric(!is.na(nhanes0$BPXSY1))
# tmp2 <- as.numeric(!is.na(nhanes0$BPXSY2))
# tmp3 <- as.numeric(!is.na(nhanes0$BPXSY3))
# tmp4 <- paste(tmp1, tmp2, tmp3)
# table(tmp4, exclude = NULL)
# # 0 0 0  0 0 1  0 1 0  0 1 1  1 0 0  1 0 1  1 1 0  1 1 1 
# #  2540     36     17    359     49     66    110   6077 
# 
# tmp1 <- as.numeric(!is.na(nhanes0$BPXDI1))
# tmp2 <- as.numeric(!is.na(nhanes0$BPXDI2))
# tmp3 <- as.numeric(!is.na(nhanes0$BPXDI3))
# tmp4 <- paste(tmp1, tmp2, tmp3)
# table(tmp4, exclude = NULL)
# # 0 0 0  0 0 1  0 1 0  0 1 1  1 0 0  1 0 1  1 1 0  1 1 1 
# #  2540     36     17    359     49     66    110   6077 
# # Identical
# 
# # 1) Use mean of 2 and 3
# # 2) If missing, use mean of 1 and 3
# # 3) If missing, use mean of 1 and 2
# # 4) If missing, use 3, then 2, then 1

# NOTE: Age of X variables left as missing when X = No
#       If using for a survival analysis, can set to be current age and censored (but do not change now)

nhanes1 <- nhanes0 %>%
  mutate(RIDRETH3 = factor(RIDRETH3,
                           labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                                      "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Multi")),
         DMDEDUC2 = factor(DMDEDUC2,
                           levels = c("Less than 9th grade",
                                      "9-11th grade (Includes 12th grade with no diploma)",
                                      "High school graduate/GED or equivalent",
                                      "Some college or AA degree",
                                      "College graduate or above"),
                           labels = c("< 9", "9-11", "HS/GED", "Some college/AA", "College grad")),
         income   = fct_collapse(INDHHIN2,
                                 "< $25,000" = c("$ 0 to $ 4,999",
                                                 "$ 5,000 to $ 9,999",
                                                 "$10,000 to $14,999",
                                                 "$15,000 to $19,999",
                                                 "$20,000 to $24,999",
                                                 "Under $20,000"),
                                 "$25,000 to < $55,000" = c("$25,000 to $34,999",
                                                            "$35,000 to $44,999",
                                                            "$45,000 to $54,999"),
                                 "$55,000+" = c("$55,000 to $64,999",
                                                "$65,000 to $74,999",
                                                "$75,000 to $99,999",
                                                "$100,000 and Over")),
         income   = factor(income,
                           levels = c("< $25,000", "$25,000 to < $55,000", "$55,000+")),
         sbp      = (as.numeric(BPXSY2) + as.numeric(BPXSY3)) / 2,
         sbp      = case_when(is.na(sbp) ~ (as.numeric(BPXSY1) + as.numeric(BPXSY3)) / 2,
                              TRUE       ~ sbp),
         sbp      = case_when(is.na(sbp) ~ (as.numeric(BPXSY1) + as.numeric(BPXSY2)) / 2,
                              TRUE       ~ sbp),
         sbp      = case_when(is.na(sbp) ~ as.numeric(BPXSY3),
                              TRUE       ~ sbp),
         sbp      = case_when(is.na(sbp) ~ as.numeric(BPXSY2),
                              TRUE       ~ sbp),
         sbp      = case_when(is.na(sbp) ~ as.numeric(BPXSY1),
                              TRUE       ~ sbp),
         dbp      = (as.numeric(BPXDI2) + as.numeric(BPXDI3)) / 2,
         dbp      = case_when(is.na(dbp) ~ (as.numeric(BPXDI1) + as.numeric(BPXDI3)) / 2,
                              TRUE       ~ dbp),
         dbp      = case_when(is.na(dbp) ~ (as.numeric(BPXDI1) + as.numeric(BPXDI2)) / 2,
                              TRUE       ~ dbp),
         dbp      = case_when(is.na(dbp) ~ as.numeric(BPXDI3),
                              TRUE       ~ dbp),
         dbp      = case_when(is.na(dbp) ~ as.numeric(BPXDI2),
                              TRUE       ~ dbp),
         dbp      = case_when(is.na(dbp) ~ as.numeric(BPXDI1),
                              TRUE       ~ dbp),
         # Skip pattern - impute 0 if no drinks in last year
         ALQ130   = case_when(ALQ111 == "No" | ALQ121 == "Never in the last year" ~ as.double(0),
                              TRUE                      ~ as.double(ALQ130)),
         # Skip pattern: Impute No if never drink
         ALQ151   = case_when(ALQ151 == "Yes"                  ~ 1,
                              ALQ151 == "No" |  ALQ111 == "No" ~ 2),
         ALQ151   = factor(ALQ151, levels = 1:2, labels = c("Yes", "No")),

         BPQ020   = factor(BPQ020, levels = c("Yes", "No")),
         BPQ080   = factor(BPQ080, levels = c("Yes", "No")),
         
         DIQ010   = factor(DIQ010, levels = c("Yes", "No", "Borderline")),
         DID040   = case_when(DID040  ==    666       ~ as.double(0.5),
                              TRUE                    ~ as.double(DID040)),
         DLQ010   = factor(DLQ010, levels = c("Yes", "No")),
         DLQ020   = factor(DLQ020, levels = c("Yes", "No")),
         # Skip pattern: Impute 0 if never used marij
         DUQ230   = case_when(DUQ200 == "No" ~ as.double(0),
                              TRUE           ~ as.double(DUQ230)),
         DUQ200   = factor(DUQ200, levels = c("Yes", "No")),
         DUQ240   = factor(DUQ240, levels = c("Yes", "No")),
         
         HIQ011   = factor(HIQ011, levels = c("Yes", "No")),
         
         MCQ010   = factor(MCQ010, levels = c("Yes", "No")),
         
         MCQ160B   = factor(MCQ160B, levels = c("Yes", "No")),
         MCQ160C   = factor(MCQ160C, levels = c("Yes", "No")),
         MCQ160D   = factor(MCQ160D, levels = c("Yes", "No")),
         MCQ160E   = factor(MCQ160E, levels = c("Yes", "No")),
         MCQ160F   = factor(MCQ160F, levels = c("Yes", "No")),
         
         # Binary CVD variable (Yes if ANY yes, otherwise No if ANY no)
         cvd      = case_when(MCQ160B == "Yes" | MCQ160C == "Yes" | MCQ160D == "Yes" | MCQ160E == "Yes" | MCQ160F == "Yes" ~ 1,
                              MCQ160B == "No"  | MCQ160C == "No"  | MCQ160D == "No"  | MCQ160E == "No"  | MCQ160F == "No"  ~ 2),
         cvd      = factor(cvd, levels = 1:2, labels = c("Yes", "No")),
         
         MCQ220   = factor(MCQ220, levels = c("Yes", "No")),
         
         DPQ010   = as.numeric(DPQ010) - 1,
         DPQ010   = ifelse(DPQ010 > 3, NA, DPQ010),
         DPQ020   = as.numeric(DPQ020) - 1,
         DPQ020   = ifelse(DPQ020 > 3, NA, DPQ020),
         DPQ030   = as.numeric(DPQ030) - 1,
         DPQ030   = ifelse(DPQ030 > 3, NA, DPQ030),
         DPQ040   = as.numeric(DPQ040) - 1,
         DPQ040   = ifelse(DPQ040 > 3, NA, DPQ040),
         DPQ050   = as.numeric(DPQ050) - 1,
         DPQ050   = ifelse(DPQ050 > 3, NA, DPQ050),
         DPQ060   = as.numeric(DPQ060) - 1,
         DPQ060   = ifelse(DPQ060 > 3, NA, DPQ060),
         DPQ070   = as.numeric(DPQ070) - 1,
         DPQ070   = ifelse(DPQ070 > 3, NA, DPQ070),
         DPQ080   = as.numeric(DPQ080) - 1,
         DPQ080   = ifelse(DPQ080 > 3, NA, DPQ080),
         DPQ090   = as.numeric(DPQ090) - 1,
         DPQ090   = ifelse(DPQ090 > 3, NA, DPQ090),
         phq9     = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090,
         
         # Skip pattern: Set to days and minutes to 0 if no activity of that type
         PAQ605   = factor(PAQ605, levels = c("Yes", "No")),
         PAQ610   = case_when(PAQ605 == "No"            ~ as.double(0),
                              TRUE                      ~ as.double(PAQ610)),
         PAD615   = case_when(PAQ605 == "No"            ~ as.double(0),
                              TRUE                      ~ as.double(PAD615)),
         PAQ635   = factor(PAQ635, levels = c("Yes", "No")),
         PAQ640   = case_when(PAQ635 == "No"            ~ as.double(0),
                              TRUE                      ~ as.double(PAQ640)),
         PAD645   = case_when(PAQ635 == "No"            ~ as.double(0),
                              TRUE                      ~ as.double(PAD645)),
         PAQ650   = factor(PAQ650, levels = c("Yes", "No")),
         PAQ655   = case_when(PAQ650 == "No"            ~ as.double(0),
                              TRUE                      ~ as.double(PAQ655)),
         PAD660   = case_when(PAQ650 == "No"            ~ as.double(0),
                              TRUE                      ~ as.double(PAD660)),
         SLQ030   = factor(SLQ030,
                           levels = c("Never",
                                      "Rarely - 1-2 nights a week",
                                      "Occasionally - 3-4 nights a week",
                                      "Frequently - 5 or more nights a week"),
                           labels = c("0", "1-2", "3-4", "5+")),
         SLQ040   = factor(SLQ040,
                           levels = c("Never",
                                      "Rarely - 1-2 nights a week",
                                      "Occasionally - 3-4 nights a week",
                                      "Frequently - 5 or more nights a week"),
                           labels = c("0", "1-2", "3-4", "5+")),
         SLQ050   = factor(SLQ050, levels = c("Yes", "No")),
         SLQ120   = factor(SLQ120,
                           levels = c("Never",
                                      "Rarely - 1 time a month",
                                      "Sometimes - 2-4 times a month",
                                      "Often- 5-15 times a month",
                                      "Almost always - 16-30 times a month"),
                           labels = c("0", "1", "2-4", "5-15", "16-30")),
         smoker   = case_when(SMQ020 == "No"                                            ~ 1,
                              SMQ020 == "Yes" & SMQ040 == "Not at all"                  ~ 2,
                              SMQ020 == "Yes" & SMQ040 %in% c("Every day", "Some days") ~ 3),
         smoker   = factor(smoker, levels = 1:3, labels = c("Never", "Past", "Current")))

#---
# Check derivations ####
#---

# myxtab(nhanes0$RIDRETH3, nhanes1$RIDRETH3)
# myxtab(nhanes0$DMDEDUC2, nhanes1$DMDEDUC2)
# myxtab(nhanes0$INDHHIN2, nhanes1$income)
# summary(nhanes1$sbp) # 2540 NAs. Correct.
# summary(nhanes1$dbp)
# myxtab(nhanes0$ALQ130, nhanes1$ALQ130) # Some NAs go to 0 due. Correct.
# myxtab(nhanes0$ALQ130[nhanes0$ALQ111 == 2 | nhanes0$ALQ121 == 0],
#        nhanes1$ALQ130[nhanes0$ALQ111 == 2 | nhanes0$ALQ121 == 0])
# myxtab(nhanes0$ALQ151, nhanes1$ALQ151) # Some NAs go to No. Correct.
# myxtab(nhanes0$ALQ151[nhanes0$ALQ111 == 2],
#        nhanes1$ALQ151[nhanes0$ALQ111 == 2])
# myxtab(nhanes0$BPQ020, nhanes1$BPQ020)
# summary(nhanes0$BPD035[!(nhanes0$BPD035 %in% c(777,999))])
# summary(nhanes1$BPD035)
# myxtab(nhanes0$BPQ080, nhanes1$BPQ080)
# myxtab(nhanes0$DIQ010, nhanes1$DIQ010)
# summary(nhanes0$DID040[!(nhanes0$DID040 %in% c(666, 777,999))])
# summary(nhanes1$DID040[!(nhanes0$DID040 %in% c(666))])
# summary(nhanes1$DID040[(nhanes0$DID040 %in% c(666))])
# myxtab(nhanes0$DLQ010, nhanes1$DLQ010)
# myxtab(nhanes0$DLQ020, nhanes1$DLQ020)
# myxtab(nhanes0$DUQ200, nhanes1$DUQ200)
# summary(nhanes0$DUQ210[!(nhanes0$DUQ210 %in% c(777,999))])
# summary(nhanes1$DUQ210)
# summary(nhanes0$DUQ213[!(nhanes0$DUQ213 %in% c(777,999))])
# summary(nhanes1$DUQ213)
# summary(nhanes0$DUQ230[!(nhanes0$DUQ230 %in% c(777,999)) & nhanes0$DUQ200 == 1])
# summary(nhanes1$DUQ230[nhanes0$DUQ200 == 1])
# summary(nhanes1$DUQ230[nhanes0$DUQ200 != 1])
# myxtab(nhanes0$DUQ230, nhanes1$DUQ230) # Some NAs go to 0. Correct.
# myxtab(nhanes0$DUQ230[nhanes0$DUQ200 == 2],
#        nhanes1$DUQ230[nhanes0$DUQ200 == 2])
# myxtab(nhanes0$DUQ240, nhanes1$DUQ240)
# myxtab(nhanes0$HIQ011, nhanes1$HIQ011)
# myxtab(nhanes0$MCQ010, nhanes1$MCQ010)
# summary(nhanes0$MCQ025[!(nhanes0$MCQ025 %in% c(77777,99999))])
# summary(nhanes1$MCQ025)
# myxtab(nhanes0$MCQ160B, nhanes1$MCQ160B)
# myxtab(nhanes0$MCQ160C, nhanes1$MCQ160C)
# myxtab(nhanes0$MCQ160D, nhanes1$MCQ160D)
# myxtab(nhanes0$MCQ160E, nhanes1$MCQ160E)
# myxtab(nhanes0$MCQ160F, nhanes1$MCQ160F)
# # Yes -> Yes
# myxtab(nhanes0$MCQ160B, nhanes1$cvd)
# myxtab(nhanes0$MCQ160C, nhanes1$cvd)
# myxtab(nhanes0$MCQ160D, nhanes1$cvd)
# myxtab(nhanes0$MCQ160E, nhanes1$cvd)
# myxtab(nhanes0$MCQ160F, nhanes1$cvd)
# summary(nhanes0$MCD180B[!(nhanes0$MCD180B %in% c(77777,99999))])
# summary(nhanes1$MCD180B)
# summary(nhanes0$MCD180C[!(nhanes0$MCD180C %in% c(77777,99999))])
# summary(nhanes1$MCD180C)
# summary(nhanes0$MCD180D[!(nhanes0$MCD180D %in% c(77777,99999))])
# summary(nhanes1$MCD180D)
# summary(nhanes0$MCD180E[!(nhanes0$MCD180E %in% c(77777,99999))])
# summary(nhanes1$MCD180E)
# summary(nhanes0$MCD180F[!(nhanes0$MCD180F %in% c(77777,99999))])
# summary(nhanes1$MCD180F)
# myxtab(nhanes0$MCQ220, nhanes1$MCQ220)
# summary(nhanes0$MCD240A[!(nhanes0$MCD240A %in% c(77777,99999))])
# summary(nhanes1$MCD240A)
# myxtab(nhanes0$DPQ010, nhanes1$DPQ010)
# myxtab(nhanes0$DPQ020, nhanes1$DPQ020)
# myxtab(nhanes0$DPQ030, nhanes1$DPQ030)
# myxtab(nhanes0$DPQ040, nhanes1$DPQ040)
# myxtab(nhanes0$DPQ050, nhanes1$DPQ050)
# myxtab(nhanes0$DPQ060, nhanes1$DPQ060)
# myxtab(nhanes0$DPQ070, nhanes1$DPQ070)
# myxtab(nhanes0$DPQ080, nhanes1$DPQ080)
# myxtab(nhanes0$DPQ090, nhanes1$DPQ090)
# summary(nhanes1$phq9)
# myxtab(nhanes0$PAQ605, nhanes1$PAQ605)
# summary(nhanes0$PAQ610[!(nhanes0$PAQ610 %in% c(77,99)) & nhanes0$PAQ605 == 1])
# summary(nhanes1$PAQ610[nhanes0$PAQ605 == 1])
# summary(nhanes1$PAQ610[nhanes0$PAQ605 != 1])
# summary(nhanes0$PAD615[!(nhanes0$PAD615 %in% c(7777,9999)) & nhanes0$PAQ605 == 1])
# summary(nhanes1$PAD615[nhanes0$PAQ605 == 1])
# summary(nhanes1$PAD615[nhanes0$PAQ605 != 1])
# myxtab(nhanes0$PAQ635, nhanes1$PAQ635)
# summary(nhanes0$PAQ640[!(nhanes0$PAQ640 %in% c(77,99)) & nhanes0$PAQ635 == 1])
# summary(nhanes1$PAQ640[nhanes0$PAQ635 == 1])
# summary(nhanes1$PAQ640[nhanes0$PAQ635 != 1])
# summary(nhanes0$PAD645[!(nhanes0$PAD645 %in% c(7777, 9999)) & nhanes0$PAQ635 == 1])
# summary(nhanes1$PAD645[nhanes0$PAQ635 == 1])
# summary(nhanes1$PAD645[nhanes0$PAQ635 != 1])
# myxtab(nhanes0$PAQ650, nhanes1$PAQ650)
# summary(nhanes0$PAQ655[!(nhanes0$PAQ655 %in% c(77,99)) & nhanes0$PAQ650 == 1])
# summary(nhanes1$PAQ655[nhanes0$PAQ650 == 1])
# summary(nhanes1$PAQ655[nhanes0$PAQ650 != 1])
# summary(nhanes0$PAD660[!(nhanes0$PAD660 %in% c(7777, 9999)) & nhanes0$PAQ650 == 1])
# summary(nhanes1$PAD660[nhanes0$PAQ650 == 1])
# summary(nhanes1$PAD660[nhanes0$PAQ650 != 1])
# myxtab(nhanes0$SLQ030, nhanes1$SLQ030)
# myxtab(nhanes0$SLQ040, nhanes1$SLQ040)
# myxtab(nhanes0$SLQ050, nhanes1$SLQ050)
# myxtab(nhanes0$SLQ120, nhanes1$SLQ120)
# table(nhanes0$SMQ020, nhanes0$SMQ040, nhanes1$smoker, exclude = NULL)

#---
# Labels ####
#---

# SKIP
# # Label derived variables
# Hmisc::label(nhanes1$income) <- "Annual household income"
# Hmisc::label(nhanes1$sbp)    <- "Systolic BP (mean of 2nd and 3rd)"
# Hmisc::label(nhanes1$dbp)    <- "Diastolic BP (mean of 2nd and 3rd)"
# Hmisc::label(nhanes1$cvd)    <- "Ever told had HF/CHD/Angina/MI/Stroke"
# Hmisc::label(nhanes1$phq9)   <- "PHQ-9 total"
# Hmisc::label(nhanes1$smoker) <- "Smoking status"
# Hmisc::label(nhanes1)

# Drop some extra variables
nhanes <- nhanes1 %>%
  select(-INDHHIN2, -starts_with("BPXSY"), -starts_with("BPXDI"), -SMQ020, -SMQ040, -PEASCCT1, -ALQ111, -ALQ121)

dim(nhanes1) # 9254  102
dim(nhanes)  # 9254   90
summary(nhanes)

Hmisc::describe(nhanes)

#---
# Create 2 subsamples of 1000 adults for unweighted analyses ####
#---
# (I want a subsample so not all the p-values are super small! Just for illustration and teaching.)

# nhanes_adult_int <- nhanes %>% 
#   filter(RIDAGEYR >= 20) 
# dim(nhanes_adult_int) # 5569  116

nhanes_adult_exam <- nhanes %>% 
  filter(RIDAGEYR >= 20 & WTMEC2YR > 0)
dim(nhanes_adult_exam) # 5265  90
# About the same. Just use the examination dataset

# Separate fasting dataset. Take larger subsample. 
nhanes_adult_fast <- nhanes %>% 
  filter(RIDAGEYR >= 20 & WTSAF2YR > 0) 
dim(nhanes_adult_fast) # 2295  90

# Use the weights to subsample so it is at least approximately representative
# (to do an actual weighted analysis, though, do NOT use this dataset!)

# Sample WITH replacement... that will help us get closer to the population distribution
# (especially in in the fasting subsample which is only N = 2295 to begin with)

# Set seed so same results every time
set.seed(354026)
SUB <- sample(x    = 1:nrow(nhanes_adult_exam),
              size = 1000,
              prob = nhanes_adult_exam$WTMEC2YR/sum(nhanes_adult_exam$WTMEC2YR),
              replace = T)
nhanes_adult_exam_sub <- nhanes_adult_exam[SUB, ]
nrow(nhanes_adult_exam)
nrow(nhanes_adult_exam_sub)

set.seed(409875)
SUB <- sample(x    = 1:nrow(nhanes_adult_fast),
              size = 1000,
              prob = nhanes_adult_fast$WTSAF2YR/sum(nhanes_adult_fast$WTSAF2YR),
              replace = T)
nhanes_adult_fast_sub <- nhanes_adult_fast[SUB, ]
nrow(nhanes_adult_fast)
nrow(nhanes_adult_fast_sub)

# Check distribution of race/ethnicity before and after, and using survey()
mydesign <- survey::svydesign(id=~SDMVPSU, weights=~WTMEC2YR, strata=~SDMVSTRA, nest=TRUE, survey.lonely.psu = "adjust", data=nhanes)
mysubset <- subset(mydesign, RIDAGEYR >= 20 & WTMEC2YR > 0)
round(cbind(100*prop.table(table(nhanes_adult_exam$RIDRETH3)),
            100*prop.table(table(nhanes_adult_exam_sub$RIDRETH3)),
            survey::svytable(~RIDRETH3, mysubset, Ntotal=100)), 1)
#                     Raw Approx Weighted correctly
# Mexican American   13.3  9.6  8.8
# Other Hispanic      9.4  9.2  7.0
# Non-Hispanic White 34.3 62.4 62.2
# Non-Hispanic Black 23.6  8.7 11.4
# Non-Hispanic Asian 14.4  6.1  5.9
# Other/Multi         5.0  4.0  4.6

# survey() does not like missing values in weights
tmp <- nhanes
tmp$WTSAF2YR[is.na(tmp$WTSAF2YR)] <- 0
mydesign <- survey::svydesign(id=~SDMVPSU, weights=~WTSAF2YR, strata=~SDMVSTRA, nest=TRUE, survey.lonely.psu = "adjust", data=tmp)
mysubset <- subset(mydesign, RIDAGEYR >= 20 & WTSAF2YR > 0)
round(cbind(100*prop.table(table(nhanes_adult_fast$RIDRETH3)),
            100*prop.table(table(nhanes_adult_fast_sub$RIDRETH3)),
            survey::svytable(~RIDRETH3, mysubset, Ntotal=100)), 1)
#                   Raw Approx Weighted correctly
# Mexican American   14.6 12.0  9.6
# Other Hispanic      9.5  7.1  6.7
# Non-Hispanic White 33.3 60.2 61.9
# Non-Hispanic Black 22.6 11.5 11.3
# Non-Hispanic Asian 14.3  4.8  5.7
# Other/Multi         5.6  4.4  4.8

# (Previously, before using replace = T, the approximation was not as good in the fasting subsample
#  because the sample was 1000 out of 2295 so could only be so good)

# https://www.governing.com/archive/state-minority-population-data-estimates.html
# 2017
# White Non-Hispanic: 60.6 percent
# Black Non-Hispanic: 12.3 percent
# Asian Non-Hispanic: 5.5 percent
# American Indian/Alaska Native Non-Hispanic: 0.7 percent
# Separately, Hispanics of any race accounted for 18.1 percent of the U.S. population.

# This works to make the unweighted analyses a little more realistic.

#---
# Create a character variable to illustrate how to convert to a factor ####
#---
nhanes_adult_fast_sub$cat_not_factor <- c(rep(1, 100), rep(2, 700), rep(3, 200))
is.factor(nhanes_adult_fast_sub$cat_not_factor)

#---
# Save final datasets ####
#---

# Full dataset
save(nhanes,                file = "nhanes1718_rmph.Rdata")

# Subsamples

nhanes_adult_exam_sub <- nhanes_adult_exam_sub %>% 
  select(-SEQN, -WTINT2YR, -WTMEC2YR, -SDMVPSU, -SDMVSTRA)

nhanes_adult_fast_sub <- nhanes_adult_fast_sub %>% 
  select(-SEQN, -WTINT2YR, -WTMEC2YR, -SDMVPSU, -SDMVSTRA)

save(nhanes_adult_exam_sub, file = "nhanes1718_adult_exam_sub_rmph.Rdata")
save(nhanes_adult_fast_sub, file = "nhanes1718_adult_fast_sub_rmph.Rdata")

#---
# Summarize data ####
#---
# load("nhanes1718_rmph.Rdata")
# load("nhanes1718_adult_exam_sub_rmph.Rdata")
# load("nhanes1718_adult_fast_sub_rmph.Rdata")

write(Hmisc::html(Hmisc::describe(nhanes)), "nhanes1718_rmph.html")
write(Hmisc::html(Hmisc::describe(nhanes_adult_exam_sub)), "nhanes1718_adult_exam_sub_rmph.html")
write(Hmisc::html(Hmisc::describe(nhanes_adult_fast_sub)), "nhanes1718_adult_fast_sub_rmph.html")

#---
# Create matched case-control data example ####
#---

load("nhanes1718_adult_exam_sub_rmph.Rdata")
library(tidyverse)

# https://cran.r-project.org/web/packages/ccoptimalmatch/vignettes/ccoptimalmatching_vignette.html
table(nhanes_adult_exam_sub$MCQ220, exclude = NULL)

nhanes_complete <- nhanes_adult_exam_sub %>% 
  select(MCQ220, smoker, RIAGENDR, income) %>% 
  drop_na()
nrow(nhanes_complete)

create_subset <- nhanes_complete %>%
  filter(MCQ220 == "Yes") %>%
  arrange(RIAGENDR, income) %>%
  distinct(RIAGENDR, income, .keep_all = TRUE) %>%
  mutate(subset = 1:n()) %>%
  select(RIAGENDR, income, subset)

nrow(create_subset)
# Unique combinations of gender and income
table(create_subset$RIAGENDR, create_subset$income, exclude = NULL)

# Merge to create case dataset
# Keep all rows from both create_subset and cases
case_with_subset <- nhanes_complete %>%
  filter(MCQ220 == "Yes") %>%
  full_join(create_subset, by = c("RIAGENDR", "income")) %>% 
  mutate(status = "Case")

# All the cases
nrow(case_with_subset)

# Merge to create control dataset
# Keep all controls that match
control_with_subset <- nhanes_complete %>%
  filter(MCQ220 == "No") %>%
  right_join(create_subset, by = c("RIAGENDR", "income")) %>% 
  mutate(status = "Control")

# Controls match on age and education
nrow(control_with_subset)

CCdat <- rbind(case_with_subset, control_with_subset)

table(CCdat$status, exclude = NULL)

table(CCdat$subset, CCdat$MCQ220, exclude = NULL)
table(CCdat$MCQ220, exclude = NULL)

# OLD - The following was not needed for this dataset
# # Missing value for MCQ220 if no matching control.
# CCdat %>%
#   filter(is.na(MCQ220)) %>% 
#   select(subset, MCQ220, RIAGENDR, income)
# 
# CCdat %>%
#   filter(subset == 38) %>% 
#   select(subset, MCQ220, RIAGENDR, income)
# 
# NoMatch <- CCdat %>% 
#   filter(is.na(MCQ220)) %>% 
#   select(subset)
# nrow(NoMatch)
# 
# CCdat <- CCdat %>% 
#   anti_join(NoMatch)
# nrow(CCdat)
# table(CCdat$MCQ220, exclude = NULL)

levels(CCdat$MCQ220)

nhanes_CC <- CCdat

summary(nhanes_CC)

save(nhanes_CC, file = "nhanes_CC_rmph.Rdata")

#---
# Create small n dataset for text example ####
#---

library(tidyverse)
load("nhanes1718_adult_fast_sub_rmph.Rdata")
nhanesf <- nhanes_adult_fast_sub
rm(nhanes_adult_fast_sub)

nhanesf.complete <- nhanesf %>% 
  select(LBDGLUSI, BMXWAIST, smoker, RIDAGEYR,
         RIAGENDR, RIDRETH3, income) %>% 
  drop_na() %>% 
  mutate(race_eth = fct_collapse(RIDRETH3,
                                 "Hispanic"           = c("Mexican American", "Other Hispanic"),
                                 "Non-Hispanic Other" = c("Non-Hispanic Asian", "Other/Multi")))

nhanesf.complete$race_ethb <- nhanesf.complete$race_eth
nhanesf.complete$incomeb <- nhanesf.complete$income

nhanesf.complete <- nhanesf.complete %>% 
  mutate(cRIDAGEYR = RIDAGEYR - 40,
         cBMXWAIST = BMXWAIST - 100)

set.seed(345823)

# Randomly select 50 rows from the dataset
ROWS.SUBSET <- sample(1:nrow(nhanesf.complete), 50)
nhanesf.complete.50 <- nhanesf.complete[ROWS.SUBSET,]
nrow(nhanesf.complete.50)

save(nhanesf.complete.50, file = "nhanesf.complete.50_rmph.Rdata")


#---
# Create small n dataset for exercise ####
#---

library(tidyverse)
load("nhanes1718_adult_fast_sub_rmph.Rdata")
nhanesf <- nhanes_adult_fast_sub
rm(nhanes_adult_fast_sub)

nhanesf.complete <- nhanesf %>% 
  select(LBDTRSI, DXDTRPF, RIDAGEYR, RIAGENDR) %>% 
  drop_na()

set.seed(53247)

# Randomly select 30 rows from the dataset
ROWS.SUBSET <- sample(1:nrow(nhanesf.complete), 30)
nhanesf.complete.30 <- nhanesf.complete[ROWS.SUBSET,]
nrow(nhanesf.complete.30)

save(nhanesf.complete.30, file = "nhanesf.complete.30_rmph.Rdata")







