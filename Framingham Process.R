# NOTE: This code is provided solely to create example datasets used in Introduction to Regression Methods for Public Health using R
#       No warranty is provided regarding the correctness of this code or regarding any analyses carried out with the
#       datasets produced by this code.

# This teaching data has been rendered anonymous through the application of certain statistical processes such as
# permutations and/or random visit selection. Thus, any inferences derived from this teaching dataset
# is not necessarily a valid estimate.

# ""Users are cautioned that teaching datasets are completely unsuitable for publication purposes since
#   specific statistical measures were used to create anonymous versions."

# See https://biolincc.nhlbi.nih.gov/teaching/

#---
# Program Name:    Framingham Process.R
# Analyst:         Ramzi W. Nahhas
# Date:            October 27, 2021
# Contents:        Process Framingham dataset
#---

# Downloaded 9/10/2021
# https://biolincc.nhlbi.nih.gov/teaching/
# https://biolincc.nhlbi.nih.gov/requests/teaching-dataset-request/

# "(NOTE: Although the enclosed dataset
# contains Framingham data ‘as collected’ by Framingham investigators, specific methods
# were employed to ensure an anonymous dataset that protects patient confidentiality;
# therefore, this dataset is inappropriate for publication purposes. All persons teaching
# with this dataset are encouraged to ensure all users are aware that this dataset is
# inappropriate for publication purposes.)"
# (Framingham Longitudinal Data Documentation.pdf)

#---
# Load data ####
#---

# install.packages(c("tidyverse", "Hmisc", "MOTE", "nlme"))

fram0 <- read.csv("frmgham2.csv", na.strings = c("", " "), header = TRUE, stringsAsFactors = F, as.is = T)
head(fram0)
names(fram0) <- toupper(names(fram0))

# Outcomes:
# Angina Pectoris
# Myocardial Infarction
# Heart Failure
# Cerebrovascular disease

# Subset of Framingham data, 4434 participants
nrow(fram0)
length(unique(fram0$RANDID))

# Long form data
ALL.VARIABLES  <- names(fram0)
TIME.INVARIANT <- names(nlme::gsummary(fram0, form = ~RANDID,
                                       invariantsOnly = T))
TIME.VARYING   <- ALL.VARIABLES[!(ALL.VARIABLES %in% TIME.INVARIANT)]
TIME.INVARIANT
# [1] "RANDID"   "SEX"      "EDUC"     "DEATH"    "ANGINA"   "HOSPMI"   "MI_FCHD"  "ANYCHD"   "STROKE"   "CVD"      "HYPERTEN" "TIMEAP"  
# [13] "TIMEMI"   "TIMEMIFC" "TIMECHD"  "TIMESTRK" "TIMECVD"  "TIMEDTH"  "TIMEHYP" 
TIME.VARYING
# [1] "TOTCHOL"  "AGE"      "SYSBP"    "DIABP"    "CURSMOKE" "CIGPDAY"  "BMI"      "DIABETES" "BPMEDS"   "HEARTRTE" "GLUCOSE"  "PREVCHD" 
# [13] "PREVAP"   "PREVMI"   "PREVSTRK" "PREVHYP"  "TIME"     "PERIOD"   "HDLC"     "LDLC"  

table(fram0$AGE, exclude = NULL)
table(fram0$TIME, exclude = NULL)
table(fram0$PERIOD, exclude = NULL) # 1 2 3

# "Participant clinic data was collected during three examination periods, approximately 6 years
# apart, from roughly 1956 to 1968. Each participant was followed for a total of 24 years for the
# outcome of the following events: Angina Pectoris, Myocardial Infarction, Atherothrombotic
# Infarction or Cerebral Hemorrhage (Stroke) or death."
# (Framingham Longitudinal Data Documentation.pdf)

# Age range at each examination period
tapply(fram0$AGE, fram0$PERIOD, range)
# $`1`
# [1] 32 70
# $`2`
# [1] 39 76
# $`3`
# [1] 44 81

# Years from start of study at each examination period
tapply(fram0$TIME/365.25, fram0$PERIOD, range)
# $`1`
# [1] 0 0
# $`2`
# [1] 4.317591 7.570157
# $`3`
# [1] 10.26146 13.28953

# Outcomes

# "For Each participant the following event data is provided. For each type of event, ‘0' indicates
# the event did not occur during followup, and ‘1' indicates an event did occur during followup.
# Only the first event occurring during the interval of baseline (PERIOD=1) to end of followup is
# provided:"
# (Framingham Longitudinal Data Documentation.pdf)

# Time definition (for ANGINA, but same for others except death)
# "Number of days from Baseline exam to first Angina during the followup
# or Number of days from Baseline to censor date. Censor date may be
# end of followup, death or last known contact date if subject is lost to
# followup"
# (Framingham Longitudinal Data Documentation.pdf)

# Time definition for DEATH
# "Number of days from Baseline exam to death if occurring during
# followup or Number of days from Baseline to censor date. Censor date
# may be end of followup, or last known contact date if subject is lost to
# followup"
# (Framingham Longitudinal Data Documentation.pdf)

table(fram0$ANGINA, exclude = NULL)
summary(fram0$TIMEAP[fram0$ANGINA == 0] / 365.25)
summary(fram0$TIMEAP[fram0$ANGINA == 1] / 365.25)
# Some zeros... might need to add a small number so they are not excluded?
# Median time is larger for event = 0; these are right-censored event times (non-events)

table(fram0$HOSPMI, exclude = NULL)
summary(fram0$TIMEMI[fram0$HOSPMI == 0] / 365.25)
summary(fram0$TIMEMI[fram0$HOSPMI == 1] / 365.25)

table(fram0$MI_FCHD, exclude = NULL)
summary(fram0$TIMEMIFC[fram0$MI_FCHD == 0] / 365.25)
summary(fram0$TIMEMIFC[fram0$MI_FCHD == 1] / 365.25)

table(fram0$ANYCHD, exclude = NULL)
summary(fram0$TIMECHD[fram0$ANYCHD == 0] / 365.25)
summary(fram0$TIMECHD[fram0$ANYCHD == 1] / 365.25)

table(fram0$STROKE, exclude = NULL)
summary(fram0$TIMESTRK[fram0$STROKE == 0] / 365.25)
summary(fram0$TIMESTRK[fram0$STROKE == 1] / 365.25)

table(fram0$CVD, exclude = NULL)
summary(fram0$TIMECVD[fram0$CVD == 0] / 365.25)
summary(fram0$TIMECVD[fram0$CVD == 1] / 365.25)

table(fram0$HYPERTEN, exclude = NULL)
summary(fram0$TIMEHYP[fram0$HYPERTEN == 0] / 365.25)
summary(fram0$TIMEHYP[fram0$HYPERTEN == 1] / 365.25)
# "Note that defining Hypertensive requires exam participation and bias can therefore occur.
# Subjects attending exams regularly have a greater opportunity to be defined as hypertensive.
# Subjects not attending exams would be assumed to be free of hypertension. Since
# Hypertension is highly prevalent, this misclassification could potentially be large."
# (Framingham Longitudinal Data Documentation.pdf)

table(fram0$DEATH, exclude = NULL)
summary(fram0$TIMEDTH[fram0$DEATH == 0] / 365.25)
summary(fram0$TIMEDTH[fram0$DEATH == 1] / 365.25)

# Number of individuals
length(unique(fram0$RANDID))
# 4434

# Number of observations per individual
# table(fram0$RANDID)

# Number of individuals with 1, 2, ... observations
table(table(fram0$RANDID))
#   1    2    3 
# 447  781 3206 

# Unique times
length(unique(fram0$AGE)) # 50 ages
length(unique(fram0$TIME)) # 932 times
length(unique(fram0$PERIOD)) # 3 periods
sort(unique(fram0$PERIOD))

# Individuals per time
table(fram0$PERIOD)
#    1    2    3 
# 4434 3930 3263
# Everyone has Period 1 data

# Total number of observations
nrow(fram0)
# 11627

# NOTE: HDLC and LDLC were collected only in Period 3

library(tidyverse)
# Create factors
fram <- fram0 %>% 
  mutate(SEX = factor(SEX,
                     levels = 1:2,
                     labels = c("Male", "Female")),
         # This is NOT in the coding manual
         # I got the lables below from https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetPdf.cgi?id=phd001597.2
         # EDUC   = factor(EDUC,
         #                 levels = 1:4,
         #                 labels = c("< HS", "HS", "Some college", "College degree")),
         # I got the labels below from Denise Hitchcock, BioLINCC, personal communication 10/28/2021
         EDUC   = factor(EDUC,
                         levels = 1:4,
                         labels = c("0-11 years",
                                    "High School Diploma, GED",
                                    "Some College, Vocational School",
                                    "College (BS, BA) degree or more")),
         BPMEDS = factor(BPMEDS,
                         levels = 0:1,
                         labels = c("Not currently used", "Current Use")),
         CURSMOKE = factor(CURSMOKE,
                         levels = 0:1,
                         labels = c("Not current smoker", "Current smoker")),
         DIABETES = factor(DIABETES,
                           levels = 0:1,
                           labels = c("Not a diabetic", "Diabetic")),
         PREVAP = factor(PREVAP,
                         levels = 0:1,
                         labels = c("Free of disease", "Prevalent disease")),
         PREVCHD = factor(PREVCHD,
                         levels = 0:1,
                         labels = c("Free of disease", "Prevalent disease")),
         PREVMI = factor(PREVMI,
                         levels = 0:1,
                         labels = c("Free of disease", "Prevalent disease")),
         PREVSTRK = factor(PREVSTRK,
                         levels = 0:1,
                         labels = c("Free of disease", "Prevalent disease")),
         PREVHYP = factor(PREVHYP,
                         levels = 0:1,
                         labels = c("Free of disease", "Prevalent disease")))
# Leave as 0/1 since that is what survival::Surv expects
         # ANGINA = factor(ANGINA,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")),
         # HOSPMI = factor(HOSPMI,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")),
         # MI_FCHD = factor(MI_FCHD,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")),
         # ANYCHD = factor(ANYCHD,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")),
         # STROKE = factor(STROKE,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")),
         # CVD = factor(CVD,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")),
         # HYPERTEN = factor(HYPERTEN,
         #                 levels = 0:1,
         #                 labels = c("Censored", "Event")))
           
# # Check
# table(fram0$SEX, fram$SEX, exclude = NULL)
# table(fram0$EDUC, fram$EDUC, exclude = NULL)
# table(fram$PERIOD, exclude = NULL)
# summary(fram$TIME)
# summary(fram$AGE)
# summary(fram$SYSBP)
# summary(fram$DIABP)
# table(fram0$BPMEDS, fram$BPMEDS, exclude = NULL) # Missing values
# table(fram$BPMEDS, fram$PERIOD, exclude = NULL) # Mostly in Period 3
# table(fram0$CURSMOKE, fram$CURSMOKE, exclude = NULL)
# summary(fram$CIGPDAY)
# summary(fram$TOTCHOL)
# summary(fram$HDLC)
# summary(fram$LDLC)
# summary(fram$BMI)
# summary(fram$GLUCOSE)
# table(fram0$DIABETES, fram$DIABETES, exclude = NULL)
# summary(fram$HEARTRTE)
# table(fram0$PREVAP, fram$PREVAP, exclude = NULL)
# table(fram0$PREVCHD, fram$PREVCHD, exclude = NULL)
# table(fram0$PREVMI, fram$PREVMI, exclude = NULL)
# table(fram0$PREVSTRK, fram$PREVSTRK, exclude = NULL)
# table(fram0$PREVHYP, fram$PREVHYP, exclude = NULL)
# table(fram0$ANGINA, fram$ANGINA, exclude = NULL)
# table(fram0$HOSPMI, fram$HOSPMI, exclude = NULL)
# table(fram0$MI_FCHD, fram$MI_FCHD, exclude = NULL)
# table(fram0$ANYCHD, fram$ANYCHD, exclude = NULL)
# table(fram0$STROKE, fram$STROKE, exclude = NULL)
# table(fram0$CVD, fram$CVD, exclude = NULL)
# table(fram0$HYPERTEN, fram$HYPERTEN, exclude = NULL)
# summary(fram$TIMEAP)
# summary(fram$TIMEMI)
# summary(fram$TIMEMIFC)
# summary(fram$TIMECHD)
# summary(fram$TIMESTRK)
# summary(fram$TIMECVD)
# summary(fram$TIMEHYP)
# summary(fram$TIMEDTH)

# Apply labels (see Framingham Longitudinal Data Documentation.pdf)
{
  Hmisc::label(fram$EDUC) <- "Baseline education"
  Hmisc::label(fram$SEX) <- "Sex"
  Hmisc::label(fram$RANDID) <- "Random ID"
  Hmisc::label(fram$TOTCHOL) <- "Serum Cholesterol mg/dL"
  Hmisc::label(fram$AGE) <- "Age (years) at examination"
  Hmisc::label(fram$SYSBP) <- "Systolic BP mmHg"
  Hmisc::label(fram$DIABP) <- "Diastolic BP mmHg"
  Hmisc::label(fram$CURSMOKE) <- "Current Cig Smoker"
  Hmisc::label(fram$CIGPDAY) <- "Cigarettes per day"
  Hmisc::label(fram$BMI) <- "Body Mass Index (kg/m2)"
  Hmisc::label(fram$DIABETES) <- "Diabetic Y/N"
  Hmisc::label(fram$BPMEDS) <- "Anti-hypertensive meds Y/N"
  Hmisc::label(fram$HEARTRTE) <- "Ventricular Rate (beats/min)"
  Hmisc::label(fram$GLUCOSE) <- "Casual Glucose mg/dL"
  Hmisc::label(fram$PREVCHD) <- "Prevalent CHD (MI,AP,CI)"
  Hmisc::label(fram$PREVAP) <- "Prevalent Angina"
  Hmisc::label(fram$PREVMI) <- "Prevalent MI (Hosp,Silent)"
  Hmisc::label(fram$PREVSTRK) <- "Prevalent Stroke (Infarct,Hem)"
  Hmisc::label(fram$PREVHYP) <- "Prevalent Hypertension"
  Hmisc::label(fram$TIME) <- "Days since Index Exam"
  Hmisc::label(fram$PERIOD) <- "Examination cycle"
  Hmisc::label(fram$HDLC) <- "HDL Cholesterol mg/dL"
  Hmisc::label(fram$LDLC) <- "LDL Cholesterol mg/dL"
  Hmisc::label(fram$DEATH) <- "Death indicator"
  Hmisc::label(fram$ANGINA) <- "Incident Angina Pectoris"
  Hmisc::label(fram$HOSPMI) <- "Incident Hospitalized MI"
  Hmisc::label(fram$MI_FCHD) <- "Incident Hosp MI-Fatal CHD"
  Hmisc::label(fram$ANYCHD) <- "Incident Hosp MI, AP, CI, Fatal CHD"
  Hmisc::label(fram$STROKE) <- "Incident Stroke Fatal/non-fatal"
  Hmisc::label(fram$CVD) <- "Incident Hosp MI or Stroke, Fatal or Non"
  Hmisc::label(fram$HYPERTEN) <- "Incident Hypertension"
  Hmisc::label(fram$TIMEAP) <- "Days Baseline to Incident Angina"
  Hmisc::label(fram$TIMEMI) <- "Days Baseline to Incident Hosp MI"
  Hmisc::label(fram$TIMEMIFC) <- "Days Baseline to Incident MI-Fatal CHD"
  Hmisc::label(fram$TIMECHD) <- "Days Baseline to Incident Any CHD"
  Hmisc::label(fram$TIMESTRK) <- "Days Baseline to Incident Stroke"
  Hmisc::label(fram$TIMECVD) <- "Days Baseline to Incident CVD"
  Hmisc::label(fram$TIMEDTH) <- "Days Baseline to Death"
  Hmisc::label(fram$TIMEHYP) <- "Days Baseline to Incident Hypertension"
}

# Fix one individual who has TIMEHYP = 0 but PREVHYPE = "Free of disease"
table(fram$PREVHYP, exclude = NULL)
SUB <- fram$TIMEHYP == 0 & fram$PREVHYP == "Free of disease"
sum(SUB)
fram$PREVHYP[SUB] <- "Prevalent disease"
table(fram$PREVHYP, exclude = NULL)

#---
# Summarize data ####
#---

# # Compare to tables in Framingham Longitudinal Data Documentation.pdf
# 
# # Continuous risk factors
# myfun1 <- function(x, sex, period) {
#   DAT    <- subset(fram, SEX == sex & PERIOD == period)
#   X      <- Hmisc::label(DAT[[x]])
#   N      <- sum(!is.na(DAT[[x]]))
#   NMISS  <- sum( is.na(DAT[[x]]))
#   MEAN   <- MOTE::apa(mean(    DAT[[x]], na.rm = T), decimals = 2)
#   SD     <- MOTE::apa(sd(      DAT[[x]], na.rm = T), decimals = 2)
#   MIN    <- MOTE::apa(min(     DAT[[x]], na.rm = T), decimals = 2)
#   P25    <- MOTE::apa(quantile(DAT[[x]], probs = 0.25, na.rm = T), decimals = 2)
#   MEDIAN <- MOTE::apa(median(  DAT[[x]], na.rm = T), decimals = 2)
#   P75    <- MOTE::apa(quantile(DAT[[x]], probs = 0.75, na.rm = T), decimals = 2)
#   MAX    <- MOTE::apa(max(     DAT[[x]], na.rm = T), decimals = 2)
#   return(c(X, N, NMISS, MEAN, SD, MIN, P25, MEDIAN, P75, MAX))
# }
# 
# wrapper1 <- function(sex, period) {
#   DF <- data.frame(rbind(myfun1("TIME", sex, period),
#                    myfun1("AGE", sex, period),
#                    myfun1("BMI", sex, period),
#                    myfun1("SYSBP", sex, period),
#                    myfun1("DIABP", sex, period),
#                    myfun1("TOTCHOL", sex, period),
#                    myfun1("HDLC", sex, period),
#                    myfun1("LDLC", sex, period),
#                    myfun1("GLUCOSE", sex, period),
#                    myfun1("CIGPDAY", sex, period),
#                    myfun1("HEARTRTE", sex, period)))
#   names(DF) <- c("Risk factor", "N", "NMiss", "Mean", "Std", "Min", "P25", "Median", "P75", "Max")
#   return(DF)
# }
# 
# wrapper1("Male",   1)
# wrapper1("Female", 1)
# wrapper1("Male",   2)
# wrapper1("Female", 2)
# wrapper1("Male",   3)
# wrapper1("Female", 3)
# 
# # Categorical risk factors
# myfun2 <- function(x) {
#   DAT <- fram %>% 
#     mutate(group = case_when(PERIOD == 1 & SEX == "Male"   ~ 1,
#                              PERIOD == 1 & SEX == "Female" ~ 2,
#                              PERIOD == 2 & SEX == "Male"   ~ 3,
#                              PERIOD == 2 & SEX == "Female" ~ 4,
#                              PERIOD == 3 & SEX == "Male"   ~ 5,
#                              PERIOD == 3 & SEX == "Female" ~ 6))
#   if(x == "TOTAL") {
#     X <- "TOTAL"
#     N <- matrix(table(DAT$group),  nrow = 1)
#     P <- 100*N/N
#   } else {
#     X <- Hmisc::label(DAT[[x]])
#     N <- table(DAT[[x]], DAT$group)
#     P <- 100*prop.table(N, margin = 2)
#   }
#   for(i in 1:nrow(P)) P[i,] <- MOTE::apa(as.numeric(P[i,]), decimals = 2)
#   return(cbind(X,
#                N[,1], P[,1],
#                N[,2], P[,2],
#                N[,3], P[,3],
#                N[,4], P[,4],
#                N[,5], P[,5],
#                N[,6], P[,6]))
# }
# 
# DF2 <- data.frame(rbind(myfun2("TOTAL"),
#                        myfun2("CURSMOKE"),
#                        myfun2("DIABETES"),
#                        myfun2("BPMEDS"),
#                        myfun2("PREVCHD"),
#                        myfun2("PREVMI"),
#                        myfun2("PREVAP"),
#                        myfun2("PREVSTRK"),
#                        myfun2("PREVHYP")))
# names(DF2) <- c("Risk Factor",
#                "1 Men N",   "1 Men Percent", 
#                "1 Women N", "1 Women Percent", 
#                "2 Men N",   "2 Men Percent", 
#                "2 Women N", "2 Women Percent", 
#                "3 Men N",   "3 Men Percent", 
#                "3 Women N", "3 Women Percent")
# rownames(DF2) <- NULL
# DF2
# 
# # Event Counts by sex
# myfun3 <- function(x) {
#   DAT <- subset(fram, PERIOD == 1)
#   if(x == "TOTAL") {
#     X <- "TOTAL"
#     N <- matrix(table(DAT$SEX),  nrow = 1)
#     P <- 100*N/N
#   } else {
#     X <- Hmisc::label(DAT[[x]])
#     N <- table(DAT[[x]], DAT$SEX)
#     P <- 100*prop.table(N, margin = 2)
#   }
#   for(i in 1:nrow(P)) P[i,] <- MOTE::apa(as.numeric(P[i,]), decimals = 2)
#   return(cbind(X,
#                N[,1], P[,1],
#                N[,2], P[,2]))
# }
# 
# DF3 <- data.frame(rbind(myfun3("TOTAL"),
#                        myfun3("HYPERTEN"),
#                        myfun3("ANGINA"),
#                        myfun3("HOSPMI"),
#                        myfun3("MI_FCHD"),
#                        myfun3("STROKE"),
#                        myfun3("ANYCHD"),
#                        myfun3("CVD"),
#                        myfun3("DEATH")))
# names(DF3) <- c("Risk Factor",
#                "Men N",   "Men Percent", 
#                "Women N", "Women Percent")
# rownames(DF3) <- NULL
# DF3
# 
# # Distributions of Time to Event by sex
# myfun4 <- function(x, sex) {
#   DAT    <- subset(fram, SEX == sex & PERIOD == 1)
#   X      <- Hmisc::label(DAT[[x]])
#   N      <- sum(!is.na(DAT[[x]]))
#   NMISS  <- sum( is.na(DAT[[x]]))
#   MEAN   <- MOTE::apa(mean(    DAT[[x]], na.rm = T), decimals = 0)
#   SD     <- MOTE::apa(sd(      DAT[[x]], na.rm = T), decimals = 0)
#   MIN    <- MOTE::apa(min(     DAT[[x]], na.rm = T), decimals = 0)
#   P25    <- MOTE::apa(quantile(DAT[[x]], probs = 0.25, na.rm = T), decimals = 0)
#   MEDIAN <- MOTE::apa(median(  DAT[[x]], na.rm = T), decimals = 0)
#   P75    <- MOTE::apa(quantile(DAT[[x]], probs = 0.75, na.rm = T), decimals = 0)
#   MAX    <- MOTE::apa(max(     DAT[[x]], na.rm = T), decimals = 0)
#   return(c(X, N, NMISS, MEAN, SD, MIN, P25, MEDIAN, P75, MAX))
# }
# 
# wrapper4 <- function(sex) {
#   DF <- data.frame(rbind(myfun4("TIMEHYP", sex),
#                          myfun4("TIMEAP", sex),
#                          myfun4("TIMEMI", sex),
#                          myfun4("TIMEMIFC", sex),
#                          myfun4("TIMESTRK", sex),
#                          myfun4("TIMECHD", sex),
#                          myfun4("TIMECVD", sex),
#                          myfun4("TIMEDTH", sex)))
#   names(DF) <- c("Time to Event", "N", "NMiss", "Mean", "Std", "Min", "P25", "Median", "P75", "Max")
#   return(DF)
# }
# 
# wrapper4("Male")
# wrapper4("Female")

# Random subset of patients ####
# ID <- unique(fram$RANDID)
# set.seed(2903724)
# SUB <- sample(ID, 1000)
# fram_sub <- fram %>% 
#   filter(RANDID %in% SUB)
# nrow(fram)
# nrow(fram_sub)

# No subset - Use all the data
fram_sub <- fram

#---
# Create dataset containing time invariant variables only ####
#---

# For each type of event, we want to subset on those who were free of
# disease at baseline (see documentation)

# tmp <- fram_sub %>% 
#   filter(PERIOD == 1 & PREVHYP == "Prevalent disease") %>% 
#   select(RANDID)
# # tmp$RANDID[1]
# # fram_sub %>% 
# #   filter(RANDID == 10552) %>% 
# #   select(PERIOD, PREVHYP, HYPERTEN, TIMEHYP)
# fram_sub %>% 
#   right_join(tmp) %>%
#   select(PERIOD, PREVHYP, HYPERTEN, TIMEHYP) %>%
#   summary()
# # All of these people have PREVHYP and HYPERTEN at all times, and their times are all 0
# 
# tmp <- fram_sub %>% 
#   filter(PERIOD == 1 & PREVCHD == "Prevalent disease") %>% 
#   select(RANDID)
# fram_sub %>% 
#   right_join(tmp) %>%
#   select(PERIOD, PREVCHD, ANYCHD, TIMECHD) %>% 
#   summary()
# 
# tmp <- fram_sub %>% 
#   filter(PERIOD == 1 & PREVMI == "Prevalent disease") %>% 
#   select(RANDID)
# fram_sub %>% 
#   right_join(tmp) %>%
#   select(PERIOD, PREVMI, HOSPMI, TIMEMI) %>% 
#   summary()
# # This one is different. Some with prevalent MI at baseline have a time > 0
#   

fram_time_invar <- fram_sub %>%
  filter(PERIOD   == 1                 & 
         PREVAP   == "Free of disease" &
         PREVMI   == "Free of disease" &
         PREVCHD  == "Free of disease" &
         PREVSTRK == "Free of disease")

fram_time_invar %>% 
  select(starts_with("PREV"), starts_with("TIME")) %>% 
  summary()
# The above removed anyone who had prevalent CHD, angina, MI, or stroke at baseline
# All their times are > 0

# Set hypertension information to missing if prevalent hypertension
fram_time_invar$TIMEHYP[ fram_time_invar$PREVHYP == "Prevalent disease"] <- NA
fram_time_invar$HYPERTEN[fram_time_invar$PREVHYP == "Prevalent disease"] <- NA

fram_time_invar %>% 
  select(starts_with("PREV"), starts_with("TIME")) %>% 
  summary()
# OLD: Still some TIMEHYP = 0 (I fixed this above)
# fram_time_invar %>%
#   filter(TIMEHYP == 0) %>% 
#   select(RANDID) %>% 
#   left_join(fram_time_invar) %>% 
#   select(RANDID, PREVHYP, TIMEHYP, HYPERTEN)
# # Just one person. Seems like an anomaly (fixed above)

fram_time_invar <- fram_time_invar %>% 
  # Time-invariant variables, and baseline age
  select(RANDID,
         AGE, SEX, EDUC,
         ANGINA,   TIMEAP,
         HOSPMI,   TIMEMI,
         MI_FCHD,  TIMEMIFC,
         ANYCHD,   TIMECHD,
         STROKE,   TIMESTRK, 
         CVD,      TIMECVD, 
         HYPERTEN, TIMEHYP, 
         DEATH,    TIMEDTH)

dim(fram_time_invar)
head(fram_time_invar)

# Analyses of the time-invariant data exclude those with prevalent CHD, AP, MI, or stroke at baseline.
# Analyses of hypertension additionally exclude those with prevalent hypertension at baseline.

#---
# Full dataset ####
#---

fram_time_var <- fram_sub

#---
# Create counting process style datasets ####
# (START, STOP]
#---

# There are 3 examination periods
table(fram_time_var$PERIOD, exclude = NULL)

# Each person has from 1 to 3 rows
table(table(fram_time_var$RANDID))

# "For more complex analyses, such as time-dependent analysis, or a counting process style of
# input, the user would have to subset the population to those free of disease at all exams and
# event data would have to be modified to reflect when the event occurred relative to the
# examinations."
# https://biolincc.nhlbi.nih.gov/media/teachingstudies/FHS_Teaching_Longitudinal_Data_Documentation_2021a.pdf?link_time=2022-01-28_13:08:41.619750

# # View the example in the PDF
# fram_time_var %>%
#   filter(PREVCHD == "Free of disease" & RANDID %in% c(11263, 12629, 9069458)) %>% 
#   select(RANDID, AGE, SEX, PERIOD, TIME, MI_FCHD, TIMEMIFC)
# #    RANDID AGE    SEX PERIOD TIME MI_FCHD TIMEMIFC
# # 1   11263  43 Female      1    0       1     5719
# # 2   11263  49 Female      2 2178       1     5719
# # 3   11263  55 Female      3 4351       1     5719
# # 4   12629  63 Female      1    0       0     8766
# # 5 9069458  42 Female      1    0       0     8766
# # 6 9069458  54 Female      3 4362       0     8766
# 
# # GOAL:
# #  RANDID age SEX period time endtime newevnt mi_fchd timemifc
# #   11263  43   2      1    0    2178       0       1     5719
# #   11263  49   2      2 2178    4351       0       1     5719
# #   11263  55   2      3 4351    5719       1       1     5719
# #   12629  63   2      1    0    8766       0       0     8766
# # 9069458  42   2      1    0    4362       0       0     8766
# # 9069458  54   2      3 4362    8766       0       0     8766
# 
# # SAS code from
# # https://biolincc.nhlbi.nih.gov/media/teachingstudies/FHS_Teaching_Longitudinal_Data_Documentation_2021a.pdf?link_time=2022-01-28_13:08:41.619750
# #
# # data analysis; set work; if prevchd=0;
# # proc sort data=analysis; by randid descending period;
# # data analysis; set analysis; by randid;
# # newevnt=mi_fchd;
# # retain exmtime;
# # if first.randid then do; endtime=timemifc; exmtime=time; end;
# # else do;
# # newevnt=0; endtime=exmtime;exmtime=time;
# # end;
# # proc sort data=analysis; by randid period;run;
# 
# # Translate SAS code into R:
# 
# # This assumes the following:
# # RANDID is constant within person
# # PERIOD changes within person
# # AGE, SEX, the event indicator (MI_FCHD), and the event time (TIMEMIFC) are CONSTANT within person
# fram <- fram_time_var %>%
#   filter(PREVCHD == "Free of disease") %>% 
#   select(RANDID, AGE, SEX, PERIOD, TIME, MI_FCHD, TIMEMIFC) %>% 
#   group_by(RANDID) %>% 
#   # Use desc(PERIOD) so the first row is the final time and lagged times are later
#   arrange(RANDID, desc(PERIOD)) %>%
#   # https://stackoverflow.com/questions/13765834/r-equivalent-of-first-or-last-sas-operator
#   # is where I got the code for FIRST. and LAST.
#   # These are reversed since I used desc
#   mutate(LAST.PERIOD  = row_number() == min(row_number()),
#          FIRST.PERIOD = row_number() == max(row_number()), # FIRST.PERIOD not actually used
#          # At the last visit (LAST.PERIOD)
#          #   Set the event indicator to be the overall event indicator
#          #   Set the end time of the interval to be the event time (which is usually > last visit time)
#          # At earlier visits (!LAST.PERIOD)
#          #   Set the event indicator to be 0
#          #   Set the end time of the interval to be the time of the NEXT visit (lag, but in reverse order)
#          NEWEVNT      = case_when( LAST.PERIOD ~ as.numeric(MI_FCHD),
#                                    !LAST.PERIOD ~ 0),
#          ENDTIME      = case_when( LAST.PERIOD ~ as.numeric(TIMEMIFC),
#                                    !LAST.PERIOD ~ as.numeric(lag(TIME)))) %>% 
#   # Put back in PERIOD order
#   arrange(RANDID, PERIOD) %>% 
#   # Select variables to keep
#   select(RANDID, AGE, SEX, PERIOD, TIME, ENDTIME, NEWEVNT, MI_FCHD, TIMEMIFC)
# 
# # Compare to PDF example  
# fram %>%
#   filter(RANDID %in% c(11263, 12629, 9069458))
# # It worked!
# 
# # But what about the other time-dependent covariates? I think survival::tmerge might be better to use

library(survival)

# # Understandign how tmerge() works
# 
# # vignette("timedep", package="survival")
# # https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf
# # "In the survival routines time intervals are open on the left and closed on the right, i.e., (tstart, tstop].
# #  Time dependent covariates apply from the start of an interval and events occur at the end of an interval."
# 
# head(pbc) # Baseline data
# table(table(pbc$id)) # 1 row per subject
# head(pbcseq, 12) # Follow-up data
# table(table(pbcseq$id)) # 1-16 rows per subject
# 
# pbc[pbc$id == 1,]
# pbcseq[pbcseq$id == 1,]
# 
# pbc[pbc$id == 2,]
# pbcseq[pbcseq$id == 2,]
# 
# pbc[pbc$id == 5,]
# pbcseq[pbcseq$id == 5,]
# 
# # ?pbc
# # time: number of days between registration and the earlier of death, transplantation,
# #       or study analysis in July, 1986
# # status: status at endpoint, 0/1/2 for censored, transplant, dead
# 
# # ?pbcseq
# # futime = Total follow-up time (time-invariant)
# # day    = Days since baseline?
# 
# # For those who died, pbc$time = pbcseq$futime
# # For those who are censored? id = 2 is confusing...
# #   time = 4500
# #   futime = 5169
# #   day = 0 to 3226
# 
# # Step 1: Specify baseline dataset
# # id <= 312 because the follow-up is only for these
# temp <- subset(pbc, id <= 312, select=c(id:sex, stage)) # baseline
# head(temp) # 1 row per person
# 
# # Step 2: Initialize (tstart, tstop] and the event indicator (in this case, death)
# pbc2 <- tmerge(temp, temp, id=id, death = event(time, status)) #set range
# head(pbc2) # Still 1 row per person
# # What does death = event(time, status) do?
# # newname = event(y,x) Mark an event at time y.
# #                      In the usual case that x is missing the new 0/1 variable will be similar to the 0/1 status
# #                      variable of a survival time.
# # Since this is the baseline data and time is the event time, this step sets up
# # the full time range for each person, with the event indicator being the event at the end of the interval
# 
# # Step 3: Fill out the intervals using the longitudinal data, including all time-varying covariates
# #         New rows are added to the data at any time where a covariate changes.
# pbc2 <- tmerge(pbc2, pbcseq, id=id, ascites = tdc(day, ascites),
#                bili = tdc(day, bili), albumin = tdc(day, albumin),
#                protime = tdc(day, protime), alk.phos = tdc(day, alk.phos))
# head(pbc2)

# # Let's try this for Framingham using DEATH as the outcome
# 
# fram <- fram_time_var %>% 
#   filter(PREVCHD == "Free of disease")
# 
# # Create a baseline dataset
# # Step 1: Specify baseline dataset
# fram_base <- fram %>% 
#   filter(PERIOD == 1) %>%
#   select(RANDID, SEX, AGE, EDUC, DEATH, TIMEDTH)
# head(fram_base)
# 
# # Step 2: Initialize (tstart, tstop] and the event indicator
# fram2 <- tmerge(fram_base, fram_base, id=RANDID, DEATH = event(TIMEDTH, DEATH))
# head(fram2) # Still 1 row per person
# 
# # Step 3: Fill out the intervals using the longitudinal data, including all time-varying covariates
# #         New rows are added to the data at any time where a covariate changes.
# fram3 <- tmerge(fram2, fram,
#                 id = RANDID,
#                 PERIOD   = tdc(TIME, PERIOD),
#                 TOTCHOL  = tdc(TIME, TOTCHOL),
#                 HDLC     = tdc(TIME, HDLC),
#                 LDLC     = tdc(TIME, LDLC),
#                 SYSBP    = tdc(TIME, SYSBP),
#                 DIABP    = tdc(TIME, DIABP),
#                 CURSMOKE = tdc(TIME, CURSMOKE),
#                 CIGPDAY  = tdc(TIME, CIGPDAY),
#                 BMI      = tdc(TIME, BMI),
#                 DIABETES = tdc(TIME, DIABETES),
#                 BPMEDS   = tdc(TIME, BPMEDS),
#                 HEARTRTE = tdc(TIME, HEARTRTE),
#                 GLUCOSE  = tdc(TIME, GLUCOSE),
#                 PREVCHD  = tdc(TIME, PREVCHD),
#                 PREVAP   = tdc(TIME, PREVAP),
#                 PREVMI   = tdc(TIME, PREVMI),
#                 PREVSTRK = tdc(TIME, PREVSTRK),
#                 PREVHYP  = tdc(TIME, PREVHYP))
#                 
# head(fram3, 9)
# 
# 
# ALL.VARIABLES  <- names(fram3)
# TIME.INVARIANT <- names(nlme::gsummary(fram3, form = ~RANDID,
#                                        invariantsOnly = T))
# TIME.VARYING   <- ALL.VARIABLES[!(ALL.VARIABLES %in% TIME.INVARIANT)]
# TIME.INVARIANT
# # "RANDID"  "SEX"     "AGE"     "EDUC"    "TIMEDTH" "PREVCHD" "PREVAP"  "PREVMI"
# # (last 3 are always 0)
# summary(fram3[, TIME.INVARIANT])
# 
# TIME.VARYING
# # [1] "DEATH"    "tstart"   "tstop"    "PERIOD"   "TOTCHOL"  "HDLC"     "LDLC"     "SYSBP"    "DIABP"   
# # [10] "CURSMOKE" "CIGPDAY"  "BMI"      "DIABETES" "BPMEDS"   "HEARTRTE" "GLUCOSE"  "PREVSTRK" "PREVHYP" 
# 
# summary(fram3)

# fram3_death = fram3
# For comparison

# Function

# EVENT = "HYPERTEN"
# TIMEVAR = "TIMEHYP"

StartStop <- function(EVENT, TIMEVAR) {
  # Subset the population to those free of disease at all exams
  fram <- fram_time_var %>% 
    filter(PREVCHD == "Free of disease" & PREVSTRK  == "Free of disease")

  if(EVENT == "HYPERTEN") {
    fram <- fram %>% 
      filter(PREVHYP == "Free of disease")
  }

  # Assign generic names
  fram$event   <- fram[[EVENT]]
  fram$timevar <- fram[[TIMEVAR]]

  # Step 1: Specify baseline dataset
  fram_base <- fram %>% 
    filter(PERIOD == 1) %>%
    select(RANDID, SEX, AGE, EDUC, event, timevar)
  # head(fram_base)
  
  # Step 2: Initialize (tstart, tstop] and the event indicator
  fram2 <- tmerge(fram_base, fram_base, id=RANDID, event = event(timevar, event))
  # head(fram2) # Still 1 row per person
  
  # Step 3: Fill out the intervals using the longitudinal data, including all time-varying covariates
  #         New rows are added to the data at any time where a covariate changes.
  fram3 <- tmerge(fram2, fram,
                  id       = RANDID,
                  PERIOD   = tdc(TIME, PERIOD),
                  TOTCHOL  = tdc(TIME, TOTCHOL),
                  HDLC     = tdc(TIME, HDLC),
                  LDLC     = tdc(TIME, LDLC),
                  SYSBP    = tdc(TIME, SYSBP),
                  DIABP    = tdc(TIME, DIABP),
                  CURSMOKE = tdc(TIME, CURSMOKE),
                  CIGPDAY  = tdc(TIME, CIGPDAY),
                  BMI      = tdc(TIME, BMI),
                  DIABETES = tdc(TIME, DIABETES),
                  BPMEDS   = tdc(TIME, BPMEDS),
                  HEARTRTE = tdc(TIME, HEARTRTE),
                  GLUCOSE  = tdc(TIME, GLUCOSE),
                  # The following are all 0 due to the filtering
                  # PREVCHD  = tdc(TIME, PREVCHD),
                  # PREVAP   = tdc(TIME, PREVAP),
                  # PREVMI   = tdc(TIME, PREVMI),
                  # PREVSTRK = tdc(TIME, PREVSTRK),
                  PREVHYP  = tdc(TIME, PREVHYP))
  
  fram4 <- fram3 %>% 
    as.data.frame()

  # Put original names back
  names(fram4)[which(names(fram4) == "event")]   <- EVENT
  names(fram4)[which(names(fram4) == "timevar")] <- TIMEVAR
  
  return(fram4)
}

fram_tv_angina   <- StartStop("ANGINA",   "TIMEAP")
fram_tv_mi       <- StartStop("HOSPMI",   "TIMEMI")
fram_tv_mi_fchd  <- StartStop("MI_FCHD",  "TIMEMIFC")
fram_tv_anychd   <- StartStop("ANYCHD",   "TIMECHD")
fram_tv_stroke   <- StartStop("STROKE",   "TIMESTRK")
fram_tv_cvd      <- StartStop("CVD",      "TIMECVD")
fram_tv_hyperten <- StartStop("HYPERTEN", "TIMEHYP")
fram_tv_death    <- StartStop("DEATH",    "TIMEDTH")

# # CHECK
# # Analysis using time-invariant should be the same as an analysis using (START, STOP] if no tv-covariates
# 
# fit1 <- coxph(Surv(TIMEAP, ANGINA) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, ANGINA) ~ SEX + AGE, data = fram_tv_angina)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p
# 
# fit1 <- coxph(Surv(TIMEMI, HOSPMI) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, HOSPMI) ~ SEX + AGE, data = fram_tv_mi)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p
# 
# fit1 <- coxph(Surv(TIMEMIFC, MI_FCHD) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, MI_FCHD) ~ SEX + AGE, data = fram_tv_mi_fchd)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p
# 
# fit1 <- coxph(Surv(TIMECHD, ANYCHD) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, ANYCHD) ~ SEX + AGE, data = fram_tv_anychd)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p
# 
# fit1 <- coxph(Surv(TIMECVD, CVD) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, CVD) ~ SEX + AGE, data = fram_tv_cvd)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p
# 
# fit1 <- coxph(Surv(TIMEHYP, HYPERTEN) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, HYPERTEN) ~ SEX + AGE, data = fram_tv_hyperten)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p
# 
# fit1 <- coxph(Surv(TIMEDTH, DEATH) ~ SEX + AGE, data = fram_time_invar)
# fit2 <- coxph(Surv(tstart, tstop, DEATH) ~ SEX + AGE, data = fram_tv_death)
# rbind(cbind(exp(summary(fit1)$coef[, "coef"]), # HR
#             exp(confint(fit1)),                # CI
#             summary(fit1)$coef[, "Pr(>|z|)"]),  # p
#       cbind(exp(summary(fit2)$coef[, "coef"]), # HR
#             exp(confint(fit2)),                # CI
#             summary(fit2)$coef[, "Pr(>|z|)"]))  # p

#---
# Save data ####
#---

save(fram_time_invar,  file = "fram_time_invar_rmph.rData")
save(fram_tv_angina,   file = "fram_tv_angina_rmph.rData")
save(fram_tv_mi,       file = "fram_tv_mi_rmph.rData")
save(fram_tv_mi_fchd,  file = "fram_tv_mi_fchd_rmph.rData")
save(fram_tv_anychd,   file = "fram_tv_anychd_rmph.rData")
save(fram_tv_stroke,   file = "fram_tv_stroke_rmph.rData")
save(fram_tv_cvd,      file = "fram_tv_cvd_rmph.rData")
save(fram_tv_hyperten, file = "fram_tv_hyperten_rmph.rData")
save(fram_tv_death,    file = "fram_tv_death_rmph.rData")

#---
# Summarize data ####
#---

# load("fram_time_invar_rmph.rData")
# load("fram_tv_angina_rmph.rData")
# load("fram_tv_mi_rmph.rData")
# load("fram_tv_mi_fchd_rmph.rData")
# load("fram_tv_anychd_rmph.rData")
# load("fram_tv_stroke_rmph.rData")
# load("fram_tv_cvd_rmph.rData")
# load("fram_tv_hyperten_rmph.rData")
# load("fram_tv_death_rmph.rData")

write(Hmisc::html(Hmisc::describe(fram_time_invar)),  "fram_time_invar_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_angina)),   "fram_tv_angina_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_mi)),       "fram_tv_mi_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_mi_fchd)),  "fram_tv_mi_fchd_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_anychd)),   "fram_tv_anychd_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_stroke)),   "fram_tv_stroke_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_cvd)),      "fram_tv_cvd_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_hyperten)), "fram_tv_hyperten_rmph.html")
write(Hmisc::html(Hmisc::describe(fram_tv_death)),    "fram_tv_death_rmph.html")

# View the HTML file to examine the distribution of each variable
