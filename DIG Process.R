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
# Program Name:    DIG Process.R
# Analyst:         Ramzi W. Nahhas
# Date:            September 15, 2021
# Contents:        Process Digitalis dataset
#---

# Downloaded 9/10/2021
# https://biolincc.nhlbi.nih.gov/teaching/
# https://biolincc.nhlbi.nih.gov/requests/teaching-dataset-request/
  
#---
# Load data ####
#---

dig0 <- read.csv("DIG.csv", na.strings = c("", " "), header = TRUE, stringsAsFactors = F, as.is = T)
head(dig0)

library(tidyverse)
# Create factors
dig <- dig0 %>% 
  mutate(TRTMT    = factor(TRTMT, levels = 0:1, labels = c("Placebo", "Digoxin")),
         RACE     = factor(RACE,  levels = 1:2, labels = c("Non-minority", "Minority")),
         SEX      = factor(SEX,   levels = 1:2, labels = c("Male", "Female")),
         DEATH    = factor(DEATH, levels = 0:1, labels = c("Alive", "Death")),
         EJFMETH  = factor(EJFMETH,
                           levels = 1:3,
                           labels = c("Radionuclide", "Angiography", "2-D Echo")),
         FUNCTCLS = factor(FUNCTCLS,
                           levels = 1:4,
                           labels = paste("Class", c("I", "II", "III", "IV"))),
         chfcause = case_when(CHFETIOL == 1 ~ 1,
                              CHFETIOL == 2 ~ 2,
                              CHFETIOL == 4 ~ 4,
                              CHFETIOL %in% c(3, 5, 6) ~ 7),
         chfcause = factor(chfcause,
                           levels = c(1, 2, 4, 7),
                           labels = c("Ischemic", "Hypertensive", "Idiopathic", "Other")),
         REASON = factor(REASON,
                         levels = 1:5,
                         labels = c("Worsening Heart Failure", "Other Cardiac", "Other Vascular", "Unknown", "Noncardiac, nonvascular")),
         diuretic = case_when(DIURETK == 1 | DIURET == 1 ~ 1,
                              DIURETK == 0 & DIURET == 0 ~ 0),
         diuretic = factor(diuretic, levels = 0:1, labels = c("No", "Yes")),
         PREVMI   = factor(PREVMI,   levels = 0:1, labels = c("No", "Yes")),
         ANGINA   = factor(ANGINA,   levels = 0:1, labels = c("No", "Yes")),
         DIABETES = factor(DIABETES, levels = 0:1, labels = c("No", "Yes")),
         HYPERTEN = factor(HYPERTEN, levels = 0:1, labels = c("No", "Yes")),
         DIGUSE   = factor(DIGUSE,   levels = 0:1, labels = c("No", "Yes")),
         ACEINHIB = factor(ACEINHIB, levels = 0:1, labels = c("No", "Yes")),
         NITRATES = factor(NITRATES, levels = 0:1, labels = c("No", "Yes")),
         VASOD    = factor(VASOD,    levels = 0:1, labels = c("No", "Yes")),
         CVD      = factor(CVD,   levels = 0:1, labels = c("No Event", "First Event")),
         WHF      = factor(WHF,   levels = 0:1, labels = c("No Event", "First Event")),
         VENA     = factor(VENA,  levels = 0:1, labels = c("No Event", "First Event")),
         SVA      = factor(SVA,   levels = 0:1, labels = c("No Event", "First Event")),
         DIG      = factor(DIG,   levels = 0:1, labels = c("No Event", "First Event")),
         MI       = factor(MI,    levels = 0:1, labels = c("No Event", "First Event")),
         UANG     = factor(UANG,  levels = 0:1, labels = c("No Event", "First Event")),
         STRK     = factor(STRK,  levels = 0:1, labels = c("No Event", "First Event")),
         CREV     = factor(CREV,  levels = 0:1, labels = c("No Event", "First Event")),
         RINF     = factor(RINF,  levels = 0:1, labels = c("No Event", "First Event")),
         OTH      = factor(OTH,   levels = 0:1, labels = c("No Event", "First Event")),
         HOSP     = factor(HOSP,  levels = 0:1, labels = c("No Event", "First Event")),
         RALES    = factor(RALES,    levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         ELEVJVP  = factor(ELEVJVP,  levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         PEDEMA   = factor(PEDEMA,   levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         RESTDYS  = factor(RESTDYS,  levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         EXERTDYS = factor(EXERTDYS, levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         ACTLIMIT = factor(ACTLIMIT, levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         S3       = factor(S3,       levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past")),
         PULCONG  = factor(PULCONG,  levels = 0:3, labels = c("None or Unknown", "Present", "Past", "Present and Past"))) %>% 
  select(-CHFETIOL, -DIURET, -DIURETK)
         
# # Check
# table(dig0$TRTMT, dig$TRTMT, exclude = NULL)
# table(dig0$RACE, dig$RACE, exclude = NULL)
# table(dig0$SEX, dig$SEX, exclude = NULL)
# table(dig0$DEATH, dig$DEATH, exclude = NULL)
# table(dig0$EJFMETH, dig$EJFMETH, exclude = NULL)
# table(dig0$FUNCTCLS, dig$FUNCTCLS, exclude = NULL)
# table(dig0$CHFETIOL, dig$chfcause, exclude = NULL)
# table(dig0$REASON, dig$REASON, exclude = NULL)
# table(dig0$DIURET[dig$diuretic=="Yes"], dig$DIURETK[dig$diuretic=="Yes"], exclude = NULL)
# table(dig0$DIURET[dig$diuretic=="No"],  dig$DIURETK[dig$diuretic=="No"],  exclude = NULL)
# table(dig0$PREVMI, dig$PREVMI, exclude = NULL)
# table(dig0$ANGINA, dig$ANGINA, exclude = NULL)
# table(dig0$DIABETES, dig$DIABETES, exclude = NULL)
# table(dig0$HYPERTEN, dig$HYPERTEN, exclude = NULL)
# table(dig0$DIGUSE, dig$DIGUSE, exclude = NULL)
# table(dig0$ACEINHIB, dig$ACEINHIB, exclude = NULL)
# table(dig0$NITRATES, dig$NITRATES, exclude = NULL)
# table(dig0$VASOD, dig$VASOD, exclude = NULL)
# table(dig0$CVD, dig$CVD, exclude = NULL)
# table(dig0$WHF, dig$WHF, exclude = NULL)
# table(dig0$VENA, dig$VENA, exclude = NULL)
# table(dig0$SVA, dig$SVA, exclude = NULL)
# table(dig0$DIG, dig$DIG, exclude = NULL)
# table(dig0$MI, dig$MI, exclude = NULL)
# table(dig0$UANG, dig$UANG, exclude = NULL)
# table(dig0$STRK, dig$STRK, exclude = NULL)
# table(dig0$CREV, dig$CREV, exclude = NULL)
# table(dig0$RINF, dig$RINF, exclude = NULL)
# table(dig0$OTH, dig$OTH, exclude = NULL)
# table(dig0$HOSP, dig$HOSP, exclude = NULL)

nrow(dig) # 6800
length(unique(dig$ID)) # 6800

# Apply labels (see DIG Documentation with supplement.pdf)
{
  Hmisc::label(dig$ID) <- "Patient ID"
  Hmisc::label(dig$TRTMT) <- "Treatment Group"
  Hmisc::label(dig$AGE) <- "Calculated: age at randomization"
  Hmisc::label(dig$RACE) <- "Q5: Race"
  Hmisc::label(dig$SEX) <- "Q4: Sex"
  Hmisc::label(dig$EJF_PER) <- "Q3: Ejection fraction (percent)"
  Hmisc::label(dig$EJFMETH) <- "Q3A: Ejection Fraction method"
  Hmisc::label(dig$CHESTX) <- "Q6: Chest X-ray (CT-Ratio)"
  Hmisc::label(dig$BMI) <- "Calculated: Body Mass Index (kg/M*M)"
  Hmisc::label(dig$KLEVEL) <- "Q9A: Serum Potassium level"
  Hmisc::label(dig$CREAT) <- "Q9: Serum Creatinine (mg/dL)"
  Hmisc::label(dig$DIGDOSER) <- "Q10: Recommended Digoxin dose"
  Hmisc::label(dig$CHFDUR) <- "Q12: Duration of CHF (months)"
  Hmisc::label(dig$RALES) <- "Q13: Rales"
  Hmisc::label(dig$ELEVJVP) <- "Q14: Elevated jugular venous pressure"
  Hmisc::label(dig$PEDEMA) <- "Q15: Peripheral Edema"
  Hmisc::label(dig$RESTDYS) <- "Q16: Dyspnea at Rest"
  Hmisc::label(dig$EXERTDYS) <- "Q17: Dyspnea on Exertion"
  Hmisc::label(dig$ACTLIMIT) <- "Q18: Limitation of activity"
  Hmisc::label(dig$S3) <- "Q19: S3 Gallop"
  Hmisc::label(dig$PULCONG) <- "Q20: Pulmonary congestion"
  Hmisc::label(dig$NSYM) <- "Calculated: Sum of Q13-Q20, Y/N status"
  Hmisc::label(dig$HEARTRTE) <- "Q21: Heart Rate (beats/min)"
  Hmisc::label(dig$DIABP) <- "Q22: Diastolic BP (mmHg)"
  Hmisc::label(dig$SYSBP) <- "Q22: Sysolic BP (mmHg)"
  Hmisc::label(dig$FUNCTCLS) <- "Q23: NYHA Functional Class"
  Hmisc::label(dig$chfcause) <- "Q24: CHF Etiology"
  Hmisc::label(dig$PREVMI) <- "Q25: Previous Myocardial Infarction"
  Hmisc::label(dig$ANGINA) <- "Q26: Current Angina"
  Hmisc::label(dig$DIABETES) <- "Q27: History of Diabetes"
  Hmisc::label(dig$HYPERTEN) <- "Q28: History of Hypertension"
  Hmisc::label(dig$DIGUSE) <- "Q29: Digoxin within past week"
  # Hmisc::label(dig$DIURETK) <- "Q30: Potassium sparing Diuretics"
  # Hmisc::label(dig$DIURET) <- "Q31: Other Diuretics"
  Hmisc::label(dig$KSUPP) <- "Q31A: Potassium supplements"
  Hmisc::label(dig$ACEINHIB) <- "Q32: Ace inhibitors"
  Hmisc::label(dig$NITRATES) <- "Q33: Nitrates"
  Hmisc::label(dig$HYDRAL) <- "Q34: Hydralazine"
  Hmisc::label(dig$VASOD) <- "Q35: Other Vasodilators"
  Hmisc::label(dig$DIGDOSE) <- "Q36: Dose of Digoxin/Placebo prescribed"
  Hmisc::label(dig$CVD) <- "Hosp: Cardiovascular Disease"
  Hmisc::label(dig$CVDDAYS) <- "Days randomization to First CVD Hosp"
  Hmisc::label(dig$WHF) <- "Hosp: Worsening Heart Failure"
  Hmisc::label(dig$WHFDAYS) <- "Days randomization to First WHF Hosp"
  Hmisc::label(dig$DIG) <- "Hosp: Digoxin Toxicity"
  Hmisc::label(dig$DIGDAYS) <- "Days rand. to First Digoxin Tox Hosp"
  Hmisc::label(dig$MI) <- "Hosp: Myocardial Infarction"
  Hmisc::label(dig$MIDAYS) <- "Days randomization to First MI Hosp"
  Hmisc::label(dig$UANG) <- "Hosp: Unstable Angina"
  Hmisc::label(dig$UANGDAYS) <- "Days rand. to First Unstable Angina Hosp"
  Hmisc::label(dig$STRK) <- "Hosp: Stroke"
  Hmisc::label(dig$STRKDAYS) <- "Days randomization to First Stroke Hosp"
  Hmisc::label(dig$SVA) <- "Hosp: Supraventricular Arrhythmia"
  Hmisc::label(dig$SVADAYS) <- "Days rand. to First SupraVent Arr. Hosp"
  Hmisc::label(dig$VENA) <- "Hosp: Ventricular Arrhythmia"
  Hmisc::label(dig$VENADAYS) <- "Days rand. to First Vent. Arr. Hosp"
  Hmisc::label(dig$CREV) <- "Hosp: Coronary Revascularization"
  Hmisc::label(dig$CREVDAYS) <- "Days rand. to First Cor. Revasc."
  Hmisc::label(dig$OCVD) <- "Hosp: Other Cardiovascular Event"
  Hmisc::label(dig$OCVDDAYS) <- "Days rand. to First Other CVD Hosp"
  Hmisc::label(dig$RINF) <- "Hosp: Respiratory Infection"
  Hmisc::label(dig$RINFDAYS) <- "Days rand. to First Resp. Infection Hosp"
  Hmisc::label(dig$OTH) <- "Hosp: Other noncardiac, nonvascular"
  Hmisc::label(dig$OTHDAYS) <- "Days rand. to 1st Other Non CVD Hosp"
  Hmisc::label(dig$HOSP) <- "Hosp: Any Hospitalization"
  Hmisc::label(dig$HOSPDAYS) <- "Days randomization to First Any Hosp"
  Hmisc::label(dig$NHOSP) <- "Number of Hospitalizations"
  Hmisc::label(dig$DEATH) <- "Vital Status of Patient"
  Hmisc::label(dig$DEATHDAY) <- "Days till last followup or death"
  Hmisc::label(dig$REASON) <- "Cause of Death"
  Hmisc::label(dig$DWHF) <- "Primary Endpt: Death or Hosp from worsening HF"
  Hmisc::label(dig$DWHFDAYS) <- "Days rand. to death or Hosp from worsening HF"
}

# tapply(dig$MIDAYS, dig$MI, range, na.rm=T)
# hist(dig$MIDAYS[dig$MI == "No Event"])
# hist(dig$MIDAYS[dig$MI == "First Event"])
# # "If no event occurred, number of days is from randomization until l date of last contact or date of death"
# # (DIG Documentation with supplement.pdf)

#---
# Summarize data ####
#---

# Compare to tables in DIG Documentation with supplement.pdf

# myfun <- function(x) {
#   N      <- MOTE::apa(sum(!is.na(x)), decimals = 0)
#   MEAN   <- MOTE::apa(mean(  x, na.rm = T), decimals = 2)
#   MEDIAN <- MOTE::apa(median(x, na.rm = T), decimals = 1)
#   SD     <- MOTE::apa(sd(    x, na.rm = T), decimals = 2)
#   return(c(N, MEAN, MEDIAN, SD))
# }
# 
# # Compare to tables in the PDF
# wrapper <- function(X) {
#   X0 <- dig[[X]][dig$TRTMT == "Placebo"]
#   X1 <- dig[[X]][dig$TRTMT == "Digoxin"]
#   c(Hmisc::label(dig[[X]]), myfun(X0), myfun(X1))
# }
# 
# rbind(wrapper("AGE"),
#       wrapper("SYSBP"),
#       wrapper("DIABP"),
#       wrapper("CREAT"),
#       wrapper("KLEVEL"),
#       wrapper("CHESTX"),
#       wrapper("CHFDUR"),
#       wrapper("EJF_PER"))
# 
# myfun2 <- function(X, KEEP = T) {
#   print(Hmisc::label(dig[[X]]))
#   print(round(100*prop.table(table(dig[[X]], dig$TRTMT), margin = 2), 1)[KEEP, , drop = F])
# }
# 
# myfun2("SEX")
# myfun2("RACE")
# myfun2("FUNCTCLS")
# myfun2("PREVMI")
# myfun2("ANGINA")
# myfun2("DIABETES")
# # Etc.
# 
# write(Hmisc::html(Hmisc::describe(dig)), "dig.html")
# # View the HTML file to examine the distribution of each variable

#---
# Save data ####
#---

save(dig, file = "dig_rmph.rData")





