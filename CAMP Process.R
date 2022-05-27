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
# Program Name:    CAMP Process.R
# Analyst:         Ramzi W. Nahhas
# Date:            September 15, 2021
# Contents:        Process CAMP dataset
#---

# Downloaded 9/10/2021
# https://biolincc.nhlbi.nih.gov/teaching/
# https://biolincc.nhlbi.nih.gov/requests/teaching-dataset-request/
  
#---
# Load data ####
#---

camp0 <- read.csv("camp_teach.csv", na.strings = c("", " "), header = TRUE, stringsAsFactors = F, as.is = T)
head(camp0)

library(tidyverse)
# Create factors
camp <- camp0 %>% 
  mutate(TX = factor(TX,
                     levels = c("bud", "ned", "pbud", "pned"),
                     labels = c("Budesonide", "Nedocromil", "Placebo Budesonide", "Placebo Nedocromil")),
         TG = factor(TG,
                     levels = c("A", "B", "C"),
                     labels = c("Budesonide", "Nedocromil", "Placebo")),
         GENDER = factor(GENDER,
                         levels = c("m", "f"),
                         labels = c("Male", "Female")),
         ETHNIC = factor(ETHNIC,
                         levels = c("b", "h", "o", "w"),
                         labels = c("Black", "Hispanic", "Other", "White")),
         anypet = factor(anypet,
                         levels = c(2, 1),
                         labels = c("No", "Yes")),
         woodstove = factor(woodstove,
                            levels = c(2, 1),
                            labels = c("No", "Yes")),
         dehumid = factor(dehumid,
                          levels = c(2, 1),
                          labels = c("No", "Yes")),
         parent_smokes = factor(parent_smokes,
                                levels = c(2, 1),
                                labels = c("No", "Yes")),
         any_smokes = factor(any_smokes,
                             levels = c(2, 1),
                             labels = c("No", "Yes")))

# # Check
# table(camp0$TX, camp$TX, exclude = NULL)
# table(camp0$TG, camp$TG, exclude = NULL)
# table(camp0$GENDER, camp$GENDER, exclude = NULL)
# table(camp0$ETHNIC, camp$ETHNIC, exclude = NULL)
# table(camp0$anypet, camp$anypet, exclude = NULL)
# table(camp0$woodstove, camp$woodstove, exclude = NULL)
# table(camp0$dehumid, camp$dehumid, exclude = NULL)
# table(camp0$parent_smokes, camp$parent_smokes, exclude = NULL)
# table(camp0$any_smokes, camp$any_smokes, exclude = NULL)
# 
# nrow(camp) # 9947
# length(unique(camp$id)) # 695
# # Longitudinal data in long form
# table(camp$fdays) # Unique times
# summary(camp$fdays)
# max(camp$fdays)/365.25 # Up to 10 years post-randomization
# 
# table(camp$visitc) # Month of visit is discrete (0 to 120)
# 
# table(table(camp$id)) # 1 to 18 visits each
# 
# summary(camp)
# # Why so many missing for the Yes/No variables?
# table(camp$visitc, camp$anypet, exclude = NULL)
# # Only at 0, 12, 24, 36, and 48 months
# 
# # Wny so many missing hemoglobin and wbc?
# tapply(camp$hemog, camp$visitc, range, na.rm=T)
# tapply(camp$wbc, camp$visitc, range, na.rm=T)
# # Only at 0 and 48 months
# 
# summary(camp[camp$visitc %in% c(0, 12, 24, 36, 48),])
# summary(camp[camp$visitc %in% c(0, 48),])

# Apply labels (see CAMP_Teaching_Documentation.pdf)
{
  Hmisc::label(camp$TX) <- "Treatment group (4 levels)"
  Hmisc::label(camp$TG) <- "Treatment group"
  Hmisc::label(camp$id) <- "Randomized participant ID"
  Hmisc::label(camp$age_rz) <- "Age in years at Randomization"
  Hmisc::label(camp$GENDER) <- "Gender"
  Hmisc::label(camp$ETHNIC) <- "Race/Ethnicity"
  Hmisc::label(camp$hemog) <- "Hemoglobin (g/dl)"
  Hmisc::label(camp$PREFEV) <- "Pre-bronchodilator FEV1"
  Hmisc::label(camp$PREFVC) <- "Pre-bronchodilator FVC"
  Hmisc::label(camp$PREFF) <- "Pre-bronchodilator FEV1/FVC ratio %"
  Hmisc::label(camp$PREPF) <- "Pre-bronchodilator peak flow"
  Hmisc::label(camp$POSFEV) <- "Post-bronchodilator FEV1"
  Hmisc::label(camp$POSFVC) <- "Post-bronchodilator FVC"
  Hmisc::label(camp$POSFF) <- "Post-bronchodilator FEV1/FVC ratio %"
  Hmisc::label(camp$POSPF) <- "Post-bronchodilator peak flow"
  Hmisc::label(camp$PREFEVPP) <- "Pre-bronchodilator FEV1 %pred"
  Hmisc::label(camp$PREFVCPP) <- "Pre-bronchodilator FVC %pred"
  Hmisc::label(camp$POSFEVPP) <- "Post-bronchodilator FEV1 %pred"
  Hmisc::label(camp$POSFVCPP) <- "Post-bronchodilator FVC %pred"
  Hmisc::label(camp$wbc) <- "White Blood Cell count (1000 cells/ul)"
  Hmisc::label(camp$agehome) <- "Age of current home (years)"
  Hmisc::label(camp$anypet) <- "Any pets"
  Hmisc::label(camp$woodstove) <- "Used wood stove for heating/cooking"
  Hmisc::label(camp$dehumid) <- "Use a dehumidifier"
  Hmisc::label(camp$parent_smokes) <- "Either Parent/partner smokes in home"
  Hmisc::label(camp$any_smokes) <- "Anyone (including visitors) smokes in home"
  Hmisc::label(camp$visitc) <- "Followup Visit (mos)"
  Hmisc::label(camp$fdays) <- "Days since randomization"
}

#---
# Summarize data ####
#---

# Compare to tables in CAMP_Teaching_Documentation.pdf

# myfun <- function(x) {
#   N      <- MOTE::apa(sum(!is.na(x)), decimals = 0)
#   MEAN   <- MOTE::apa(mean(  x, na.rm = T), decimals = 2)
#   SD     <- MOTE::apa(sd(    x, na.rm = T), decimals = 3)
#   MEDIAN <- MOTE::apa(median(x, na.rm = T), decimals = 2)
#   return(c(N, MEAN, SD, MEDIAN))
# }
# 
# # Compare to tables in the PDF
# wrapper <- function(X) {
#   DAT    <- camp[camp$visitc == 0,]
#   XA     <- DAT[[X]][DAT$TG == "Budesonide"]
#   XB     <- DAT[[X]][DAT$TG == "Nedocromil"]
#   XC     <- DAT[[X]][DAT$TG == "Placebo"]
#   c(Hmisc::label(camp[[X]]), myfun(XA), myfun(XB), myfun(XC))
# }
# 
# rbind(wrapper("age_rz"),
#       wrapper("hemog"),
#       wrapper("wbc"),
#       wrapper("agehome"),
#       wrapper("PREFEV"),
#       wrapper("PREFVC"),
#       wrapper("PREFF"),
#       wrapper("PREPF"),
#       wrapper("POSFEV"),
#       wrapper("POSFVC"),
#       wrapper("POSFF"),
#       wrapper("POSPF"),
#       wrapper("PREFEVPP"),
#       wrapper("PREFVCPP"),
#       wrapper("POSFEVPP"),
#       wrapper("POSFVCPP"))
# 
# myfun2 <- function(X, KEEP = T) {
#   DAT    <- camp[camp$visitc == 0,]
#   print(Hmisc::label(camp[[X]]))
#   print(round(100*prop.table(table(DAT[[X]], DAT$TG), margin = 2), 1)[KEEP, , drop = F])
# }
# 
# myfun2("GENDER", 2)
# myfun2("ETHNIC")
# myfun2("anypet", 2)
# myfun2("woodstove", 2)
# myfun2("dehumid", 2) # Does not match... Probably because I set DK to missing so that changed the denominator
# myfun2("parent_smokes", 2)
# myfun2("any_smokes", 2)
# 

#---
# Create wide-form dataset just using 0 and 48 month data ####
#---

# Select pre and post bronchodilator FEV1 at 48 months
camp48 <- camp %>% 
  filter(visitc == 48) %>% 
  rename(PREFEV48 = PREFEV,
         POSFEV48 = POSFEV) %>% 
  select(id, PREFEV48, POSFEV48)

# Merge with baseline data
camp_0_48 <- camp %>% 
  filter(visitc == 0) %>% 
  rename(PREFEV0 = PREFEV,
         POSFEV0 = POSFEV) %>% 
  select(id, TG, age_rz, GENDER, ETHNIC, agehome, anypet, woodstove, dehumid, parent_smokes, any_smokes, PREFEV0, POSFEV0) %>% 
  inner_join(camp48)

dim(camp_0_48)

summary(camp_0_48)

write(Hmisc::html(Hmisc::describe(camp_0_48)), "camp_0_48.html")
# View the HTML file to examine the distribution of each variable

#---
# Save data ####
#---

save(camp_0_48, file = "camp_0_48_rmph.rData")

# NOTE: The "main study" is visitc = 0. The outcome is post-BD FEV1. The "48" variables are from the 48 month
#        follow-up data. So the main study comparison is POSFEV0 vs. TG.



