# NOTE: This code is provided solely to create example datasets used in Introduction to Regression Methods for Public Health using R
#       No warranty is provided regarding the correctness of this code or regarding any analyses carried out with the
#       datasets produced by this code.

#---
## Program Name:    NSDUH_2019 MI Simulation.R
## Analyst:         Ramzi W. Nahhas
## Date:            March 15, 2022
## Contents:        Create missing data example using NSDUH
##                  For use in binary logistic regression with MI
##                  Also to illustrate how complete case analysis (CC) and MI perform
##                      with MCAR, MAR, and MNAR data.
#---

#---
# Load data ####
#---

# load("nsduh2019_adult_sub_rmph.RData")
load("Data/nsduh2019_adult_sub_rmph.RData")

# install.packages("tidyverse", "Hmisc")
library(tidyverse)
library(mice)

logit  <- function(p) log(p/(1-p))
ilogit <- function(x) exp(x)/(1+exp(x))

# For debugging
# NSIM   = 5
# p_mcar = 0.05
# p_ref  = 0.005
# b_mj   = 2.5
# b_alc  = 0.08

mysim <- function(NSIM,
                  p_mcar,
                  p_ref,
                  b_mj,
                  b_alc) {
  
  # Create 3 different missing data datasets from NSDUH
  # Among these variables, only alc_agefirst is missing in the subsample
  # Drop those cases. We do not want to impute a first age for those 
  # who never drank.
  nsduh_0 <- nsduh_adult_sub %>% 
    select(mj_lifetime, alc_agefirst, demog_age_cat6, demog_sex, demog_income) %>% 
    drop_na()
  nrow(nsduh_0) # 843
  
  # Fit to full data
  fit_0 <- glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
               family = binomial, data=nsduh_0)
  
  # Betas
  beta_0        <- data.frame(summary(fit_0)$coef[-1, c("Estimate", "Std. Error")])
  names(beta_0) <- c("beta", "se_beta")

  # Initialize
  beta_mcar_cc <- beta_mar_cc <- beta_mnar_cc <- array(NA, dim = c(nrow(beta_0), ncol(beta_0), NSIM))
  beta_mcar_mi <- beta_mar_mi <- beta_mnar_mi <- array(NA, dim = c(nrow(beta_0), ncol(beta_0), NSIM))
  NMISS_mcar   <- NMISS_mar   <- NMISS_mnar   <- rep(NA, NSIM)
  
  # MCAR - Randomly set 100*p_mcar % of each variable to missing
  # MAR  - Set to missing based on a model for each variable given the others
  # MNAR - Missingness also depends on the variable itself
  
  # (this is not a perfect illustration... the MAR models actually make
  #  use of missing values, just not in the variable being imputed. This
  #  makes setting up the simulation much simpler and serves to illustrate the point)
  
  MJ  <-  b_mj*as.numeric(nsduh_0$mj_lifetime == levels(nsduh_0$mj_lifetime)[2])
  ALC <-  b_alc*(nsduh_0$alc_agefirst - mean(nsduh_0$alc_agefirst))
  AGE <-  0.20*as.numeric(nsduh_0$demog_age_cat6 == levels(nsduh_0$demog_age_cat6)[2]) +
          0.40*as.numeric(nsduh_0$demog_age_cat6 == levels(nsduh_0$demog_age_cat6)[3]) +
          0.60*as.numeric(nsduh_0$demog_age_cat6 == levels(nsduh_0$demog_age_cat6)[4]) +
          0.80*as.numeric(nsduh_0$demog_age_cat6 == levels(nsduh_0$demog_age_cat6)[5])
  SEX <- -0.60*as.numeric(nsduh_0$demog_sex == levels(nsduh_0$demog_sex)[2])
  INC <-  0.25*as.numeric(nsduh_0$demog_income == levels(nsduh_0$demog_income)[2]) +
          0.50*as.numeric(nsduh_0$demog_income == levels(nsduh_0$demog_income)[3]) +
          0.75*as.numeric(nsduh_0$demog_income == levels(nsduh_0$demog_income)[4])
  
  p_mar_mj   <- ilogit(logit(p_ref)      + ALC + AGE + SEX + INC)
  p_mar_alc  <- ilogit(logit(p_ref) + MJ       + AGE + SEX + INC)
  p_mar_age  <- ilogit(logit(p_ref) + MJ + ALC       + SEX + INC)
  p_mar_sex  <- ilogit(logit(p_ref) + MJ + ALC + AGE       + INC)
  p_mar_inc  <- ilogit(logit(p_ref) + MJ + ALC + AGE + SEX      )
  p_mnar     <- ilogit(logit(p_ref) + MJ + ALC + AGE + SEX + INC)
  
  # par(mfrow=c(3,2))
  # hist(p_mar_mj)
  # abline(v=p_mcar, lwd=2, col="red")
  # hist(p_mar_alc)
  # abline(v=p_mcar, lwd=2, col="red")
  # hist(p_mar_age)
  # abline(v=p_mcar, lwd=2, col="red")
  # hist(p_mar_sex)
  # abline(v=p_mcar, lwd=2, col="red")
  # hist(p_mar_inc)
  # abline(v=p_mcar, lwd=2, col="red")
  # hist(p_mnar)
  # abline(v=p_mcar, lwd=2, col="red")
  # par(mfrow=c(1,1))
  # 
  # # Example: p_mar does not depend on alc but p_mnar does
  # par(mfrow=c(2,1))
  # plot(nsduh_0$alc_agefirst, p_mar_alc)
  # plot(nsduh_0$alc_agefirst, p_mnar)
  # par(mfrow=c(1,1))
  # 
  # par(mfrow=c(2,1))
  # plot(nsduh_0$mj_lifetime, p_mar_mj)
  # plot(nsduh_0$mj_lifetime, p_mnar)
  # par(mfrow=c(1,1))
  # 
  # plot(data.frame(p_mar_mj,
  #                 p_mar_alc,
  #                 p_mar_age,
  #                 p_mar_sex,
  #                 p_mar_inc))
  # # (the MNAR probs are all the same)

  # Set values to missing
  
  for(i in 1:NSIM) {
    
    print(paste("NSIM =", i))
    
    # Initialize    
    nsduh_mcar <- nsduh_mar <- nsduh_mnar <- nsduh_0
    
    # MCAR
    SUB_mcar_mj  <- rbernoulli(nrow(nsduh_0), p = p_mcar)
    SUB_mcar_alc <- rbernoulli(nrow(nsduh_0), p = p_mcar)
    SUB_mcar_age <- rbernoulli(nrow(nsduh_0), p = p_mcar)
    SUB_mcar_sex <- rbernoulli(nrow(nsduh_0), p = p_mcar)
    SUB_mcar_inc <- rbernoulli(nrow(nsduh_0), p = p_mcar)
    
    nsduh_mcar$mj_lifetime[   SUB_mcar_mj ] <- NA
    nsduh_mcar$alc_agefirst[  SUB_mcar_alc] <- NA
    nsduh_mcar$demog_age_cat6[SUB_mcar_age] <- NA
    nsduh_mcar$demog_sex[     SUB_mcar_sex] <- NA
    nsduh_mcar$demog_income[  SUB_mcar_inc] <- NA
    # summary(nsduh_mcar)
    
    # MAR
    SUB_mar_mj  <- rbernoulli(nrow(nsduh_0), p = p_mar_mj)
    SUB_mar_alc <- rbernoulli(nrow(nsduh_0), p = p_mar_alc)
    SUB_mar_age <- rbernoulli(nrow(nsduh_0), p = p_mar_age)
    SUB_mar_sex <- rbernoulli(nrow(nsduh_0), p = p_mar_sex)
    SUB_mar_inc <- rbernoulli(nrow(nsduh_0), p = p_mar_inc)
    
    nsduh_mar$mj_lifetime[   SUB_mar_mj ] <- NA
    nsduh_mar$alc_agefirst[  SUB_mar_alc] <- NA
    nsduh_mar$demog_age_cat6[SUB_mar_age] <- NA
    nsduh_mar$demog_sex[     SUB_mar_sex] <- NA
    nsduh_mar$demog_income[  SUB_mar_inc] <- NA
    # summary(nsduh_mar)
    
    # MNAR
    SUB_mnar_mj  <- rbernoulli(nrow(nsduh_0), p = p_mnar)
    SUB_mnar_alc <- rbernoulli(nrow(nsduh_0), p = p_mnar)
    SUB_mnar_age <- rbernoulli(nrow(nsduh_0), p = p_mnar)
    SUB_mnar_sex <- rbernoulli(nrow(nsduh_0), p = p_mnar)
    SUB_mnar_inc <- rbernoulli(nrow(nsduh_0), p = p_mnar)
    
    nsduh_mnar$mj_lifetime[   SUB_mnar_mj ] <- NA
    nsduh_mnar$alc_agefirst[  SUB_mnar_alc] <- NA
    nsduh_mnar$demog_age_cat6[SUB_mnar_age] <- NA
    nsduh_mnar$demog_sex[     SUB_mnar_sex] <- NA
    nsduh_mnar$demog_income[  SUB_mnar_inc] <- NA
    # summary(nsduh_mnar)
    
    NMISS_mcar[i] <- nsduh_mcar %>% drop_na() %>% nrow()
    NMISS_mar[ i] <- nsduh_mar  %>% drop_na() %>% nrow()
    NMISS_mnar[i] <- nsduh_mnar %>% drop_na() %>% nrow()
    
    # Multiple Imputation
    imp_mcar <- mice(nsduh_mcar,
                     seed  = 3,
                     m     = 5,
                     print = F)
    imp_mar  <- mice(nsduh_mar,
                     seed  = 3,
                     m     = 5,
                     print = F)
    imp_mnar <- mice(nsduh_mnar,
                     seed  = 3,
                     m     = 5,
                     print = F)
    
    # Complete-case analysis with MCAR data
    fit_mcar_cc <- glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
                       family = binomial, data=nsduh_mcar)
    # Complete-case analysis with MAR data
    fit_mar_cc  <- glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
                       family = binomial, data=nsduh_mar)
    # Complete-case analysis with MNAR data
    fit_mnar_cc <- glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
                       family = binomial, data=nsduh_mnar)
    
    # MI analysis with MCAR data
    fit_mcar_mi <- with(imp_mcar,
                        glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
                            family = binomial))
    # MI analysis with MAR data
    fit_mar_mi  <- with(imp_mar,
                        glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
                            family = binomial))
    # MI analysis with MNAR data
    fit_mnar_mi <- with(imp_mnar,
                        glm(mj_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex + demog_income,
                            family = binomial))
    
    # betas
    beta_mcar_cc[,,i] <- summary(fit_mcar_cc)$coef[-1, c("Estimate", "Std. Error")]
    beta_mar_cc[ ,,i] <- summary(fit_mar_cc)$coef[ -1, c("Estimate", "Std. Error")]
    beta_mnar_cc[,,i] <- summary(fit_mnar_cc)$coef[-1, c("Estimate", "Std. Error")]
    beta_mcar_mi[,,i] <- as.matrix(summary(pool(fit_mcar_mi))[-1, c("estimate", "std.error")])
    beta_mar_mi[ ,,i] <- as.matrix(summary(pool(fit_mar_mi))[ -1, c("estimate", "std.error")])
    beta_mnar_mi[,,i] <- as.matrix(summary(pool(fit_mnar_mi))[-1, c("estimate", "std.error")])
  }
  
  # Average over simulations
  beta_mcar_cc_mean <- data.frame(apply(beta_mcar_cc, c(1,2), mean))
  beta_mar_cc_mean  <- data.frame(apply(beta_mar_cc,  c(1,2), mean))
  beta_mnar_cc_mean <- data.frame(apply(beta_mnar_cc, c(1,2), mean))
  beta_mcar_mi_mean <- data.frame(apply(beta_mcar_mi, c(1,2), mean))
  beta_mar_mi_mean  <- data.frame(apply(beta_mar_mi,  c(1,2), mean))
  beta_mnar_mi_mean <- data.frame(apply(beta_mnar_mi, c(1,2), mean))
  names(beta_mcar_cc_mean) <- names(beta_mar_cc_mean) <- names(beta_mnar_cc_mean) <- c("beta", "se")
  names(beta_mcar_mi_mean) <- names(beta_mar_mi_mean) <- names(beta_mnar_mi_mean) <- c("beta", "se")
  
  # Compute ORs
  beta_0$aor            <- exp(beta_0$beta)
  beta_mcar_cc_mean$aor <- exp(beta_mcar_cc_mean$beta)
  beta_mar_cc_mean$aor  <- exp(beta_mar_cc_mean$beta)
  beta_mnar_cc_mean$aor <- exp(beta_mnar_cc_mean$beta)
  beta_mcar_mi_mean$aor <- exp(beta_mcar_mi_mean$beta)
  beta_mar_mi_mean$aor  <- exp(beta_mar_mi_mean$beta)
  beta_mnar_mi_mean$aor <- exp(beta_mnar_mi_mean$beta)
  
  MIN <- min(c(beta_mcar_cc_mean$aor,
               beta_mar_cc_mean$aor,
               beta_mnar_cc_mean$aor,
               beta_mcar_mi_mean$aor,
               beta_mar_mi_mean$aor,
               beta_mnar_mi_mean$aor))
  MAX <- max(c(beta_mcar_cc_mean$aor,
               beta_mar_cc_mean$aor,
               beta_mnar_cc_mean$aor,
               beta_mcar_mi_mean$aor,
               beta_mar_mi_mean$aor,
               beta_mnar_mi_mean$aor))
  
  par(mfrow=c(3,2))
  plot(beta_0$aor, beta_mcar_cc_mean$aor,
       xlim = range(beta_0$aor), ylim = c(MIN, MAX),
       xlab = "AOR, no missing data",
       ylab = "AOR estimate")
  abline(a=0, b=1, lty=2)
  title("MCAR, CC")
  
  plot(beta_0$aor, beta_mcar_mi_mean$aor,
       xlim = range(beta_0$aor), ylim = c(MIN, MAX),
       xlab = "AOR, no missing data",
       ylab = "AOR estimate")
  abline(a=0, b=1, lty=2)
  title("MCAR, MI")

  plot(beta_0$aor, beta_mar_cc_mean$aor,
       xlim = range(beta_0$aor), ylim = c(MIN, MAX),
       xlab = "AOR, no missing data",
       ylab = "AOR estimate")
  abline(a=0, b=1, lty=2)
  title("MAR, CC")
  
  plot(beta_0$aor, beta_mar_mi_mean$aor,
       xlim = range(beta_0$aor), ylim = c(MIN, MAX),
       xlab = "AOR, no missing data",
       ylab = "AOR estimate")
  abline(a=0, b=1, lty=2)
  title("MAR, MI")
  
  plot(beta_0$aor, beta_mnar_cc_mean$aor,
       xlim = range(beta_0$aor), ylim = c(MIN, MAX),
       xlab = "AOR, no missing data",
       ylab = "AOR estimate")
  abline(a=0, b=1, lty=2)
  title("MNAR, CC")
  
  plot(beta_0$aor, beta_mnar_mi_mean$aor,
       xlim = range(beta_0$aor), ylim = c(MIN, MAX),
       xlab = "AOR, no missing data",
       ylab = "AOR estimate")
  abline(a=0, b=1, lty=2)
  title("MNAR, MI")
  par(mfrow=c(1,1))
  
  # # Plot SEs
  # # SE with no missing data is always lower because larger n
  # # SE after MI similar in the 3 models
  # MIN <- min(c(beta_mcar_cc_mean$se,
  #              beta_mar_cc_mean$se,
  #              beta_mnar_cc_mean$se,
  #              beta_mcar_mi_mean$se,
  #              beta_mar_mi_mean$se,
  #              beta_mnar_mi_mean$se))
  # MAX <- max(c(beta_mcar_cc_mean$se,
  #              beta_mar_cc_mean$se,
  #              beta_mnar_cc_mean$se,
  #              beta_mcar_mi_mean$se,
  #              beta_mar_mi_mean$se,
  #              beta_mnar_mi_mean$se))
  # 
  # par(mfrow=c(3,2))
  # plot(beta_0$se, beta_mcar_cc_mean$se,
  #      xlim = range(beta_0$se), ylim = c(MIN, MAX),
  #      xlab = "SE, no missing data",
  #      ylab = "SE estimate")
  # abline(a=0, b=1, lty=2)
  # title("MCAR, CC")
  # 
  # plot(beta_0$se, beta_mcar_mi_mean$se,
  #      xlim = range(beta_0$se), ylim = c(MIN, MAX),
  #      xlab = "SE, no missing data",
  #      ylab = "SE estimate")
  # abline(a=0, b=1, lty=2)
  # title("MCAR, MI")
  # 
  # plot(beta_0$se, beta_mar_cc_mean$se,
  #      xlim = range(beta_0$se), ylim = c(MIN, MAX),
  #      xlab = "SE, no missing data",
  #      ylab = "SE estimate")
  # abline(a=0, b=1, lty=2)
  # title("MAR, CC")
  # 
  # plot(beta_0$se, beta_mar_mi_mean$se,
  #      xlim = range(beta_0$se), ylim = c(MIN, MAX),
  #      xlab = "SE, no missing data",
  #      ylab = "SE estimate")
  # abline(a=0, b=1, lty=2)
  # title("MAR, MI")
  # 
  # plot(beta_0$se, beta_mnar_cc_mean$se,
  #      xlim = range(beta_0$se), ylim = c(MIN, MAX),
  #      xlab = "SE, no missing data",
  #      ylab = "SE estimate")
  # abline(a=0, b=1, lty=2)
  # title("MNAR, CC")
  # 
  # plot(beta_0$se, beta_mnar_mi_mean$se,
  #      xlim = range(beta_0$se), ylim = c(MIN, MAX),
  #      xlab = "SE, no missing data",
  #      ylab = "SE estimate")
  # abline(a=0, b=1, lty=2)
  # title("MNAR, MI")
  # par(mfrow=c(1,1))
  
  cat("MCAR n\n")
  print(range(NMISS_mcar))
  cat("\n\n")
  cat("MAR n\n")
  print(range(NMISS_mar))
  cat("\n\n")
  cat("MNAR n\n")
  print(range(NMISS_mnar))
  cat("\n\n")
  
  # Return datasets from the final simulation iteration
  return(list("nsduh_0"    = nsduh_0,
              "nsduh_mcar" = nsduh_mcar,
              "nsduh_mar"  = nsduh_mar,
              "nsduh_mnar" = nsduh_mnar))
}

# set.seed(1)
# mysim(NSIM   = 5,
#       p_mcar = 0.05,
#       p_ref  = 0.02,
#       b_mj   = 2.5,
#       b_alc  = 0.08)
# # This figure looks great
# # Range of sample sizes across the simulations:
# # MCAR n
# # [1] 635 656
# # MAR n
# # [1] 399 424
# # MNAR n
# # [1] 366 391

# For the text, I want the N to be closer to 600...
set.seed(1)
OUT <- mysim(NSIM   = 25,
             p_mcar = 0.05,
             p_ref  = 0.005,
             b_mj   = 2.5,
             b_alc  = 0.08)
# This figure looks great, too
# Range of sample sizes across the simulations:
# MCAR n
# [1] 634 677
# MAR n
# [1] 643 684
# MNAR n
# [1] 615 653

# Export figure to "MissingDataSimulation.png"

# Save datasets from final simulation iteration
nsduh_0    <- OUT[[1]]
nsduh_mcar <- OUT[[2]]
nsduh_mar  <- OUT[[3]]
nsduh_mnar <- OUT[[4]]
save(nsduh_0,    file = "nsduh_0_rmph.RData")
save(nsduh_mcar, file = "nsduh_mcar_rmph.RData")
save(nsduh_mar,  file = "nsduh_mar_rmph.RData")
save(nsduh_mnar, file = "nsduh_mnar_rmph.RData")
