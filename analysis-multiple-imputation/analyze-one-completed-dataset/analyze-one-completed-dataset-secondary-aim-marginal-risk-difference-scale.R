###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters for generating the current completed dataset
###############################################################################
mi_dataset_num <- .__current_idx

source("paths.R")
library(tidyverse)
library(mice)
library(geepack)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
# When running this script within imputation-pipeline.R, the package MASS may still be loaded in the global environment
select <- dplyr::select 

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results"))
}

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num))
}

###############################################################################
# Read in completed dataset, merge the time variables to
# corresponding participant-decision point, convert factors to numeric type,
# Note that dat_long_completed will contain both the original long dataset and 
# replicates.
###############################################################################

if(isTRUE(.__sensitivity_using_deterministically_imputed_proximal_outcome)){
  dat_long_completed <- readRDS(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_impute_proximal_outcome_deterministically.rds", sep = "")))
  
  if(isTRUE(.__use_deterministic_rule_conservative)){
    dat_long_completed <- dat_long_completed %>% mutate(Y_origninal = Y) %>% mutate(Y = Y_conservative)
  }else{
    dat_long_completed <- dat_long_completed %>% mutate(Y_origninal = Y) %>% mutate(Y = Y_liberal)
  }
  
}else{
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_long_completed.rds"))
}

max_replicate_id <- max(dat_long_completed[["replicate_id"]])

###############################################################################
# Replace NA's with -1's among decision points having eligibility = 0.
###############################################################################
my_list <- list(Y = -1,
                coinflip = -1,
                hour_coinflip_local = -1, 
                days_between_v1_and_coinflip_local = -1,
                days_between_v1_and_coinflip_local_squared = -1,
                any_response_2qs = -1, 
                any_recent_eligible_dp = -1, 
                engagement_most_recent_eligible = -1,
                age = -1, 
                is_male = -1, 
                is_latino = -1, 
                is_not_latino_and_black = -1, 
                is_not_latino_and_other = -1, 
                baseline_tobacco_history = -1, 
                has_partner = -1, 
                income_val = -1)

dat_long_completed <- dat_long_completed %>% replace_na(my_list)

###############################################################################
# Grab subset of decision points which will be used to estimate the treatment
# effect.
###############################################################################
dat_long_completed <- dat_long_completed %>% arrange(replicate_id, participant_id, decision_point)

if(isTRUE(.__use_all_days)){
  dat_long_completed <- dat_long_completed
}else{
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
}

###############################################################################
# We will set up the dataset so that in analysis, we may have the following 
# comparisons:
#   * high effort versus no prompt
#   * low effort versus no prompt
###############################################################################

# We will set up the dataset so that in analysis, we may have the following comparisons:
#   * high effort prompt versus no prompt
#   * low effort prompt versus no prompt
dat_long_completed <- dat_long_completed %>%
  mutate(treatment_cate = case_when(
    is_low_effort == 1 ~ 2,  # This allows us to make the comparison: low effort prompt versus no prompt
    is_high_effort == 1 ~ 1, # This allows us to make the comparison: high effort prompt versus no prompt
    coinflip == 0 ~ 0,       # Important note: the value of zero in treatment_cate is used to indicate the reference category to be used. 
    .default = NULL)) %>%
  mutate(rand_prob = case_when(
    treatment_cate == 0 ~ 0.50,
    treatment_cate == 1 ~ 0.25,
    treatment_cate == 2 ~ 0.25,
    .default = NULL))

# rand_prob_A0 is the randomization probability corresponding to the reference category (whatever was coded to take on a value of 0 in treatment_cate)
dat_long_completed[["rand_prob_A0"]] <- dat_long_completed %>% filter(treatment_cate == 0) %>% .[["rand_prob"]] %>% unique(.)

###############################################################################
# Analysis with completed dataset
###############################################################################
dat_for_analysis <- dat_long_completed %>% filter(replicate_id == 0)
dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)

fit <- tryCatch(expr = {geeglm(Y ~ is_high_effort + is_low_effort + age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + hour_coinflip_local + days_between_v1_and_coinflip_local + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
                               family = "gaussian", 
                               data = dat_for_analysis_elig, 
                               id = participant_id, 
                               waves = decision_point)},
                warning = function(w){"Hey, a warning"})

if(length(fit) == 1){
  fit_converted <- "Hey, a warning"
  results_obj <- "Hey, a warning"
}else{
  fit_converted <- fit
  
  results_obj <- data.frame(variable = rownames(summary(fit)$coefficients),
                            estimates = summary(fit)$coefficients[,"Estimate"],
                            std_err = summary(fit)$coefficients[,"Std.err"],
                            LCL95 = summary(fit)$coefficients[,"Estimate"] - qnorm(0.975)*summary(fit)$coefficients[,"Std.err"],
                            UCL95 = summary(fit)$coefficients[,"Estimate"] + qnorm(0.975)*summary(fit)$coefficients[,"Std.err"])
  
  Lmat <- matrix(c(0,1,0, rep(0,13),
                   0,0,1, rep(0,13),
                   0,1,-1, rep(0,13)), 
                 ncol = 16, byrow = TRUE)
  
  est_contrast <- Lmat %*% as.matrix(fit$coefficients)
  est_vcov_contrast <- Lmat %*% vcov(fit) %*% t(Lmat)
  est_stderr_contrast <- sqrt(diag(est_vcov_contrast))
  LB95 <- c(est_contrast) - qnorm(0.975) * est_stderr_contrast
  UB95 <- c(est_contrast) + qnorm(0.975) * est_stderr_contrast
  
  dat_result_contrast <- data.frame(contrast = c("high vs none", "low vs none", "high vs low"), 
                                    estimates = est_contrast, 
                                    std_err = est_stderr_contrast,
                                    LB95 = LB95, 
                                    UB95 = UB95)
}

if(isTRUE(.__use_all_days) & isTRUE(!.__sensitivity_using_deterministically_imputed_proximal_outcome)){
  add_prefix <- "sensitivity_"
}

if(isFALSE(.__use_all_days) & isTRUE(.__sensitivity_using_deterministically_imputed_proximal_outcome)){
  if(isTRUE(.__use_deterministic_rule_conservative)){
    add_prefix <- "sensitivity2_"
  }else{
    add_prefix <- "sensitivity3_"
  }
}

if(isFALSE(.__use_all_days) & isTRUE(!.__sensitivity_using_deterministically_imputed_proximal_outcome)){
  add_prefix <- ""
}

saveRDS(fit_converted, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste(add_prefix, "fit_obj_secondary_marginal_risk_difference.rds", sep = "")))
saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste(add_prefix, "results_obj_secondary_marginal_risk_difference.rds", sep = "")))
saveRDS(dat_result_contrast, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste(add_prefix, "contrast_secondary_marginal_risk_difference.rds", sep = "")))

###############################################################################
# Analysis with replicated dataset
###############################################################################

for(idx_replicate in 1:max_replicate_id){
  dat_for_analysis <- dat_long_completed %>% filter(replicate_id == idx_replicate)
  dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)
  
  fit <- tryCatch(expr = {geeglm(Y ~ is_high_effort + is_low_effort + age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + hour_coinflip_local + days_between_v1_and_coinflip_local + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
                                 family = "gaussian", 
                                 data = dat_for_analysis_elig, 
                                 id = participant_id, 
                                 waves = decision_point)},
                  warning = function(w){"Hey, a warning"})
  
  if(length(fit) == 1){
    fit_converted <- "Hey, a warning"
    results_obj <- "Hey, a warning"
  }else{
    fit_converted <- fit
    
    results_obj <- data.frame(variable = rownames(summary(fit)$coefficients),
                              estimates = summary(fit)$coefficients[,"Estimate"],
                              std_err = summary(fit)$coefficients[,"Std.err"],
                              LCL95 = summary(fit)$coefficients[,"Estimate"] - qnorm(0.975)*summary(fit)$coefficients[,"Std.err"],
                              UCL95 = summary(fit)$coefficients[,"Estimate"] + qnorm(0.975)*summary(fit)$coefficients[,"Std.err"])
    
    Lmat <- matrix(c(0,1,0, rep(0,13),
                     0,0,1, rep(0,13),
                     0,1,-1, rep(0,13)), 
                   ncol = 16, byrow = TRUE)
    
    est_contrast <- Lmat %*% as.matrix(fit$coefficients)
    est_vcov_contrast <- Lmat %*% vcov(fit) %*% t(Lmat)
    est_stderr_contrast <- sqrt(diag(est_vcov_contrast))
    LB95 <- c(est_contrast) - qnorm(0.975) * est_stderr_contrast
    UB95 <- c(est_contrast) + qnorm(0.975) * est_stderr_contrast
    
    dat_result_contrast <- data.frame(contrast = c("high vs none", "low vs none", "high vs low"), 
                                      estimates = est_contrast, 
                                      std_err = est_stderr_contrast,
                                      LB95 = LB95, 
                                      UB95 = UB95)
  }
  
  if(isTRUE(.__use_all_days) & isTRUE(!.__sensitivity_using_deterministically_imputed_proximal_outcome)){
    add_prefix <- "sensitivity_"
  }
  
  if(isFALSE(.__use_all_days) & isTRUE(.__sensitivity_using_deterministically_imputed_proximal_outcome)){
    if(isTRUE(.__use_deterministic_rule_conservative)){
      add_prefix <- "sensitivity2_"
    }else{
      add_prefix <- "sensitivity3_"
    }
  }
  
  if(isFALSE(.__use_all_days) & isTRUE(!.__sensitivity_using_deterministically_imputed_proximal_outcome)){
    add_prefix <- ""
  }
  
  saveRDS(fit_converted, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste(add_prefix, "fit_obj_secondary_marginal_risk_difference", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste(add_prefix, "results_obj_secondary_marginal_risk_difference", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(dat_result_contrast, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste(add_prefix, "contrast_secondary_marginal_risk_difference", "_replicate_", idx_replicate, ".rds", sep = "")))
}

