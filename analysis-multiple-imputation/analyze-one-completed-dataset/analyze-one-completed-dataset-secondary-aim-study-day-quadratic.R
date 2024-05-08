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

source(file = file.path("custom-analysis-code-emee-estimator", "emee_categorical_trt_with_Delta.R"))
source(file = file.path("custom-analysis-code-emee-estimator", "helper-functions.R"))
source(file = file.path("custom-analysis-code-emee-estimator", "emee-categorical-verbose-output.R"))

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
dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_long_completed.rds"))
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
dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))

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
# Grab subset of decision points which will be used to estimate the treatment
# effect.
###############################################################################
dat_long_completed <- dat_long_completed %>% arrange(replicate_id, participant_id, decision_point)
dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))

###############################################################################
# Analysis with completed dataset
###############################################################################
dat_for_analysis <- dat_long_completed %>% filter(replicate_id == 0)
dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)

fit1 <- tryCatch(expr = {emee_categorical_trt_with_Delta(
                 dta = dat_for_analysis_elig,
                 id_varname = "participant_id",
                 decision_time_varname = "decision_point",
                 treatment_varname = "treatment_cate",
                 outcome_varname = "Y",
                 control_varname = c("age", "is_male", "is_latino", "is_not_latino_and_black", "is_not_latino_and_other", "baseline_tobacco_history", "has_partner", "income_val", "hour_coinflip_local", "days_between_v1_and_coinflip_local", "any_response_2qs", "any_recent_eligible_dp", "engagement_most_recent_eligible"),
                 moderator_varname = c("days_between_v1_and_coinflip_local", "days_between_v1_and_coinflip_local_squared"),
                 rand_prob_varname = "rand_prob",
                 rand_prob_A0_varname = "rand_prob_A0",
                 avail_varname = NULL,
                 rand_prob_tilde_varname = NULL,
                 # For MRT with constant randomization probability, 
                 # simply set rand_prob_tilde to be equal to the actual randomization probabilities
                 # Here, what ever you coded as 0, 1, 2 in treatment_cate
                 # specify the randomization probabilities in that order
                 rand_prob_tilde = c(0.50, 0.25, 0.25),  
                 estimator_initial_value = NULL,
                 Delta = 1,
                 # Number of treatment categories (Important note: this must exclude the reference category)
                 num_trt = 2  
                 )},
                 warning = function(w){"Hey, a warning"})

if(class(fit1) == "character"){
  fit1_converted <- "Hey, a warning"
  results_obj <- "Hey, a warning"
}else{
  fit1_converted <- convert_to_emee_fit_object(fit1, dat_for_analysis, "participant_id")
  
  Lmat1 <- matrix(c(1,0,0,-1,0,0,
                    0,1,0,0,-1,0,
                    0,0,1,0,0,-1), nrow = 3, byrow = TRUE)
  
  Lmat2 <- matrix(cbind(rep(1,8),
                        seq(2,9,1),
                        seq(2,9,1) * seq(2,9,1),
                        -1*rep(1,8),
                        -1*seq(2,9,1),
                        -1*seq(2,9,1) * seq(2,9,1)), ncol = 6, byrow = FALSE)
  
  Lmat <- rbind(Lmat1, Lmat2)
  
  results_obj <- summary(fit1_converted, show_control_fit = TRUE, lincomb = Lmat) 
}

saveRDS(fit1_converted, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "fit_obj_secondary_study_day_quadratic.rds"))
saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_secondary_study_day_quadratic.rds"))

###############################################################################
# Analysis with replicated dataset
###############################################################################

for(idx_replicate in 1:max_replicate_id){
  dat_for_analysis <- dat_long_completed %>% filter(replicate_id == idx_replicate)
  dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)
  
  fit1 <- tryCatch(expr = {emee_categorical_trt_with_Delta(
                   dta = dat_for_analysis_elig,
                   id_varname = "participant_id",
                   decision_time_varname = "decision_point",
                   treatment_varname = "treatment_cate",
                   outcome_varname = "Y",
                   control_varname = c("age", "is_male", "is_latino", "is_not_latino_and_black", "is_not_latino_and_other", "baseline_tobacco_history", "has_partner", "income_val", "hour_coinflip_local", "days_between_v1_and_coinflip_local", "any_response_2qs", "any_recent_eligible_dp", "engagement_most_recent_eligible"),
                   moderator_varname = c("days_between_v1_and_coinflip_local", "days_between_v1_and_coinflip_local_squared"),
                   rand_prob_varname = "rand_prob",
                   rand_prob_A0_varname = "rand_prob_A0",
                   avail_varname = NULL,
                   rand_prob_tilde_varname = NULL,
                   # For MRT with constant randomization probability, 
                   # simply set rand_prob_tilde to be equal to the actual randomization probabilities
                   # Here, what ever you coded as 0, 1, 2 in treatment_cate
                   # specify the randomization probabilities in that order
                   rand_prob_tilde = c(0.50, 0.25, 0.25),  
                   estimator_initial_value = NULL,
                   Delta = 1,
                   # Number of treatment categories (Important note: this must exclude the reference category)
                   num_trt = 2  
                   )},
                   warning = function(w){"Hey, a warning"})
  
  if(class(fit1) == "character"){
    fit1_converted <- "Hey, a warning"
    results_obj <- "Hey, a warning"
  }else{
    fit1_converted <- convert_to_emee_fit_object(fit1, dat_for_analysis, "participant_id")
    
    Lmat1 <- matrix(c(1,0,0,-1,0,0,
                      0,1,0,0,-1,0,
                      0,0,1,0,0,-1), nrow = 3, byrow = TRUE)
    
    Lmat2 <- matrix(cbind(rep(1,8),
                          seq(2,9,1),
                          seq(2,9,1) * seq(2,9,1),
                          -1*rep(1,8),
                          -1*seq(2,9,1),
                          -1*seq(2,9,1) * seq(2,9,1)), ncol = 6, byrow = FALSE)
    
    Lmat <- rbind(Lmat1, Lmat2)
    
    results_obj <- summary(fit1_converted, show_control_fit = TRUE, lincomb = Lmat)
  }
  
  saveRDS(fit1_converted, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit1_obj_secondary_study_day_quadratic", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_secondary_study_day_quadratic", "_replicate_", idx_replicate, ".rds", sep = "")))
}

