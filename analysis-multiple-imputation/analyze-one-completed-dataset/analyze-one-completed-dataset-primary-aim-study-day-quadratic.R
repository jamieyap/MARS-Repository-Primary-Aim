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
library(MRTAnalysis)

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
# Analysis with completed dataset
###############################################################################
dat_for_analysis <- dat_long_completed %>% filter(replicate_id == 0)

fit1 <- tryCatch(expr = {emee(
                 data = dat_for_analysis,
                 id = "participant_id",  
                 outcome = "Y",
                 treatment = "coinflip",
                 rand_prob = 0.5,
                 moderator_formula = ~ 1 + days_between_v1_and_coinflip_local + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local),  
                 control_formula = ~ 1 + age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + hour_coinflip_local + days_between_v1_and_coinflip_local + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local) + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
                 availability = "eligibility"
                 )},
                 warning = function(w){"Hey, a warning"})

if(class(fit1) == "character"){
  results_obj <- "Hey, a warning"
}else{
  Lmat <- matrix(c(rep(1,8), 2:9, (2:9)*(2:9)), ncol = 3, byrow = FALSE)
  results_obj <- summary(fit1, show_control_fit = TRUE, lincomb = Lmat)
}

saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "fit_obj_primary_study_day_quadratic.rds"))
saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_study_day_quadratic.rds"))

###############################################################################
# Analysis with replicated dataset
###############################################################################
max_replicate_id <- max(dat_long_completed[["replicate_id"]])

for(idx_replicate in 1:max_replicate_id){
  dat_for_analysis <- dat_long_completed %>% filter(replicate_id == idx_replicate)
  
  fit1 <- tryCatch(expr = {emee(data = dat_for_analysis,
                                id = "participant_id",  
                                outcome = "Y",
                                treatment = "coinflip",
                                rand_prob = 0.5,
                                moderator_formula = ~ 1 + days_between_v1_and_coinflip_local + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local),  
                                control_formula = ~ 1 + age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + hour_coinflip_local + days_between_v1_and_coinflip_local + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local) + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
                                availability = "eligibility"
                              )}, 
                   warning = function(w){"Hey, a warning"})
  
  if(class(fit1) == "character"){
    results_obj <- "Hey, a warning"
  }else{
    Lmat <- matrix(c(rep(1,8), 2:9, (2:9)*(2:9)), ncol = 3, byrow = FALSE)
    results_obj <- summary(fit1, show_control_fit = TRUE, lincomb = Lmat)
  }
  
  saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit_obj_primary_study_day_quadratic", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(results_obj, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_primary_study_day_quadratic", "_replicate_", idx_replicate, ".rds", sep = "")))
}

