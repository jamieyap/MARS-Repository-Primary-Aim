###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters for generating the current completed dataset
###############################################################################
mi_dataset_num <- .__par_mi_number

source("paths.R")
library(tidyverse)
library(mice)
library(MRTAnalysis)

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results"))
}

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num))
}

###############################################################################
# Grab time variables (not included in the sequentially completed datasets)
###############################################################################
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))
dat_primary_aim <- dat_primary_aim %>% select(mars_id, participant_id, decision_point, hour_coinflip_local, days_between_v1_and_coinflip_local, ts_coinflip_local)
all_ids <- unique(dat_primary_aim[["mars_id"]])
n_ids <- length(all_ids)

scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))
scanned_decision_points_within_range <- scanned_decision_points_within_range %>% select(mars_id, decision_point, decision_points_most_recent_eligible)

###############################################################################
# Read in completed dataset, merge the time variables to
# corresponding participant-decision point, convert factors to numeric type,
# Note that dat_long_completed will contain both the original long dataset and 
# replicates.
###############################################################################
dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_long_completed.rds"))
dat_long_completed <- left_join(x = dat_long_completed, y = dat_primary_aim, by = join_by(participant_id == participant_id, decision_point == decision_point))
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

###############################################################################
# Analysis with completed dataset! 
# =o)
###############################################################################
dat_for_analysis <- dat_long_completed %>% filter(replicate_id == 0)

fit1 <- emee(
  data = dat_for_analysis,
  id = "participant_id",  
  outcome = "Y",
  treatment = "coinflip",
  rand_prob = 0.5,
  moderator_formula = ~ 1,  
  control_formula = ~ 1, #age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + days_between_v1_and_coinflip_local + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
  availability = "eligibility"
)

results_obj_primary_marginal <- summary(fit1, show_control_fit = TRUE)

saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "fit_obj_primary_marginal.rds"))
saveRDS(results_obj_primary_marginal, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_marginal.rds"))

###############################################################################
# Analysis with replicated dataset! 
# =o)
###############################################################################

max_replicate_id <- max(dat_long_completed[["replicate_id"]])

for(idx_replicate in 1:max_replicate_id){
  dat_for_analysis <- dat_long_completed %>% filter(replicate_id == idx_replicate)
  
  fit1 <- emee(
    data = dat_for_analysis,
    id = "participant_id",  
    outcome = "Y",
    treatment = "coinflip",
    rand_prob = 0.5,
    moderator_formula = ~ 1,  
    control_formula = ~ 1, #age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + hour_coinflip_local + days_between_v1_and_coinflip_local + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
    availability = "eligibility"
  )
  
  results_obj_primary_marginal <- summary(fit1, show_control_fit = TRUE)
  
  saveRDS(fit1, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("fit_obj_primary_marginal", "_replicate_", idx_replicate, ".rds", sep = "")))
  saveRDS(results_obj_primary_marginal, file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_primary_marginal", "_replicate_", idx_replicate, ".rds", sep = "")))
}






