rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages
###############################################################################
source("paths.R")
library(geepack)
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

###############################################################################
# Mean among eligible decision points micro-randomized to prompt (any type)
###############################################################################
list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est_logodds_scale <- results_logodds_scale[["Estimate"]]
                                           var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
                                           return(results)
                                           })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:48){
    all_est_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est_logodds_scale"]]
    all_var_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var_logodds_scale"]]
  }
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_prompt_by_dp.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_prompt_by_dp.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 0)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est_logodds_scale <- results_logodds_scale[["Estimate"]]
                                           var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
                                           return(results)
                                         })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:48){
    all_est_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est_logodds_scale"]]
    all_var_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var_logodds_scale"]]
  }
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_no_prompt_by_dp.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_no_prompt_by_dp.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_high_effort == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est_logodds_scale <- results_logodds_scale[["Estimate"]]
                                           var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
                                           return(results)
                                         })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:48){
    all_est_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est_logodds_scale"]]
    all_var_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var_logodds_scale"]]
  }
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_high_effort_prompt_by_dp.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_high_effort_prompt_by_dp.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
list_all_estimates_by_dp <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_low_effort == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp <- lapply(list_current_fit_by_dp,
                                         function(current_fit){
                                           results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                           est_logodds_scale <- results_logodds_scale[["Estimate"]]
                                           var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                           results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
                                           return(results)
                                         })
  list_all_estimates_by_dp <- append(list_all_estimates_by_dp, list(list_current_estimates_by_dp))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 48), nrow = .__total_imputed_datasets, ncol = 48)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(idx_decision_point in 1:48){
    all_est_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["est_logodds_scale"]]
    all_var_logodds_scale[mi_dataset_num, idx_decision_point] <- list_all_estimates_by_dp[[mi_dataset_num]][[idx_decision_point]][["var_logodds_scale"]]
  }
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_low_effort_prompt_by_dp.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_low_effort_prompt_by_dp.rds"))

