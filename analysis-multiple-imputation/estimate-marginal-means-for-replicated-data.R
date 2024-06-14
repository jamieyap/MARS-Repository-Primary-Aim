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
list_all_estimates <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter(eligibility == 1) %>% filter(coinflip == 1)
  
  current_fit <- geeglm(Y ~ 1, data = dat_long_completed, id = participant_id, family = binomial)
  
  results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
  est_logodds_scale <- results_logodds_scale[["Estimate"]]
  var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
  results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
  
  current_estimates <- results
  
  list_all_estimates <- append(list_all_estimates, list(current_estimates))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  all_est_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["est_logodds_scale"]]
  all_var_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["var_logodds_scale"]]
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_prompt.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_prompt.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
list_all_estimates <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter(eligibility == 1) %>% filter(coinflip == 0)
  
  current_fit <- geeglm(Y ~ 1, data = dat_long_completed, id = participant_id, family = binomial)
  
  results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
  est_logodds_scale <- results_logodds_scale[["Estimate"]]
  var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
  results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
  
  current_estimates <- results
  
  list_all_estimates <- append(list_all_estimates, list(current_estimates))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  all_est_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["est_logodds_scale"]]
  all_var_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["var_logodds_scale"]]
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_no_prompt.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_no_prompt.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
list_all_estimates <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter(eligibility == 1) %>% filter(is_high_effort == 1)
  
  current_fit <- geeglm(Y ~ 1, data = dat_long_completed, id = participant_id, family = binomial)
  
  results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
  est_logodds_scale <- results_logodds_scale[["Estimate"]]
  var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
  results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
  
  current_estimates <- results
  
  list_all_estimates <- append(list_all_estimates, list(current_estimates))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  all_est_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["est_logodds_scale"]]
  all_var_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["var_logodds_scale"]]
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_high_effort_prompt.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_high_effort_prompt.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
list_all_estimates <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter((decision_point >= 7) & (decision_point <= 54))
  
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 1) %>% filter(eligibility == 1) %>% filter(is_low_effort == 1)
  
  current_fit <- geeglm(Y ~ 1, data = dat_long_completed, id = participant_id, family = binomial)
  
  results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
  est_logodds_scale <- results_logodds_scale[["Estimate"]]
  var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
  results <- tibble(est_logodds_scale = est_logodds_scale, var_logodds_scale = var_logodds_scale)
  
  current_estimates <- results
  
  list_all_estimates <- append(list_all_estimates, list(current_estimates))
}

all_est_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)
all_var_logodds_scale <- matrix(rep(NA, .__total_imputed_datasets * 1), nrow = .__total_imputed_datasets, ncol = 1)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  all_est_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["est_logodds_scale"]]
  all_var_logodds_scale[mi_dataset_num, 1] <- list_all_estimates[[mi_dataset_num]][["var_logodds_scale"]]
}

saveRDS(all_est_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_low_effort_prompt.rds"))
saveRDS(all_var_logodds_scale, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_low_effort_prompt.rds"))

