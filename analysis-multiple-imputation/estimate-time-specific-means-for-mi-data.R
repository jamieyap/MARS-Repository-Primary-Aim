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
list_all_estimates_by_dp_mu_scale <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp_mu_scale <- lapply(list_current_fit_by_dp, 
                                                  function(current_fit){
                                                    var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                                    results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                                    est_logodds <- results_logodds_scale[["Estimate"]]
                                                    est_prob <- exp(est_logodds)/(1 + exp(est_logodds))
                                                    grad <- est_prob * (1 - est_prob)
                                                    var_G <- grad * var_logodds_scale * grad
                                                    results_mu_scale <- tibble(est = est_prob, std_err = sqrt(var_G))
                                                    return(results_mu_scale)
                                                  })
  list_all_estimates_by_dp_mu_scale <- append(list_all_estimates_by_dp_mu_scale, list(list_current_estimates_by_dp_mu_scale))
}

all_est_prob <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)
all_est_var <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(dp in 1:60){
    all_est_prob[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["est"]]
    all_est_var[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["std_err"]]^2
  }
}

saveRDS(all_est_prob, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_prompt_by_dp.rds"))
saveRDS(all_est_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_prompt_by_dp.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
list_all_estimates_by_dp_mu_scale <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 0)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp_mu_scale <- lapply(list_current_fit_by_dp, 
                                                  function(current_fit){
                                                    var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                                    results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                                    est_logodds <- results_logodds_scale[["Estimate"]]
                                                    est_prob <- exp(est_logodds)/(1 + exp(est_logodds))
                                                    grad <- est_prob * (1 - est_prob)
                                                    var_G <- grad * var_logodds_scale * grad
                                                    results_mu_scale <- tibble(est = est_prob, std_err = sqrt(var_G))
                                                    return(results_mu_scale)
                                                  })
  list_all_estimates_by_dp_mu_scale <- append(list_all_estimates_by_dp_mu_scale, list(list_current_estimates_by_dp_mu_scale))
}

all_est_prob <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)
all_est_var <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(dp in 1:60){
    all_est_prob[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["est"]]
    all_est_var[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["std_err"]]^2
  }
}

saveRDS(all_est_prob, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_no_prompt_by_dp.rds"))
saveRDS(all_est_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_no_prompt_by_dp.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
list_all_estimates_by_dp_mu_scale <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_high_effort == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp_mu_scale <- lapply(list_current_fit_by_dp, 
                                                  function(current_fit){
                                                    var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                                    results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                                    est_logodds <- results_logodds_scale[["Estimate"]]
                                                    est_prob <- exp(est_logodds)/(1 + exp(est_logodds))
                                                    grad <- est_prob * (1 - est_prob)
                                                    var_G <- grad * var_logodds_scale * grad
                                                    results_mu_scale <- tibble(est = est_prob, std_err = sqrt(var_G))
                                                    return(results_mu_scale)
                                                  })
  list_all_estimates_by_dp_mu_scale <- append(list_all_estimates_by_dp_mu_scale, list(list_current_estimates_by_dp_mu_scale))
}

all_est_prob <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)
all_est_var <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(dp in 1:60){
    all_est_prob[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["est"]]
    all_est_var[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["std_err"]]^2
  }
}

saveRDS(all_est_prob, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_high_effort_prompt_by_dp.rds"))
saveRDS(all_est_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_high_effort_prompt_by_dp.rds"))

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
list_all_estimates_by_dp_mu_scale <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_long_completed <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_low_effort == 1)
  list_current_fit_by_dp <- dat_long_completed %>% group_by(decision_point) %>% group_map(~ geeglm(Y ~ 1, data = .x, id = participant_id, family = binomial))
  list_current_estimates_by_dp_mu_scale <- lapply(list_current_fit_by_dp, 
                                                  function(current_fit){
                                                    var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
                                                    results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
                                                    est_logodds <- results_logodds_scale[["Estimate"]]
                                                    est_prob <- exp(est_logodds)/(1 + exp(est_logodds))
                                                    grad <- est_prob * (1 - est_prob)
                                                    var_G <- grad * var_logodds_scale * grad
                                                    results_mu_scale <- tibble(est = est_prob, std_err = sqrt(var_G))
                                                    return(results_mu_scale)
                                                  })
  list_all_estimates_by_dp_mu_scale <- append(list_all_estimates_by_dp_mu_scale, list(list_current_estimates_by_dp_mu_scale))
}

all_est_prob <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)
all_est_var <- matrix(rep(NA, .__total_imputed_datasets * 60), nrow = .__total_imputed_datasets, ncol = 60)

for(mi_dataset_num in 1:.__total_imputed_datasets){
  for(dp in 1:60){
    all_est_prob[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["est"]]
    all_est_var[mi_dataset_num, dp] <- list_all_estimates_by_dp_mu_scale[[mi_dataset_num]][[dp]][["std_err"]]^2
  }
}

saveRDS(all_est_prob, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_low_effort_prompt_by_dp.rds"))
saveRDS(all_est_var, file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_low_effort_prompt_by_dp.rds"))

