###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Prepare for pooling
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))
all_ids <- unique(dat_primary_aim[["mars_id"]])
num_participants <- length(all_ids)

###############################################################################
# Workflow: Pool results for primary aim
###############################################################################

# Causal part of the analysis model -------------------------------------------
list_Q <- list()
list_U <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_marginal.rds"))
  list_Q[[mi_dataset_num]] <- results_obj$causal_excursion_effect[1,"Estimate"]
  list_U[[mi_dataset_num]] <- (results_obj$causal_excursion_effect[1,"StdErr"])^2
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants)
}

fit_pooled <- data.frame(Estimate = pool_manual$qbar, StdErr = sqrt(pool_manual$t), LCL = NA_real_, UCL = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)

row.names(fit_pooled) <- "Treatment (Prompt = 1, No Prompt = 0)"
fit_pooled_causal <- fit_pooled

# Control part of the analysis model ------------------------------------------
results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", 1, "results_obj_primary_marginal.rds"))
num_terms <- nrow(results_obj$control_variables)

list_pooled_est <- list()
list_pooled_std_err <- list()
list_pool_manual_output <- list()

for(j in 1:num_terms){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_marginal.rds"))
    
    list_Q[[mi_dataset_num]] <- results_obj$control_variables[j,"Estimate"]
    list_U[[mi_dataset_num]] <- (results_obj$control_variables[j,"StdErr"])^2
  }
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants)
  list_pool_manual_output <- append(list_pool_manual_output, list(pool_manual))
  list_pooled_est <- append(list_pooled_est, pool_manual$qbar)
  list_pooled_std_err <- append(list_pooled_std_err, sqrt(pool_manual$t))
}

fit_pooled <- data.frame(Estimate = unlist(list_pooled_est), StdErr = unlist(list_pooled_std_err), LCL = NA_real_, UCL = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)

row.names(fit_pooled) <- row.names(results_obj$control_variables)
fit_pooled_control <- fit_pooled

# Prepare to save output ------------------------------------------------------
fit_pooled_causal <- format(round(fit_pooled_causal, 3), nsmall = 3)
fit_pooled_control <- format(round(fit_pooled_control, 3), nsmall = 3)

write.csv(fit_pooled_causal, file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal.csv"), row.names = TRUE)
write.csv(fit_pooled_control, file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_control.csv"), row.names = TRUE)

###############################################################################
# Workflow: Posterior predictive check for primary aim
###############################################################################
list_all_comparisons_est <- list()
list_all_comparisons_stderr <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  idx_replicate <- 1
  results_obj_rep <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("results_obj_primary_marginal", "_replicate_", idx_replicate, ".rds", sep = "")))
  results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "results_obj_primary_marginal.rds"))
  
 dat_this_comparison <- (results_obj_rep$causal_excursion_effect >= results_obj$causal_excursion_effect)
 dat_this_comparison <- as.data.frame(dat_this_comparison)
 
 these_cols <- c("Estimate")
 dat_this_comparison_est <- dat_this_comparison %>% select(all_of(these_cols))
 list_all_comparisons_est <- append(list_all_comparisons_est, list(dat_this_comparison_est))
 
 these_cols <- c("StdErr")
 dat_this_comparison_stderr <- dat_this_comparison %>% select(all_of(these_cols))
 list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(dat_this_comparison_stderr))
}

dat_all_comparisons_est <- bind_cols(list_all_comparisons_est)
dat_all_comparisons_stderr <- bind_cols(list_all_comparisons_stderr)

pbcom_est <- rowMeans(dat_all_comparisons_est)
pbcom_stderr <- rowMeans(dat_all_comparisons_stderr)

dat_pbcom <- data.frame(pbcom_est, pbcom_stderr)
dat_pbcom <- format(round(dat_pbcom, 3), nsmall = 3)
write.csv(dat_pbcom, file = file.path("analysis-multiple-imputation", "formatted-output", "pbcom_H1_causal.csv"), row.names = TRUE)
