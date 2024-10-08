###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Prepare for pooling
###############################################################################
source("paths.R")
source(file = file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-utils.R"))
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
# When running this script within imputation-pipeline.R, the package MASS may still be loaded in the global environment
select <- dplyr::select 

dat_secondary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))
all_ids <- unique(dat_secondary_aim[["mars_id"]])
num_participants <- length(all_ids)

###############################################################################
# Workflow: Pool results for secondary aim
###############################################################################
results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", 1, "sensitivity2_results_obj_secondary_marginal_risk_difference.rds"))
these_row_names <- results_obj[["variable"]]
num_terms <- nrow(results_obj)

list_pooled_est <- list()
list_pooled_std_err <- list()
list_pool_manual_output <- list()
list_pool_stats <- list()

for(j in 1:num_terms){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "sensitivity2_results_obj_secondary_marginal_risk_difference.rds"))
    
    if(length(results_obj) == 1){
      list_Q[[mi_dataset_num]] <- NULL
      list_U[[mi_dataset_num]] <- NULL
    }else{
      list_Q[[mi_dataset_num]] <- results_obj[j,"estimates"]
      list_U[[mi_dataset_num]] <- (results_obj[j,"std_err"])^2
    }
  }
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants)
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - nrow(results_obj) - 1, pool_manual = pool_manual)
  list_pool_stats <- append(list_pool_stats, list(pool_stats))
  
  list_pool_manual_output <- append(list_pool_manual_output, list(pool_manual))
  list_pooled_est <- append(list_pooled_est, pool_manual$qbar)
  list_pooled_std_err <- append(list_pooled_std_err, sqrt(pool_manual$t))
}

fit_pooled <- data.frame(Estimate = unlist(list_pooled_est), StdErr = unlist(list_pooled_std_err), LCL = NA_real_, UCL = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)

row.names(fit_pooled) <- these_row_names

# Causal part of the analysis model -------------------------------------------
fit_pooled_causal <- fit_pooled[2:3,]

# Control part of the analysis model ------------------------------------------
fit_pooled_control <- fit_pooled[c(1, 4:num_terms),]

dat_pool_stats <- bind_rows(list_pool_stats)
row.names(dat_pool_stats) <- results_obj[["variable"]]
dat_pool_stats <- dat_pool_stats[2:3,]

###############################################################################
# Workflow: pool results for contrast
###############################################################################
list_pooled_est <- list()
list_pooled_std_err <- list()
list_pool_manual_output <- list()
list_pool_stats <- list()

for(j in 1:3){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "sensitivity2_contrast_secondary_marginal_risk_difference.rds"))
    
    if(length(results_obj) == 1){
      list_Q[[mi_dataset_num]] <- NULL
      list_U[[mi_dataset_num]] <- NULL
    }else{
      list_Q[[mi_dataset_num]] <- results_obj[j,"estimates"]
      list_U[[mi_dataset_num]] <- (results_obj[j,"std_err"])^2
    }
  }
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants)
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - nrow(results_obj) - 1, pool_manual = pool_manual)
  list_pool_stats <- append(list_pool_stats, list(pool_stats))
  
  list_pool_manual_output <- append(list_pool_manual_output, list(pool_manual))
  list_pooled_est <- append(list_pooled_est, pool_manual$qbar)
  list_pooled_std_err <- append(list_pooled_std_err, sqrt(pool_manual$t))
}

fit_pooled <- data.frame(Estimate = unlist(list_pooled_est), StdErr = unlist(list_pooled_std_err), LCL = NA_real_, UCL = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)

fit_pooled_contrast <- fit_pooled
row.names(fit_pooled_contrast) <- results_obj[["contrast"]]

dat_pool_stats_contrast <- bind_rows(list_pool_stats)
row.names(dat_pool_stats_contrast) <- results_obj[["contrast"]]

###############################################################################
# Save output
###############################################################################
fit_pooled_causal_formatted <- format(round(fit_pooled_causal, 3), nsmall = 3)
fit_pooled_control_formatted <- format(round(fit_pooled_control, 3), nsmall = 3)
fit_pooled_contrast_formatted <- format(round(fit_pooled_contrast, 3), nsmall = 3)

dat_pool_stats_formatted <- format(round(dat_pool_stats, 5), nsmall = 5)
dat_pool_stats_contrast_formatted <- format(round(dat_pool_stats_contrast, 5), nsmall = 5)

write.csv(fit_pooled_causal_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity2_pooled_H2_causal_risk_difference_scale.csv"), row.names = TRUE)
write.csv(fit_pooled_control_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity2_pooled_H2_control_risk_difference_scale.csv"), row.names = TRUE)
write.csv(fit_pooled_contrast_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity2_pooled_H2_contrast_risk_difference_scale.csv"), row.names = TRUE)

write.csv(dat_pool_stats_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity2_pool_stats_H2_causal_risk_difference_scale.csv"), row.names = TRUE)
write.csv(dat_pool_stats_contrast_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity2_pool_stats_H2_contrast_risk_difference_scale.csv"), row.names = TRUE)

