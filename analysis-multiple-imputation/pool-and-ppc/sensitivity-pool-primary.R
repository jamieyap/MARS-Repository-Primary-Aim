###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Prepare for pooling
###############################################################################
source("paths.R")
source(file.path(path_code, "analysis-multiple-imputation", "pool-and-ppc", "pool-utils.R"))
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

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
  results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "sensitivity_results_obj_primary_marginal.rds"))
  
  if(class(results_obj) == "character"){
    list_Q[[mi_dataset_num]] <- NULL
    list_U[[mi_dataset_num]] <- NULL
  }else{
    list_Q[[mi_dataset_num]] <- results_obj$causal_excursion_effect[1,"Estimate"]
    list_U[[mi_dataset_num]] <- (results_obj$causal_excursion_effect[1,"StdErr"])^2
  }
}

pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants, k = 1)
pool_stats <- calculate_pool_statistics(results_obj = results_obj, pool_manual = pool_manual)

fit_pooled <- data.frame(Estimate = pool_manual$qbar, StdErr = sqrt(pool_manual$t), LCL = NA_real_, UCL = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)

row.names(fit_pooled) <- "Treatment (Prompt = 1, No Prompt = 0)"
fit_pooled_causal <- fit_pooled
print(fit_pooled_causal)

row.names(pool_stats) <- "Treatment (Prompt = 1, No Prompt = 0)"

# Control part of the analysis model ------------------------------------------

# The purpose of the next few lines is to count the number of control variables;
# the count will already include the intercept term so there is no need to add 1
results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", 1, "sensitivity_results_obj_primary_marginal.rds"))
num_terms <- nrow(results_obj$control_variables)

list_pooled_est <- list()
list_pooled_std_err <- list()

for(j in 1:num_terms){
  list_Q <- list()
  list_U <- list()
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "sensitivity_results_obj_primary_marginal.rds"))
    
    if(class(results_obj) == "character"){
      list_Q[[mi_dataset_num]] <- NULL
      list_U[[mi_dataset_num]] <- NULL
    }else{
      list_Q[[mi_dataset_num]] <- results_obj$control_variables[j,"Estimate"]
      list_U[[mi_dataset_num]] <- (results_obj$control_variables[j,"StdErr"])^2
    }
  }
  
  pool_manual <- pool.scalar(Q = unlist(list_Q), U = unlist(list_U), n = num_participants, k = 1)
  list_pooled_est <- append(list_pooled_est, pool_manual$qbar)
  list_pooled_std_err <- append(list_pooled_std_err, sqrt(pool_manual$t))
}

fit_pooled <- data.frame(Estimate = unlist(list_pooled_est), StdErr = unlist(list_pooled_std_err), LCL = NA_real_, UCL = NA_real_, p_value = NA_real_)
fit_pooled[["LCL"]] <- fit_pooled[["Estimate"]] - fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["UCL"]] <- fit_pooled[["Estimate"]] + fit_pooled[["StdErr"]] * qnorm(0.975)
fit_pooled[["p_value"]] <- 2*pnorm(abs(fit_pooled[["Estimate"]]/fit_pooled[["StdErr"]]), lower.tail = FALSE)

row.names(fit_pooled) <- row.names(results_obj$control_variables)
fit_pooled_control <- fit_pooled

###############################################################################
# Workflow: Posterior predictive check for primary aim causal parameters
###############################################################################
list_all_comparisons_est <- list()
list_all_comparisons_stderr <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
 results_obj <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, "sensitivity_results_obj_primary_marginal.rds"))
 
 if(class(results_obj) == "character"){
   list_all_comparisons_est <- append(list_all_comparisons_est, list(NULL))
   list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(NULL))
   next
 }
 
 for(idx_replicate in 1:.__par_total_replicates){
   results_obj_rep <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", mi_dataset_num, paste("sensitivity_results_obj_primary_marginal", "_replicate_", idx_replicate, ".rds", sep = "")))
   
   if((class(results_obj) == "character") | (class(results_obj_rep) == "character")){
     list_all_comparisons_est <- append(list_all_comparisons_est, list(NULL))
     list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(NULL)) 
   }else{
     dat_this_comparison <- (results_obj_rep$causal_excursion_effect >= results_obj$causal_excursion_effect)
     dat_this_comparison <- as.data.frame(dat_this_comparison)
     
     these_cols <- c("Estimate")
     dat_this_comparison_est <- dat_this_comparison %>% select(all_of(these_cols))
     list_all_comparisons_est <- append(list_all_comparisons_est, list(dat_this_comparison_est))
     
     these_cols <- c("StdErr")
     dat_this_comparison_stderr <- dat_this_comparison %>% select(all_of(these_cols))
     list_all_comparisons_stderr <- append(list_all_comparisons_stderr, list(dat_this_comparison_stderr)) 
   }
 }
}

dat_all_comparisons_est <- bind_cols(list_all_comparisons_est)
dat_all_comparisons_stderr <- bind_cols(list_all_comparisons_stderr)

pbcom_est <- rowMeans(dat_all_comparisons_est)
pbcom_stderr <- rowMeans(dat_all_comparisons_stderr)
dat_pbcom <- data.frame(pbcom_est, pbcom_stderr)

###############################################################################
# Save output
###############################################################################
fit_pooled_causal_formatted <- format(round(fit_pooled_causal, 3), nsmall = 3)
fit_pooled_control_formatted <- format(round(fit_pooled_control, 3), nsmall = 3)
dat_pbcom_formatted <- format(round(dat_pbcom, 3), nsmall = 3)
pool_stats_formatted <- format(round(pool_stats, 5), nsmall = 5)

write.csv(fit_pooled_causal_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity_pooled_H1_causal.csv"), row.names = TRUE)
write.csv(fit_pooled_control_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity_pooled_H1_control.csv"), row.names = TRUE)
write.csv(dat_pbcom_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity_pbcom_H1_causal.csv"), row.names = TRUE)
write.csv(pool_stats_formatted, file = file.path("analysis-multiple-imputation", "formatted-output", "sensitivity_pool_stats_H1_causal.csv"), row.names = TRUE)
