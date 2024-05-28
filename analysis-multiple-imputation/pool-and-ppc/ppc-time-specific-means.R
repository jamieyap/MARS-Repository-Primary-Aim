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
source(file = file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-utils.R"))
library(mice)
library(geepack)
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", 1, paste("dat_long_completed", ".rds", sep = "")))
use_alpha <- 0.05/2

###############################################################################
# Mean among eligible decision points micro-randomized to prompt (any type)
###############################################################################
mi_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_prompt_by_dp.rds"))
mi_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_prompt_by_dp.rds"))
replicates_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_prob_prompt_by_dp.rds"))
replicates_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_var_prompt_by_dp.rds"))

comparison_est <- (replicates_est_prob >= mi_est_prob)
comparison_stderr <- (sqrt(replicates_est_var) >= sqrt(mi_est_var))
ppc_est <- colMeans(comparison_est)
ppc_stderr <- colMeans(comparison_stderr)
dat_ppc <- tibble(decision_point = 1:60, ppc_est = ppc_est, ppc_stderr = ppc_stderr)

list_all_pool_stats <- list()

for(dp in 1:60){
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 1) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est_prob[, dp], 
                             U = mi_est_var[, dp], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est_prob[, dp])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$Qbar_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$Qbar_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% mutate(decision_point = 1:60) %>% select(decision_point, n, Qbar, pooled_stderr, Qbar_conf_int_lb, Qbar_conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_prompt_by_dp.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_prompt_by_dp.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
mi_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_no_prompt_by_dp.rds"))
mi_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_no_prompt_by_dp.rds"))
replicates_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_prob_no_prompt_by_dp.rds"))
replicates_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_var_no_prompt_by_dp.rds"))

comparison_est <- (replicates_est_prob >= mi_est_prob)
comparison_stderr <- (sqrt(replicates_est_var) >= sqrt(mi_est_var))
ppc_est <- colMeans(comparison_est)
ppc_stderr <- colMeans(comparison_stderr)
dat_ppc <- tibble(decision_point = 1:60, ppc_est = ppc_est, ppc_stderr = ppc_stderr)

list_all_pool_stats <- list()

for(dp in 1:60){
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 0) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est_prob[, dp], 
                             U = mi_est_var[, dp], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est_prob[, dp])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$Qbar_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$Qbar_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% mutate(decision_point = 1:60) %>% select(decision_point, n, Qbar, pooled_stderr, Qbar_conf_int_lb, Qbar_conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt_by_dp.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt_by_dp.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
mi_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_high_effort_prompt_by_dp.rds"))
mi_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_high_effort_prompt_by_dp.rds"))
replicates_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_prob_high_effort_prompt_by_dp.rds"))
replicates_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_var_high_effort_prompt_by_dp.rds"))

comparison_est <- (replicates_est_prob >= mi_est_prob)
comparison_stderr <- (sqrt(replicates_est_var) >= sqrt(mi_est_var))
ppc_est <- colMeans(comparison_est)
ppc_stderr <- colMeans(comparison_stderr)
dat_ppc <- tibble(decision_point = 1:60, ppc_est = ppc_est, ppc_stderr = ppc_stderr)

list_all_pool_stats <- list()

for(dp in 1:60){
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_high_effort == 1) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est_prob[, dp], 
                             U = mi_est_var[, dp], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est_prob[, dp])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$Qbar_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$Qbar_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% mutate(decision_point = 1:60) %>% select(decision_point, n, Qbar, pooled_stderr, Qbar_conf_int_lb, Qbar_conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt_by_dp.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt_by_dp.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
mi_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_prob_low_effort_prompt_by_dp.rds"))
mi_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_var_low_effort_prompt_by_dp.rds"))
replicates_est_prob <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_prob_low_effort_prompt_by_dp.rds"))
replicates_est_var <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_var_low_effort_prompt_by_dp.rds"))

comparison_est <- (replicates_est_prob >= mi_est_prob)
comparison_stderr <- (sqrt(replicates_est_var) >= sqrt(mi_est_var))
ppc_est <- colMeans(comparison_est)
ppc_stderr <- colMeans(comparison_stderr)
dat_ppc <- tibble(decision_point = 1:60, ppc_est = ppc_est, ppc_stderr = ppc_stderr)

list_all_pool_stats <- list()

for(dp in 1:60){
  num_participants <- dat_long_completed %>% filter(decision_point == dp) %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_low_effort == 1) %>% nrow(.)
  pool_manual <- pool.scalar(Q = mi_est_prob[, dp], 
                             U = mi_est_var[, dp], 
                             n = num_participants, 
                             k = 1)
  
  pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participants - 1, pool_manual = pool_manual)
  pool_stats$n <- num_participants
  pool_stats$Qbar <- mean(mi_est_prob[, dp])
  pool_stats$pooled_stderr <- sqrt(pool_stats$total_var)
  pool_stats$Qbar_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  pool_stats$Qbar_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$pooled_stderr
  list_all_pool_stats <- append(list_all_pool_stats, list(pool_stats))
}

dat_all_pool_stats <- bind_rows(list_all_pool_stats)
dat_all_pool_stats <- dat_all_pool_stats %>% mutate(decision_point = 1:60) %>% select(decision_point, n, Qbar, pooled_stderr, Qbar_conf_int_lb, Qbar_conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- dat_all_pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt_by_dp.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt_by_dp.csv"), row.names = FALSE)

