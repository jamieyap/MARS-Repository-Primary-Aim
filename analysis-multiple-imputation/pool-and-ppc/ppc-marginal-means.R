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
mi_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_prompt.rds"))
mi_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_prompt.rds"))
replicates_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_prompt.rds"))
replicates_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_prompt.rds"))

comparison_est <- ((exp(replicates_est_logodds)/(1 + exp(replicates_est_logodds))) >= (exp(mi_est_logodds)/(1 + exp(mi_est_logodds))))
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(ppc_est = ppc_est)

num_participant_decision_points <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 1) %>% nrow(.)
pool_manual <- pool.scalar(Q = mi_est_logodds[, 1], 
                           U = mi_var_logodds[, 1], 
                           n = num_participant_decision_points, 
                           k = 1)

pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participant_decision_points - 1, pool_manual = pool_manual)
pool_stats$n <- num_participant_decision_points
pool_stats$Qbar <- mean(mi_est_logodds[, 1])
pool_stats$logodds_scale_pooled_stderr <- sqrt(pool_stats$total_var)
pool_stats$logodds_scale_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$logodds_scale_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$est_prob <- exp(pool_stats$Qbar)/(1 + exp(pool_stats$Qbar))
pool_stats$conf_int_lb <- exp(pool_stats$logodds_scale_conf_int_lb)/(1 + exp(pool_stats$logodds_scale_conf_int_lb))
pool_stats$conf_int_ub <- exp(pool_stats$logodds_scale_conf_int_ub)/(1 + exp(pool_stats$logodds_scale_conf_int_ub))

pool_stats <- pool_stats %>% 
  select(n, Qbar, logodds_scale_pooled_stderr, logodds_scale_conf_int_lb, logodds_scale_conf_int_ub, est_prob, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_prompt.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_prompt.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
mi_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_no_prompt.rds"))
mi_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_no_prompt.rds"))
replicates_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_no_prompt.rds"))
replicates_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_no_prompt.rds"))

comparison_est <- ((exp(replicates_est_logodds)/(1 + exp(replicates_est_logodds))) >= (exp(mi_est_logodds)/(1 + exp(mi_est_logodds))))
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(ppc_est = ppc_est)

num_participant_decision_points <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(coinflip == 0) %>% nrow(.)
pool_manual <- pool.scalar(Q = mi_est_logodds[, 1], 
                           U = mi_var_logodds[, 1], 
                           n = num_participant_decision_points, 
                           k = 1)

pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participant_decision_points - 1, pool_manual = pool_manual)
pool_stats$n <- num_participant_decision_points
pool_stats$Qbar <- mean(mi_est_logodds[, 1])
pool_stats$logodds_scale_pooled_stderr <- sqrt(pool_stats$total_var)
pool_stats$logodds_scale_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$logodds_scale_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$est_prob <- exp(pool_stats$Qbar)/(1 + exp(pool_stats$Qbar))
pool_stats$conf_int_lb <- exp(pool_stats$logodds_scale_conf_int_lb)/(1 + exp(pool_stats$logodds_scale_conf_int_lb))
pool_stats$conf_int_ub <- exp(pool_stats$logodds_scale_conf_int_ub)/(1 + exp(pool_stats$logodds_scale_conf_int_ub))

pool_stats <- pool_stats %>% 
  select(n, Qbar, logodds_scale_pooled_stderr, logodds_scale_conf_int_lb, logodds_scale_conf_int_ub, est_prob, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
mi_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_high_effort_prompt.rds"))
mi_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_high_effort_prompt.rds"))
replicates_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_high_effort_prompt.rds"))
replicates_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_high_effort_prompt.rds"))

comparison_est <- ((exp(replicates_est_logodds)/(1 + exp(replicates_est_logodds))) >= (exp(mi_est_logodds)/(1 + exp(mi_est_logodds))))
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(ppc_est = ppc_est)

num_participant_decision_points <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_high_effort == 1) %>% nrow(.)
pool_manual <- pool.scalar(Q = mi_est_logodds[, 1], 
                           U = mi_var_logodds[, 1], 
                           n = num_participant_decision_points, 
                           k = 1)

pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participant_decision_points - 1, pool_manual = pool_manual)
pool_stats$n <- num_participant_decision_points
pool_stats$Qbar <- mean(mi_est_logodds[, 1])
pool_stats$logodds_scale_pooled_stderr <- sqrt(pool_stats$total_var)
pool_stats$logodds_scale_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$logodds_scale_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$est_prob <- exp(pool_stats$Qbar)/(1 + exp(pool_stats$Qbar))
pool_stats$conf_int_lb <- exp(pool_stats$logodds_scale_conf_int_lb)/(1 + exp(pool_stats$logodds_scale_conf_int_lb))
pool_stats$conf_int_ub <- exp(pool_stats$logodds_scale_conf_int_ub)/(1 + exp(pool_stats$logodds_scale_conf_int_ub))

pool_stats <- pool_stats %>% 
  select(n, Qbar, logodds_scale_pooled_stderr, logodds_scale_conf_int_lb, logodds_scale_conf_int_ub, est_prob, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
mi_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_est_logodds_low_effort_prompt.rds"))
mi_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "mi_var_logodds_low_effort_prompt.rds"))
replicates_est_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_est_logodds_low_effort_prompt.rds"))
replicates_var_logodds <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "mi-analysis-results", "replicates_var_logodds_low_effort_prompt.rds"))

comparison_est <- ((exp(replicates_est_logodds)/(1 + exp(replicates_est_logodds))) >= (exp(mi_est_logodds)/(1 + exp(mi_est_logodds))))
ppc_est <- colMeans(comparison_est)
dat_ppc <- tibble(ppc_est = ppc_est)

num_participant_decision_points <- dat_long_completed %>% filter(replicate_id == 0) %>% filter(eligibility == 1) %>% filter(is_low_effort == 1) %>% nrow(.)
pool_manual <- pool.scalar(Q = mi_est_logodds[, 1], 
                           U = mi_var_logodds[, 1], 
                           n = num_participant_decision_points, 
                           k = 1)

pool_stats <- calculate_pool_statistics2(degrees_of_freedom = num_participant_decision_points - 1, pool_manual = pool_manual)
pool_stats$n <- num_participant_decision_points
pool_stats$Qbar <- mean(mi_est_logodds[, 1])
pool_stats$logodds_scale_pooled_stderr <- sqrt(pool_stats$total_var)
pool_stats$logodds_scale_conf_int_lb <- pool_stats$Qbar - qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$logodds_scale_conf_int_ub <- pool_stats$Qbar + qnorm(p = use_alpha, lower.tail = FALSE) * pool_stats$logodds_scale_pooled_stderr
pool_stats$est_prob <- exp(pool_stats$Qbar)/(1 + exp(pool_stats$Qbar))
pool_stats$conf_int_lb <- exp(pool_stats$logodds_scale_conf_int_lb)/(1 + exp(pool_stats$logodds_scale_conf_int_lb))
pool_stats$conf_int_ub <- exp(pool_stats$logodds_scale_conf_int_ub)/(1 + exp(pool_stats$logodds_scale_conf_int_ub))

pool_stats <- pool_stats %>% 
  select(n, Qbar, logodds_scale_pooled_stderr, logodds_scale_conf_int_lb, logodds_scale_conf_int_ub, est_prob, conf_int_lb, conf_int_ub, everything())

dat_ppc <- dat_ppc %>% round(., digits = 3)
dat_all_pool_stats <- pool_stats %>% round(., digits = 3)

write.csv(dat_ppc, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt.csv"), row.names = FALSE)
write.csv(dat_all_pool_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt.csv"), row.names = FALSE)


