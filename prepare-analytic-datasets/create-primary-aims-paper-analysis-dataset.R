rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

dat_mars_basic <- readRDS(file = file.path(path_manipulated_data, "dat_mars_basic.rds"))
dat_mars_derived_time_vars <- readRDS(file = file.path(path_manipulated_data, "dat_mars_derived_time_vars.rds"))
dat_mars_time_varying_noise_reduction_vars <- readRDS(file = file.path(path_manipulated_data, "dat_mars_time_varying_noise_reduction_vars.rds"))
dat_mars_time_varying_moderators <- readRDS(file = file.path(path_manipulated_data, "dat_mars_time_varying_moderators.rds"))
dat_mars_time_varying_covariates <- readRDS(file = file.path(path_manipulated_data, "dat_mars_time_varying_covariates.rds"))
dat_mars_coded_demogs <- readRDS(file = file.path(path_manipulated_data, "dat_mars_coded_demogs.rds"))
dat_mars_baseline_moderators <- readRDS(file = file.path(path_manipulated_data, "dat_mars_baseline_moderators.rds"))
dat_mars_proximal_cigarette_smoking <- readRDS(file = file.path(path_proximal_smoking_pipeline_data, "dat_mars_proximal_cigarette_smoking.rds"))
dat_mars_proximal_vaping <- readRDS(file = file.path(path_proximal_smoking_pipeline_data, "dat_mars_proximal_vaping.rds"))

# This is app usage data
dat_app_usage <- readRDS(file = file.path(path_app_usage_data, "app_usage_for_mainpipeline_updated0212.rds"))
these_app_usage_vars <- c("ts_emi_resp", "emi_resp", 
                          "num_page", "num_click", "in_mars", "activ_started", "activ_done", "activ_done_m", "time_spent",
                          "num_page_p", "num_click_p", "in_mars_p", "activ_started_p", "activ_done_p", "activ_done_m_p", "time_spent_p",
                          "num_message", "in_tips", "read_tips", "read_tips_m", "time_spent_tips", "tips_comp_time", "tips_comp_wps",
                          "num_message_p", "in_tips_p", "read_tips_p", "read_tips_m_p", "time_spent_tips_p", "tips_comp_time_p", "tips_comp_wps_p",
                          "num_page_preblock", "num_click_preblock", "in_mars_preblock", "activ_started_preblock", "activ_done_preblock", "activ_done_m_preblock", "time_spent_preblock",
                          "num_message_preblock", "in_tips_preblock", "read_tips_preblock", "read_tips_m_preblock", "time_spent_tips_preblock", "tips_comp_time_preblock", "tips_comp_wps_preblock")

dat_app_usage <- dat_app_usage %>% select(mars_id, decision_point, all_of(these_app_usage_vars))

###############################################################################
# This data frame contains variables for testing primary, secondary, and
# exploratory hypotheses concerning time
###############################################################################
dat_primary_aim <- left_join(x = dat_mars_basic, 
                             y = dat_mars_derived_time_vars, 
                             by = join_by(mars_id == mars_id,
                                          decision_point == decision_point))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_time_varying_noise_reduction_vars, 
                             by = join_by(mars_id == mars_id,
                                          decision_point == decision_point))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_time_varying_moderators, 
                             by = join_by(mars_id == mars_id,
                                          decision_point == decision_point))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_time_varying_covariates %>% select(-eligibility), 
                             by = join_by(mars_id == mars_id,
                                          decision_point == decision_point))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_proximal_cigarette_smoking, 
                             by = join_by(mars_id == mars_id,
                                          decision_point == decision_point))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_proximal_vaping, 
                             by = join_by(mars_id == mars_id,
                                          decision_point == decision_point))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_coded_demogs, 
                             by = join_by(mars_id == mars_id))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_mars_baseline_moderators, 
                             by = join_by(mars_id == mars_id))

dat_primary_aim <- left_join(x = dat_primary_aim, 
                             y = dat_app_usage, 
                             by = join_by(mars_id == mars_id, decision_point == decision_point))

###############################################################################
# Save dataset
###############################################################################
saveRDS(dat_primary_aim, file = file.path(path_manipulated_data, "dat_primary_aim.rds"))

