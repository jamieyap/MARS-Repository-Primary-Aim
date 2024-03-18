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
                             y = dat_mars_time_varying_covariates, 
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

###############################################################################
# Save dataset
###############################################################################
saveRDS(dat_primary_aim, file = file.path(path_manipulated_data, "dat_primary_aim.rds"))

