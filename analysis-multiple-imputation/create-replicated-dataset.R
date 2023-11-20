###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters
###############################################################################
total_replicates <- .__par_total_replicates

################################################################################
# Load packages and datasets
################################################################################
source("paths.R")
library(tidyverse)

dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))

################################################################################
# Load more datasets
#
# Note: Categorical variables in dat_mars_mi_time_varying_covariates that
# will be imputed have already been converted from numeric to factor
################################################################################
dat_mars_mi_time_varying_covariates <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_mars_mi_time_varying_covariates.rds"))

################################################################################
# Create long format dataset that we will begin with in our workflow for
# creating multiply imputed datasets
################################################################################
cols_dat_primary_aim <- colnames(dat_primary_aim)
cols_dat_mars_mi_time_varying_covariates <- colnames(dat_mars_mi_time_varying_covariates)
cols_in_common <- setdiff(intersect(cols_dat_primary_aim, cols_dat_mars_mi_time_varying_covariates), c("mars_id","decision_point"))

dat_mars_mi_time_varying_covariates <- dat_mars_mi_time_varying_covariates %>% select(-any_of(cols_in_common))
dat_primary_aim <- left_join(x = dat_primary_aim, y = dat_mars_mi_time_varying_covariates, by = join_by(mars_id == mars_id, decision_point == decision_point))

################################################################################
# Create replicates
################################################################################
dat_primary_aim <- dat_primary_aim %>% mutate(replicate_id = 0) %>% select(replicate_id, everything())

if(total_replicates > 0){
  list_dat_all <- list()
  list_dat_all <- append(list_dat_all, list(dat_primary_aim))
  
  for(idx in 1:total_replicates){
    dat_replicate <- dat_primary_aim %>% mutate(replicate_id = idx)
    list_dat_all <- append(list_dat_all, list(dat_replicate))
  }
  
  dat_primary_aim <- bind_rows(list_dat_all)
}

################################################################################
# Save
################################################################################
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))

