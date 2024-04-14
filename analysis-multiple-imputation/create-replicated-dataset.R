###############################################################################
# Input arguments to this script
###############################################################################
rm(list = ls())
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

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
all_ids <- unique(dat_primary_aim[["mars_id"]])

################################################################################
# Create lagged variables
################################################################################
dat_primary_aim <- dat_primary_aim %>% 
  group_by(participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  mutate(Y_lag1 = lag(Y),
         src_scored_lag1 = lag(src_scored),
         cigarette_availability_lag1 = lag(cigarette_availability),
         wearing_patch_lag1 = lag(wearing_patch),
         cigarette_counts_lag1 = lag(cigarette_counts)) %>%
  ungroup(.)

################################################################################
# Create other variables
################################################################################
dat_primary_aim <- dat_primary_aim %>%
  mutate(days_between_v1_and_coinflip_local_squared = days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local,
         any_app_usage_preblock = if_else((in_mars_preblock == 1) | (in_tips_preblock == 1), 1, 0),
         total_app_usage_time_spent_preblock = (time_spent_preblock + time_spent_tips_preblock)/1000) # time_spent_preblock and time_spent_tips_preblock are in milliseconds; dividing the sum will let the resulting variable be in seconds

################################################################################
# Create replicates
################################################################################
dat_primary_aim <- dat_primary_aim %>% 
  mutate(replicate_id = 0) %>% 
  select(replicate_id, everything())

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

