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
# Construct new variables using tracked variables
################################################################################
dat_matched_conditions <- readRDS(file = file.path(path_manipulated_data, "dat_matched_conditions.rds"))
dat_matched_conditions <- dat_matched_conditions %>% 
  select(mars_id, decision_point, 
         privacy_data_not_found_quick_survey, driving_data_not_found_quick_survey, 
         privacy_data_not_found_rand, driving_data_not_found_rand,
         privacy_data_not_found_ema, driving_data_not_found_ema) %>%
  mutate(driving_data_not_found_combined = if_else((driving_data_not_found_quick_survey == 0) & (driving_data_not_found_rand == 0), 1, 0))
dat_primary_aim <- left_join(x = dat_primary_aim, y = dat_matched_conditions, by = join_by(mars_id == mars_id, decision_point == decision_point))

dat_primary_aim <- dat_primary_aim %>% 
  mutate(is_complete_v1_quest = if_else(v1_baseline_quest_complete == "Complete", 1, 0))

dat_primary_aim <- dat_primary_aim %>%
  mutate(days_between_v1_and_coinflip_local_squared = days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local,
         any_app_usage_preblock = if_else((in_mars_preblock == 1) | (in_tips_preblock == 1), 1, 0),
         total_app_usage_time_spent_preblock = (time_spent_preblock + time_spent_tips_preblock)/1000) # time_spent_preblock and time_spent_tips_preblock are in milliseconds; dividing the sum will let the resulting variable be in seconds

################################################################################
# Create lagged variables
################################################################################
dat_primary_aim <- dat_primary_aim %>% 
  group_by(participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  mutate(Y_lag1 = lag(Y),
         cigarette_counts_lag1 = lag(cigarette_counts),
         src_scored_lag1 = lag(src_scored),
         cigarette_availability_lag1 = lag(cigarette_availability),
         wearing_patch_lag1 = lag(wearing_patch)) %>%
  ungroup(.)

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
    dat_replicate <- dat_primary_aim %>% 
      mutate(replicate_id = idx) %>%
      mutate(Y = NA,
             cigarette_counts = NA,
             src_scored = NA,
             cigarette_availability = NA,
             wearing_patch = NA)
    list_dat_all <- append(list_dat_all, list(dat_replicate))
  }
  
  dat_primary_aim <- bind_rows(list_dat_all)
}

################################################################################
# Save
################################################################################
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))

