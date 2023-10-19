rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# This dataset contains data from only those sequences which began
# within the 10-day MRT period but still includes participants whom we 
# determined should be excluded from all analysis (i.e., part of 
# mars_ids_excluded_from_all_analytic_datasets)
# This dataset is where we will grab information on the two question survey
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
# Among participants who have any data from sequences which began within the
# 10-day MRT period, we identified those participants who did not complete
# at least 3 EMA between the 2nd and 9th day inclusive.
# These participant ID's are saved in the variable below.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))
# Dataset with the primary proximal outcome Y
# Note that this dataset has participants who are part of
# mars_ids_excluded_from_all_analytic_datasets already excluded
dat_mars_basic <- readRDS(file = file.path(path_manipulated_data, "dat_mars_basic.rds"))
# We will use ts_coinflip_local from this dataset
dat_mars_derived_time_vars <- readRDS(file = file.path(path_manipulated_data, "dat_mars_derived_time_vars.rds"))
# This dataset has the most recent eligible decision point already calculated
# This will serve as out starting point for creating variables in terms of the
# most recent eligible decision point
# Note that this dataset has participants who are part of
# mars_ids_excluded_from_all_analytic_datasets already excluded
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))
all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)

################################################################################
# Collapse brief completion status into a few levels
################################################################################
dat_analysis[["status_survey_2qs_collapsed"]] <- collapse_survey_2qs_status(cleaned_data_frame = dat_analysis)
dat_analysis[["total_2qs_items_with_response"]] <- count_total_items_with_response_2qs(cleaned_data_frame = dat_analysis)
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, status_survey_2qs_collapsed, total_2qs_items_with_response, cig_available, negative_affect)

################################################################################
# We will create two variables: an indicator for whether the brief survey was
# completed and reported engagement in self-regulatory strategies at the most
# recent eligible decision point
################################################################################
dat_analysis <- left_join(x = dat_mars_basic, y = dat_analysis, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- left_join(x = dat_analysis, y = dat_mars_derived_time_vars, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- left_join(x = dat_analysis, y = scanned_decision_points_within_range %>% select(-eligibility), by = join_by(mars_id == mars_id, decision_point == decision_point))

dat_analysis[["any_response_2qs"]] <- if_else(dat_analysis[["status_survey_2qs_collapsed"]] != "no_response", 1, 0)
dat_analysis[["any_recent_eligible_dp"]] <- if_else(!is.na(dat_analysis[["decision_points_most_recent_eligible"]]), 1, 0)
dat_analysis[["engagement_most_recent_eligible"]] <- NA_real_
dat_analysis[["coinflip_most_recent_eligible"]] <- NA_real_
dat_analysis[["hours_elapsed_since_most_recent_eligible"]] <- NA_real_

list_all_dat <- list()

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  
  for(j in 1:n_blocks){
    if(dat_current_participant[j,"any_recent_eligible_dp"] == 1){
      this_idx <- dat_current_participant[j,"decision_points_most_recent_eligible"]
      dat_current_participant[j,"engagement_most_recent_eligible"] <- dat_current_participant %>% filter(decision_point == this_idx) %>% .[["Y"]]
      dat_current_participant[j,"coinflip_most_recent_eligible"] <- dat_current_participant %>% filter(decision_point == this_idx) %>% .[["coinflip"]]
      past_timestamp <- dat_current_participant %>% filter(decision_point == this_idx) %>% .[["ts_coinflip_local"]]
      present_timestamp <- dat_current_participant[j,"ts_coinflip_local"]
      hours_elapsed_between_timestamps <- int_length(past_timestamp %--% present_timestamp)/(60*60)
      dat_current_participant[j,"hours_elapsed_since_most_recent_eligible"] <- hours_elapsed_between_timestamps
    }else{
      ##########################################################################
      # We will be using Method 1 recommended by Dziak and Henry in the paper
      # cited below where we have an indicator for whether a variable 
      # is relevant (this is any_recent_eligible_dp in our case)
      # and the numeric value of the variable when it 
      # is relevant (this is engagement_most_recent_eligible in our case)
      # In line with Dziak and Henry's Method 1, we will include both of these 
      # variables in our regression model and code 
      # engagement_most_recent_eligible as 0 whenever any_recent_eligible_dp 
      # is zero
      # 
      # Paper:
      # * Dziak, J. J., & Henry, K. L. (2017). Two-part predictors in regression models. Multivariate behavioral research, 52(5), 551-561.
      ##########################################################################
      dat_current_participant[j,"engagement_most_recent_eligible"] <- 0
      dat_current_participant[j,"coinflip_most_recent_eligible"] <- 0
      dat_current_participant[j,"hours_elapsed_since_most_recent_eligible"] <- 0
    }
  }
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

################################################################################
# Select columns needed
################################################################################
dat_analysis <- dat_analysis %>%
  select(mars_id, decision_point,
         status_survey_2qs_collapsed, total_2qs_items_with_response, 
         any_response_2qs, cig_available, negative_affect,
         any_recent_eligible_dp, engagement_most_recent_eligible,
         coinflip_most_recent_eligible, hours_elapsed_since_most_recent_eligible)

################################################################################
# Save output
################################################################################
saveRDS(dat_analysis, file = file.path(path_manipulated_data, "dat_mars_time_varying_noise_reduction_vars.rds"))

