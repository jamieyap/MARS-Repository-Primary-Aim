###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters for generating the current completed dataset
###############################################################################
mi_dataset_num <- .__par_mi_number
use_maxit_value <- .__par_maxit_value

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_block1.rds"))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with("_dp2"))

dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
dat_wide_init <- dat_wide

################################################################################
# How many replicates do we have?
################################################################################
all_replicate_ids <- unique(dat_wide[["replicate_id"]])
maximum_replicate_id <- max(all_replicate_ids)
minimum_replicate_id <- min(all_replicate_ids)

###############################################################################
# Step 0. Update completed dataset
###############################################################################
if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, "any_recent_eligible_dp_dp2"] == 1){
        matched_dp <- dat_current[this_participant, "matched_recent_dp2"]
        matched_value <- dat_current[this_participant, paste("Y_dp", matched_dp, sep = "")]
        matched_value <- as.numeric(matched_value) - 1
        dat_current[this_participant, "engagement_most_recent_eligible_dp2"] <- matched_value
      }
    }
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, "any_recent_eligible_dp_dp2"] == 1){
      matched_dp <- dat_wide[this_participant, "matched_recent_dp2"]
      matched_value <- dat_wide[this_participant, paste("Y_dp", matched_dp, sep = "")]
      matched_value <- as.numeric(matched_value) - 1
      dat_wide[this_participant, "engagement_most_recent_eligible_dp2"] <- matched_value
    }
  }
}

###############################################################################
# Step 1. Impute responses to 2qs
#
# Specify a regression model for each variable individually.
# Since we only wish to impute responses to 2qs at this step, our list of 
# formulas only consist of the regression model for response to 2qs
# Other variables have missing values but since we did not specify a model 
# for them at this step, the mice package will not impute them
###############################################################################
my_list <- list()
my_list[["quick_survey_response_dp2"]] <- as.formula(paste("quick_survey_response_dp2 ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + ffmq_nonjudge"))

restriction_meet_string <- "eligibility_dp2 == 1"
restriction_violate_string <- "eligibility_dp2 == 0"

rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 1,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
dat_wide_completed <- rbind(rows_meet_restriction_completed, rows_violate_restriction)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_quick_survey_response_dp2.rds"))

###############################################################################
# Step 2. Impute primary proximal outcome (engagement in 
# self-regulatory strategies) and other variables assessed in EMA
###############################################################################
dat_wide <- dat_wide_completed

my_list <- list()
my_list[["Y_dp2"]] <- as.formula(paste("Y_dp2 ~ is_low_effort_dp2 + is_high_effort_dp2 + any_recent_eligible_dp_dp2 + engagement_most_recent_eligible_dp2 + cigarette_counts_dp2 + src_scored_dp2"))
my_list[["cigarette_counts_dp2"]] <- as.formula(paste("cigarette_counts_dp2 ~ is_low_effort_dp2 + is_high_effort_dp2 + Y_dp2 + src_scored_dp2 + quick_survey_response_dp2 + baseline_tobacco_history + income_val"))
my_list[["src_scored_dp2"]] <- as.formula(paste("src_scored_dp2 ~ is_low_effort_dp2 + is_high_effort_dp2 + Y_dp2 + cigarette_counts_dp2"))

restriction_meet_string <- "eligibility_dp2 == 1"
restriction_violate_string <- "eligibility_dp2 == 0"

rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 

dat_wide_completed <- rbind(rows_meet_restriction_completed, rows_violate_restriction)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_ema_response_dp2.rds"))

###############################################################################
# Save
###############################################################################
saveRDS(dat_wide_init, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_init_block2.rds"))
saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_block2.rds"))

