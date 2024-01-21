rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
dat_primary_aim <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))

################################################################################
# How many replicates do we have?
################################################################################
all_replicate_ids <- unique(dat_primary_aim[["replicate_id"]])
maximum_replicate_id <- max(all_replicate_ids)

################################################################################
# Variables assessed at baseline which have missing values and hence will be
# imputed
################################################################################
these_vars <- c("has_partner", 
                "income_val",
                "srq_mean",        # self-regulation: higher scores indicate higher self-regulation abilities
                "mdes_pos_mean",   # mdes: higher scores indicate more intense positive emotions
                "mdes_neg_mean",   # mdes: higher scores indicate more intense negative emotions
                "gratitude")       # gratitude: higher scores indicate higher gratitude

dat_baseline_wide <- dat_primary_aim %>%
  select(replicate_id, participant_id, all_of(these_vars)) %>%  
  arrange(replicate_id, participant_id) %>% unique(.)

if(maximum_replicate_id > 0){
  dat_original <- dat_baseline_wide %>% filter(replicate_id == 0)
  dat_replicate <- dat_baseline_wide %>% filter(replicate_id > 0)
  
  dat_replicate <- dat_replicate %>% 
    mutate(across(.cols = all_of(these_vars), 
                  .fns = function(curr_col){
                    curr_col = NA_real_
                    return(curr_col)
                    }
                  )
           )
  
  dat_baseline_wide <- rbind(dat_original, dat_replicate)
}

################################################################################
# Control variables assessed at baseline which do not have missing data
# and hence do not need to be imputed
################################################################################
dat_control_wide <- dat_primary_aim %>%
  select(replicate_id, participant_id, 
         age, is_female, is_latino, is_not_latino_and_black, is_not_latino_and_other,
         baseline_tobacco_history) %>%
  arrange(replicate_id, participant_id) %>% unique(.)

################################################################################
# Time-varying variables which have missing values and hence will be imputed
################################################################################
these_vars <- c("quick_survey_response",
                "Y",
                "cigarette_counts",
                "src_scored")

dat_timevarying_long_with_missing <- dat_primary_aim %>%
  select(replicate_id, participant_id, decision_point, 
         all_of(these_vars),
         any_recent_eligible_dp, engagement_most_recent_eligible) %>% 
  arrange(replicate_id, participant_id, decision_point)

if(maximum_replicate_id > 0){
  dat_original <- dat_timevarying_long_with_missing %>% filter(replicate_id == 0)
  dat_replicate <- dat_timevarying_long_with_missing %>% filter(replicate_id > 0)
  
  dat_replicate <- dat_replicate %>% 
    mutate(across(.cols = all_of(these_vars), 
                  .fns = function(curr_col){
                    curr_col = NA_real_
                    return(curr_col)
                  }))
  
  dat_timevarying_long_with_missing <- rbind(dat_original, dat_replicate)
  
  dat_timevarying_long_with_missing <- dat_timevarying_long_with_missing %>%
    mutate(engagement_most_recent_eligible = if_else((any_recent_eligible_dp == 1) & (replicate_id > 0), NA, engagement_most_recent_eligible))
}

################################################################################
# Time-varying variables which DO NOT have missing values and hence will NOT be 
# imputed
################################################################################
these_vars <- c("eligibility", 
                "elig24hrs", 
                "counts_rand_past24hrs", 
                "coinflip", 
                "is_high_effort", 
                "is_low_effort",
                "matched_24hrs", 
                "matched_recent", 
                "any_response_2qs",  
                "Y_nreported_past24hrs", 
                "quick_survey_nreported_past24hrs",
                "emi_resp_indicator",
                "coinflip_sum_past24hrs",
                "emi_resp_indicator_sum_past24hrs",
                "is_high_effort_sum_past24hrs",
                "is_low_effort_sum_past24hrs")

dat_timevarying_long_without_missing <- dat_primary_aim %>%
  select(replicate_id, participant_id, decision_point, all_of(these_vars)) %>% 
  arrange(replicate_id, participant_id, decision_point)

################################################################################
# Merge data frame containing time-varying variables with missing data
# and data frame containing time-varying variables without missing data
################################################################################
dat_timevarying_long <- full_join(x = dat_timevarying_long_without_missing,
                                  y = dat_timevarying_long_with_missing,
                                  by = join_by(replicate_id == replicate_id,
                                               participant_id == participant_id,
                                               decision_point == decision_point))

################################################################################
# Binary variables in dat_timevarying_long that will be imputed are converted
# from numeric to factor
################################################################################
dat_timevarying_long[["Y"]] <- as_factor(dat_timevarying_long[["Y"]])
dat_timevarying_long[["quick_survey_response"]] <- as_factor(dat_timevarying_long[["quick_survey_response"]])
dat_baseline_wide[["has_partner"]] <- as_factor(dat_baseline_wide[["has_partner"]])

################################################################################
# Transform dataset with time-varying covariates from long to wide format
################################################################################
spec1 <- dat_timevarying_long %>% 
  build_wider_spec(names_from = decision_point, 
                   names_prefix = "dp",
                   values_from = c(Y, 
                                   any_recent_eligible_dp, 
                                   engagement_most_recent_eligible,
                                   src_scored, 
                                   cigarette_counts,
                                   quick_survey_response,
                                   eligibility, 
                                   elig24hrs, 
                                   counts_rand_past24hrs, 
                                   coinflip, 
                                   is_high_effort, 
                                   is_low_effort,
                                   matched_24hrs, 
                                   matched_recent, 
                                   any_response_2qs, 
                                   Y_nreported_past24hrs, 
                                   quick_survey_nreported_past24hrs, 
                                   emi_resp_indicator,
                                   coinflip_sum_past24hrs,
                                   emi_resp_indicator_sum_past24hrs,
                                   is_high_effort_sum_past24hrs,
                                   is_low_effort_sum_past24hrs))

dat_timevarying_wide <- dat_timevarying_long %>% pivot_wider_spec(spec1, id_cols = c("replicate_id", "participant_id"))

################################################################################
# What are the variable names in each dataset that will have to eventually be 
# transformed from wide to long format at a later step in the pipeline?
################################################################################
cols_to_drop <- c("replicate_id", "participant_id", "decision_point")
cols_to_keep_control <- colnames(dat_control_wide)[!(colnames(dat_control_wide) %in% cols_to_drop)]
cols_to_keep_baseline <- colnames(dat_baseline_wide)[!(colnames(dat_baseline_wide) %in% cols_to_drop)]
cols_to_keep_timevarying <- colnames(dat_timevarying_long)[!(colnames(dat_timevarying_long) %in% cols_to_drop)]

saveRDS(cols_to_drop, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_drop.rds"))
saveRDS(cols_to_keep_control, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_control.rds"))
saveRDS(cols_to_keep_baseline, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
saveRDS(cols_to_keep_timevarying, file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))

################################################################################
# Save datasets
################################################################################
saveRDS(dat_control_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_control_wide.rds"))
saveRDS(dat_baseline_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_baseline_wide.rds"))
saveRDS(dat_timevarying_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))


