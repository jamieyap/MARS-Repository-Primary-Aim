rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))

################################################################################
# Binary variables in dat_primary_aim that will be imputed are converted from 
# numeric to factor
################################################################################
dat_primary_aim[["Y"]] <- as_factor(dat_primary_aim[["Y"]])
dat_primary_aim[["has_partner"]] <- as_factor(dat_primary_aim[["has_partner"]])

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
# Create long format dataset that we will begin with in our workflow for
# creating multiply imputed datasets
################################################################################
dat_control_wide <- dat_primary_aim %>%
  select(participant_id, 
         age, is_female, 
         is_latino, is_not_latino_and_black, is_not_latino_and_other,
         baseline_tobacco_history, has_partner, income_val) %>%
  arrange(participant_id) %>%
  unique(.)

dat_control_wide[["is_income_observed"]] <- if_else(!is.na(dat_control_wide[["income_val"]]), 1, 0)

dat_timevarying_long <- dat_primary_aim %>%
  select(participant_id, decision_point, 
         Y,  
         eligibility, elig24hrs, counts_rand_past24hrs, coinflip, is_high_effort, is_low_effort,
         matched_24hrs, matched_recent, 
         any_response_2qs, any_recent_eligible_dp, engagement_most_recent_eligible,
         src_scored, cigarette_counts,
         Y_nreported_past24hrs, quick_survey_nreported_past24hrs, quick_survey_response) %>% 
  arrange(participant_id)

################################################################################
# Transform dataset with time-varying covariates from long to wide format
################################################################################
spec1 <- dat_timevarying_long %>% 
  build_wider_spec(names_from = decision_point, 
                   names_prefix = "dp",
                   values_from = c(Y, 
                                   eligibility, elig24hrs, counts_rand_past24hrs, coinflip, is_high_effort, is_low_effort,
                                   matched_24hrs, matched_recent, 
                                   any_response_2qs, any_recent_eligible_dp, engagement_most_recent_eligible,
                                   src_scored, 
                                   cigarette_counts,
                                   Y_nreported_past24hrs, quick_survey_nreported_past24hrs, quick_survey_response))

dat_timevarying_wide <- dat_timevarying_long %>% pivot_wider_spec(spec1, id_cols = participant_id)

################################################################################
# Moderators which were assessed at baseline
################################################################################
dat_baseline_wide <- dat_primary_aim %>%
  select(participant_id, 
         srq_mean,       # self-regulation: higher scores indicate higher self-regulation abilities
         mdes_pos_mean,  # mdes: higher scores indicate more intense positive emotions
         mdes_neg_mean,  # mdes: higher scores indicate more intense negative emotions
         maas_total,     # mindfulness: higher scores indicate higher mindfulness
         ffmq_nonjudge,  # mindfulness: higher scores indicate higher non-judging of inner experiences
         gratitude) %>%  # gratitude: higher scores indicate higher gratitude
  arrange(participant_id) %>%
  unique(.)

################################################################################
# Save datasets
################################################################################
saveRDS(dat_control_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_control_wide.rds"))
saveRDS(dat_baseline_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_baseline_wide.rds"))
saveRDS(dat_timevarying_wide, file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))

# This is the original dat_primary_aim dataset, except that:
# Y is not numeric, but rather, a factor
# quick_survey_response is a factor
# and we have added new columns from dat_mars_mi_time_varying_covariates
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_pipeline_data, "dat_long.rds")) 

