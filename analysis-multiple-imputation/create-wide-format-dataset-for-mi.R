rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range_with_metadata.rds"))
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))
dat_mars_mi_time_varying_covariates <- readRDS(file = file.path(path_multiple_imputation_data, "dat_mars_mi_time_varying_covariates.rds"))

################################################################################
# Save the earliest decision point within the past 24 hours and most recent
# eligible decision point. Here, NULL or NA values represent the fact that
# the value is undefined, rather than missing. We code NULL or NA values here
# as -1.
################################################################################
scanned_decision_points_within_range[["matched_24hrs"]] <- lapply(scanned_decision_points_within_range[["decision_points_past24hrs"]],  
                                                                  function(x){
                                                                    cond <- (!is.null(x[[1]]))
                                                                    if(cond){
                                                                      out <- min(x[[1]])
                                                                    }else{
                                                                      out <- -1
                                                                    }
                                                                  })

scanned_decision_points_within_range[["matched_24hrs"]] <- unlist(scanned_decision_points_within_range[["matched_24hrs"]])
scanned_decision_points_within_range[["matched_recent"]] <- scanned_decision_points_within_range[["decision_points_most_recent_eligible"]]
scanned_decision_points_within_range[["matched_recent"]] <- replace(scanned_decision_points_within_range[["matched_recent"]], is.na(scanned_decision_points_within_range[["matched_recent"]]), -1)

scanned_decision_points_within_range <- scanned_decision_points_within_range %>%
  select(mars_id, decision_point, matched_24hrs, matched_recent)

################################################################################
# Create long format dataset that we will begin with in our workflow for
# creating multiply imputed datasets
################################################################################
dat_primary_aim <- left_join(x = dat_primary_aim, y = dat_mars_mi_time_varying_covariates, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_primary_aim <- left_join(x = dat_primary_aim, y = scanned_decision_points_within_range, by = join_by(mars_id == mars_id, decision_point == decision_point))

################################################################################
# Total number of micro-randomizations in the past 24 hours prior to the 
# current micro-randomization (not including the current micro-randomization)
################################################################################
dat_primary_aim <- dat_primary_aim %>%
  mutate(elig24hrs = if_else(counts_rand_past24hrs > 0, 1, 0))  

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

dat_timevarying_long <- dat_primary_aim %>%
  select(participant_id, decision_point, 
         Y, 
         eligibility, elig24hrs, coinflip, 
         matched_24hrs, matched_recent, 
         any_response_2qs, any_recent_eligible_dp, engagement_most_recent_eligible,
         happy, src_scored, substance_is_any, substance_is_any_nicotine,
         cigarette_counts) %>% 
  arrange(participant_id)

# This step is specific to our workflow
# If eligibility == 0, code variables assessed in EMA as -1
dat_timevarying_long <- dat_timevarying_long %>%
  mutate(Y = replace(Y, eligibility == 0, -1),
         happy = replace(happy, eligibility == 0, -1),
         src_scored = replace(src_scored, eligibility == 0, -1),
         substance_is_any = replace(substance_is_any, eligibility == 0, -1),
         substance_is_any_nicotine = replace(substance_is_any_nicotine, eligibility == 0, -1),
         cigarette_counts = replace(cigarette_counts, eligibility == 0, -1))

these_participants <- dat_control_wide[["participant_id"]]

################################################################################
# Transform dataset with time-varying covariates from long to wide format
################################################################################
spec1 <- dat_timevarying_long %>% 
  select(-participant_id) %>%
  build_wider_spec(names_from = decision_point, 
                   names_prefix = "dp",
                   values_from = c(Y, 
                                   eligibility, elig24hrs, coinflip, 
                                   matched_24hrs, matched_recent, 
                                   any_response_2qs, any_recent_eligible_dp, engagement_most_recent_eligible,
                                   happy, src_scored, substance_is_any, substance_is_any_nicotine,
                                   cigarette_counts))

dat_timevarying_wide <- dat_timevarying_long %>% pivot_wider_spec(spec1)
dat_timevarying_wide[["participant_id"]] <- these_participants

################################################################################
# Moderators which were assessed at baseline
################################################################################
dat_baseline_wide <- dat_primary_aim %>%
  select(participant_id, 
         srq_mean, # self-regulation: higher scores indicate higher self-regulation abilities
         mdes_pos_mean, # mdes: higher scores indicate more intense positive emotions
         mdes_neg_mean, # mdes: higher scores indicate more intense negative emotions
         ecig_1, # do you use any electronic nicotine delivery system like vape pen, JUUL, or ecigarettes: 1 = yes, 0 = no
         ecig_3, # on days you use a vape pen or JUUL, please estimate how many separate times per day you usually use it. number: min = 0; max = 500
         maas_total, # mindfulness: higher scores indicate higher mindfulness
         ffmq_nonjudge,  # mindfulness: higher scores indicate higher non-judging of inner experiences
         gratitude) %>%  # gratitude: higher scores indicate higher gratitude
  arrange(participant_id) %>%
  unique(.)

################################################################################
# Save datasets
################################################################################
saveRDS(dat_control_wide, file = file.path(path_multiple_imputation_data, "dat_control_wide.rds"))
saveRDS(dat_baseline_wide, file = file.path(path_multiple_imputation_data, "dat_baseline_wide.rds"))
saveRDS(dat_timevarying_wide, file = file.path(path_multiple_imputation_data, "dat_timevarying_wide.rds"))
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_data, "dat_long.rds"))  # This is the original dat_primary_aim dataset but with additional columns

