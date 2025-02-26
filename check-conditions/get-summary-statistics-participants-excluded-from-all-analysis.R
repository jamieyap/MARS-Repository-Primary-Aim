source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
mars_ids_did_not_meet_ema_completion_criteria <- readRDS(file = file.path(path_manipulated_data, "mars_ids_did_not_meet_ema_completion_criteria.rds"))
dat_parsed_conditions <- readRDS(file = file.path(path_manipulated_data, "dat_parsed_conditions.rds"))

################################################################################
# Calculate summary statistics for participants excluded due to completing
# less than 3 EMA
################################################################################
dat_matched_to_decision_points <- dat_matched_to_decision_points %>% filter(mars_id %in% mars_ids_did_not_meet_ema_completion_criteria)

dat_parsed_conditions_2qs <- dat_parsed_conditions %>% 
  filter(evaluated_true == TRUE) %>%
  filter(is_2qs == 1) %>%
  select(mars_id, 
         condition_id_quick_survey = condition_id,
         ts_condition_check_completed_mountain_quick_survey = ts_condition_check_completed_mountain,
         privacy_data_not_found_quick_survey = privacy_data_not_found,
         privacy_activated_quick_survey = privacy_activated,
         privacy_not_activated_quick_survey = privacy_not_activated,
         driving_data_not_found_quick_survey = driving_data_not_found,
         driving_detected_quick_survey = driving_detected,
         driving_not_detected_quick_survey = driving_not_detected)

dat_parsed_conditions_rand <- dat_parsed_conditions %>% 
  filter(evaluated_true == TRUE) %>%
  filter(is_rand == 1) %>%
  select(mars_id, 
         condition_id_rand = condition_id,
         ts_condition_check_completed_mountain_rand = ts_condition_check_completed_mountain,
         privacy_data_not_found_rand = privacy_data_not_found,
         privacy_activated_rand = privacy_activated,
         privacy_not_activated_rand = privacy_not_activated,
         driving_data_not_found_rand = driving_data_not_found,
         driving_detected_rand = driving_detected,
         driving_not_detected_rand = driving_not_detected)

dat_parsed_conditions_ema <- dat_parsed_conditions %>% 
  filter(evaluated_true == TRUE) %>%
  filter(is_ema == 1) %>%
  select(mars_id, 
         condition_id_ema = condition_id,
         ts_condition_check_completed_mountain_ema = ts_condition_check_completed_mountain,
         privacy_data_not_found_ema = privacy_data_not_found,
         privacy_activated_ema = privacy_activated,
         privacy_not_activated_ema = privacy_not_activated,
         driving_data_not_found_ema = driving_data_not_found,
         driving_detected_ema = driving_detected,
         driving_not_detected_ema = driving_not_detected)

###############################################################################
# Match parsed information on the context
# surrounding the decision on whether micro-randomization,
# 2qs administration and EMA administration ought to occur
# to the corresponding decision point.
###############################################################################
dat_matched_to_decision_points <- left_join(x = dat_matched_to_decision_points,
                                            y = dat_parsed_conditions_2qs,
                                            by = join_by(mars_id == mars_id,
                                                         closest(ts_2qs_triggered_mountain >= ts_condition_check_completed_mountain_quick_survey)
                                            ))

dat_matched_to_decision_points <- left_join(x = dat_matched_to_decision_points,
                                            y = dat_parsed_conditions_rand,
                                            by = join_by(mars_id == mars_id,
                                                         closest(ts_coinflip_mountain >= ts_condition_check_completed_mountain_rand)
                                            ))

dat_matched_to_decision_points <- left_join(x = dat_matched_to_decision_points,
                                            y = dat_parsed_conditions_ema,
                                            by = join_by(mars_id == mars_id,
                                                         closest(ts_ema_triggered_mountain >= ts_condition_check_completed_mountain_ema)
                                            ))

dat_matched_to_decision_points <- dat_matched_to_decision_points %>%
  mutate(is_triggered_quick_survey = 1*(!is.na(ts_2qs_triggered_mountain)),
         is_triggered_rand = 1*(!is.na(ts_coinflip_mountain)),
         is_triggered_ema = 1*(!is.na(ts_ema_triggered_mountain))) %>%
  select(mars_id, olson, cluster_id, decision_point, block_number, block_bounds_mountain,
         A,
         status_survey_ema,
         is_triggered_quick_survey, is_triggered_rand, is_triggered_ema,
         condition_id_quick_survey,
         condition_id_rand,
         condition_id_ema,
         ts_condition_check_completed_mountain_quick_survey,
         ts_condition_check_completed_mountain_rand,
         ts_condition_check_completed_mountain_ema,
         # conditions for 2qs
         privacy_data_not_found_quick_survey,
         privacy_activated_quick_survey,
         privacy_not_activated_quick_survey,
         driving_data_not_found_quick_survey,
         driving_detected_quick_survey,
         driving_not_detected_quick_survey,
         # conditions for micro-randomization
         privacy_data_not_found_rand,
         privacy_activated_rand,
         privacy_not_activated_rand,
         driving_data_not_found_rand,
         driving_detected_rand,
         driving_not_detected_rand,
         # conditions for EMA
         privacy_data_not_found_ema,
         privacy_activated_ema,
         privacy_not_activated_ema,
         driving_data_not_found_ema,
         driving_detected_ema,
         driving_not_detected_ema)

dat_summary <- dat_matched_to_decision_points %>%
  filter((decision_point >= 7) & (decision_point <= 54)) %>%
  mutate(eligibility = if_else(!is.na(A), 1, 0)) %>%
  mutate(randomized_and_ema_delivered = if_else((eligibility==1) & (is_triggered_ema==1), 1, 0),
         randomized_and_ema_delivered_and_ema_completed = if_else((eligibility==1) & (is_triggered_ema==1) & (status_survey_ema == "completed"), 1, 0)) %>%
  summarise(n_participants = length(unique(mars_id)),
            n_dp = n(),
            n_checked = sum(!is.na(ts_condition_check_completed_mountain_rand)),
            n_randomized = sum(eligibility),
            n_randomized_and_ema_delivered = sum(randomized_and_ema_delivered),
            n_randomized_and_ema_delivered_and_completed = sum(randomized_and_ema_delivered_and_ema_completed, na.rm = TRUE))

dat_summary

###############################################################################
# Save output
###############################################################################
write.csv(dat_summary, "check-conditions/dat_summary_decision_points_excluded_due_to_low_ema_completion.csv", row.names = FALSE)
