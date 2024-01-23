rm(list = ls())

source("paths.R")
library(tidyverse)

dat_parsed_conditions <- readRDS(file = file.path(path_manipulated_data, "dat_parsed_conditions.rds"))
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

###############################################################################
# Grab only the rows and columns you need
###############################################################################
dat_matched_to_decision_points <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))
dat_parsed_conditions <- dat_parsed_conditions %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

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

###############################################################################
# Create summary statistics
###############################################################################
dat_summary <- dat_matched_to_decision_points %>%
  filter(is_triggered_rand == 1) %>%
  group_by(privacy_data_not_found_rand,
           privacy_activated_rand,
           privacy_not_activated_rand,
           driving_data_not_found_rand,
           driving_detected_rand,
           driving_not_detected_rand) %>%
  summarise(count = n())

dat_summary2 <- dat_matched_to_decision_points %>%
  filter((cluster_id >= 2) & (cluster_id <= 9)) %>%
  filter(is_triggered_rand == 1) %>%
  group_by(privacy_data_not_found_rand,
           privacy_activated_rand,
           privacy_not_activated_rand,
           driving_data_not_found_rand,
           driving_detected_rand,
           driving_not_detected_rand) %>%
  summarise(count = n())

###############################################################################
# Save output
###############################################################################
saveRDS(dat_matched_to_decision_points, file = file.path(path_manipulated_data, "dat_matched_conditions.rds"))

write.csv(dat_summary, "check-conditions/dat_summary_conditions_days_one_to_ten.csv", row.names = FALSE)
write.csv(dat_summary2, "check-conditions/dat_summary_conditions_days_two_to_nine.csv", row.names = FALSE)

