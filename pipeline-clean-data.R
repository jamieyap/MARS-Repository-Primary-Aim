###############################################################################
# Record when pipeline begins
###############################################################################

.start_time_pipe <- Sys.time()

###############################################################################
# Extract log files and ID's
###############################################################################

# Input: 
#   None
# Output: 
#   list_ids.rds
source("parse-grafana-ids.R")
rm(list = ls())

# Input: 
#   list_ids.rds
# Output: 
#   list_all_system_log.rds
#   list_all_loweffort_log.rds
source("unzip-files.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: micro-randomization
###############################################################################

# Input: 
#   list_all_system_log.rds
# Output: 
#   list_dat_triggered_coinflip.rds
source("parse-micro-randomization-time.R")
rm(list = ls())

# Input: 
#   list_all_system_log.rds
# Output: 
#   list_all_emi.rds
source("parse-treatment-assignments.R")
rm(list = ls())

# Input: 
#   list_dat_triggered_coinflip.rds
#   list_all_emi.rds
# Output:  
#   dat_all_with_matched_treatment_assignments.rds
source("match-treatment-assignments.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: brief survey
###############################################################################

# Input:
#   list_all_system_log.rds
# Output:
#   list_dat_triggered_2qs.rds
#   list_dat_triggered_ema.rds 
source("parse-survey-trigger-time.R")
rm(list = ls())

# Input:
#   list_all_loweffort_log.rds
# Output:
#   list_dat_response_to_triggered_2qs.rds
source("parse-responses-2qs.R")
rm(list = ls())

# Input:
#   list_all_system_log.rds
# Output:
#   list_status_survey_2qs.rds
#   list_status_survey_EMA.rds
source("parse-status-surveys.R")
rm(list = ls())

# Input:
#   brief_survey_linked_trigger_to_status.rds
#   list_dat_response_to_triggered_2qs.rds
# Output:
#   dat_all_with_matched_status_2qs.rds
source("match-2qs.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: EMA
###############################################################################

# Input:
#   list_ids.rds
# Output:
#   dat_long_ema_responses.rds
source("parse-responses-ema.R")
rm(list = ls())

# Input:
#   dat_long_ema_responses.rds
# Output:
#   dat_master_ema_questions.rds
#   dat_conventional_long_format_ema_responses.rds
#   dat_primary_outcome.rds
source("reshape-parsed-ema.R")
rm(list = ls())

# Input:
#   ema_linked_trigger_to_status.rds
#   dat_conventional_long_format_ema_responses.rds
# Output:
#   dat_all_with_matched_status_ema.rds
source("match-ema.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: 2QS, micro-randomization, EMA
###############################################################################

# Input:
#   dat_all_with_matched_status_2qs.rds
#   dat_all_with_matched_treatment_assignments.rds
#   dat_all_with_matched_status_ema.rds
# Output:
#   dat_all_burst_with_pilot.rds -- note that this data frame contains data from the 3 pilot participants
#   dat_all_burst.rds -- note that this data frame DOES NOT have pilot participant data
#   dat_decisions_atypical_sequence.rds
source("stitch-together-block-level-data.R")
rm(list = ls())

# Input:
#   dat_all_burst.rds
#   dat_tracking_atypical_sequence.rds
# Output:
#   dat_cleaned_burst.rds
source("clean-up-atypical-sequences.R")
rm(list = ls())

###############################################################################
# Create V1 date variable and crosswalk between two types of ID's
###############################################################################

# Input:
#   list_ids.rds
#   Visit1_DataChecklist.rds
# Output:
#   crosswalk_grafana_rsr.rds
source("create-crosswalk-with-visit-data-ids.R")
rm(list = ls())

# Input:
#   list_ids.rds
#   crosswalk_grafana_rsr.rds
#   event_log.rds
# Output:
#   dat_visit_dates.rds
#   dat_visit_dates_V1_only.rds
source("create-visit-dates.R")
rm(list = ls())

###############################################################################
# Consolidate demographic variables from raw data
###############################################################################

# Input:
#   dat_visit_dates.rds
#   redcap_crosswalk.rds
#   redcap_metadata.rds
#   redcap_data.rds
# Output:
#   dat_demogs.rds
source("derive-demogs-from-raw-data.R")
rm(list = ls())

###############################################################################
# Preparatory steps for creating the analytical datasets
###############################################################################

# Input:
#   dat_visit_dates_V1_only.rds
#   dat_cleaned_burst.rds
# Output:
#   dat_cleaned_burst_with_outside_study_indicator.rds
#   dat_within_study.rds
#   dat_outside_study.rds
#   dat_matched_to_decision_points.rds
#   mars_ids_did_not_have_mhealth_data_within_mrt_period
source("remove-out-of-study-period.R")
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_did_not_have_mhealth_data_within_mrt_period
# Output:
#   mars_ids_did_not_meet_ema_completion_criteria.rds
#   mars_ids_pilot.rds -- ID's corresponding to beta testing/pilot participants
#   mars_ids_excluded_from_all_analytic_datasets.rds
source("screen-for-analytical-dataset-inclusion.R")
rm(list = ls())

###############################################################################
# Record when CORE pipeline is completed
###############################################################################
.end_time_pipe <- Sys.time()

.total_time_pipe <- difftime(time1 = .end_time_pipe, time2 = .start_time_pipe, units = "mins")

print(.total_time_pipe)

###############################################################################
# Derive analytical datasets
# 
# Note:
#   The rows of all the datasets listed under 'Output' are subsets 
#   of the rows in dat_matched_to_decision_points.rds
#   since rows belonging to participants who were not in the data file
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   were excluded.
###############################################################################

rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
# Output:
#   dat_mars_basic.rds
source("prepare-analytic-datasets/create-basic-analysis-dataset.R")
rm(list = ls())

# Input:
#   dat_demogs.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
# Output:
#   dat_mars_coded_demogs.rds
#   dat_summary_missing_demogs.rds
#   dat_summary_demogs_continuous.rds
#   dat_summary_demogs_binary.rds
source("prepare-analytic-datasets/create-derived-demog-variable-dataset.R")
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
# Output:
#   scanned_decision_points_within_range.rds
#   scanned_decision_points_within_range_with_metadata.rds
source("prepare-analytic-datasets/scan-for-decision-points-within-range.R")
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
# Output:
#   dat_mars_derived_time_vars.rds
source("prepare-analytic-datasets/create-derived-time-variable-dataset.R")
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   dat_master_ema_questions.rds
#   dat_master_ema_response_options.rds
# Output:
#   dat_mars_time_varying_moderators.rds
source("prepare-analytic-datasets/create-time-varying-moderator-variable-dataset.R")  
rm(list = ls())

# Input:
#   v1_baseline_quest.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
# Output:
#   dat_mars_baseline_moderators.rds
source("prepare-analytic-datasets/create-static-moderator-variable-dataset.R")  
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   dat_mars_basic.rds
#   scanned_decision_points_within_range.rds
# Output:
#   dat_mars_time_varying_noise_reduction_vars.rds
source("prepare-analytic-datasets/create-time-varying-noise-reduction-variables-dataset.R")  
rm(list = ls())

# Input:
#   dat_mars_basic.rds
#   dat_mars_derived_time_vars.rds
#   dat_mars_time_varying_noise_reduction_vars.rds
#   dat_mars_time_varying_moderators.rds
#   scanned_decision_points_within_range.rds
# Output:
#   dat_mars_time_varying_covariates.rds
source("prepare-analytic-datasets/create-time-varying-covariates-dataset.R")  
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   dat_master_ema_questions.rds
#   dat_master_ema_response_options.rds
# Output:
#   parsed_cigarette_smoking_times.rds
source("prepare-analytic-datasets/indicator-for-proximal-cigarette-smoking/parse-cigarette-smoking-times.R")  
rm(list = ls())

# Input:
#   parsed_cigarette_smoking_times.rds
# Output:
#   dat_mars_proximal_cigarette_smoking_with_metadata.rds
#   dat_mars_proximal_cigarette_smoking.rds
source("prepare-analytic-datasets/indicator-for-proximal-cigarette-smoking/implement-decision-rule-cigarette-smoking-times.R")  
rm(list = ls())

# Input:
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
#   dat_master_ema_questions.rds
#   dat_master_ema_response_options.rds
# Output:
#   parsed_vape_puff_times.rds
source("prepare-analytic-datasets/indicator-for-proximal-vaping/parse-vaping-smoking-times.R")  
rm(list = ls())

# Input:
#   parsed_vape_puff_times.rds
# Output:
#   dat_mars_proximal_vaping_with_metadata.rds
#   dat_mars_proximal_vaping.rds
source("prepare-analytic-datasets/indicator-for-proximal-vaping/implement-decision-rule-vaping-smoking-times.R")
rm(list = ls())

# Input:
#   dat_mars_basic.rds
#   dat_mars_derived_time_vars.rds
#   dat_mars_time_varying_noise_reduction_vars.rds
#   dat_mars_time_varying_moderators.rds
#   dat_mars_time_varying_covariates.rds
#   dat_mars_coded_demogs.rds
#   dat_mars_baseline_mdoerators.rds
#   dat_mars_proximal_cigarette_smoking.rds
#   dat_mars_proximal_vaping.rds
#   app_usage_for_mainpipeline_updated0212.rds
# Output:
#   dat_primary_aim.rds
source("prepare-analytic-datasets/create-primary-aims-paper-analysis-dataset.R")
rm(list = ls())

###############################################################################
# Linking data and metadata elements: conditions under which
# micro-randomization occurred, 2qs was administered, EMA was administered
###############################################################################

# Input: 
#   list_all_system_log.rds
# Output: 
#   dat_parsed_conditions.rds
source("check-conditions/parse-conditions.R")
rm(list = ls())


# Input: 
#   dat_parsed_conditions.rds
#   dat_matched_to_decision_points.rds
#   mars_ids_excluded_from_all_analytic_datasets.rds
# Output: 
#   dat_matched_conditions.rds
source("check-conditions/match-conditions.R")
rm(list = ls())

###############################################################################
# Record when pipeline is completed
###############################################################################
.end_time_pipe <- Sys.time()

.total_time_pipe <- difftime(time1 = .end_time_pipe, time2 = .start_time_pipe, units = "mins")

print(.total_time_pipe)

