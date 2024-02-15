rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# This dataset contains data from only those sequences which began
# within the 10-day MRT period.
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
# Among participants who have any data from sequences which began within the
# 10-day MRT period, we identified those participants who did not complete
# at least 3 EMA between the 2nd and 9th day inclusive.
# These participant ID's are saved in the variable below.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))
# Contains masterlist of EMA questions
dat_master_ema_questions <- readRDS(file = file.path(path_manipulated_data, "dat_master_ema_questions.rds"))
# Contains masterlist of response options, response types, response options related to each EMA question
dat_master_ema_response_options <- readRDS(file = file.path(path_manipulated_data, "dat_master_ema_response_options.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))
all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)

################################################################################
# Collapse EMA status into a few levels
################################################################################
dat_analysis[["status_survey_ema_collapsed"]] <- collapse_survey_ema_status(cleaned_data_frame = dat_analysis)

################################################################################
# Grab the columns you will need
################################################################################
dat_analysis <- dat_analysis %>% 
  select(mars_id, 
         decision_point, 
         olson, 
         ts_coinflip_mountain,
         status_survey_ema,
         status_survey_ema_collapsed,
         ts_ema_triggered_mountain,
         ts_status_survey_ema_mountain,
         Q19_response,  # Question Text: "Since the last assessment, did you use any of the following? (check all that apply)"
         Q24_response,  # Question Text: "Since the last assessment about how many puffs of the vape pen, JUUL, or e-cigarette did you take?"
         Q25_response,  # Question Text: "You said you vaped 1 puff since the last assessment. What time was it when you vaped?"
         Q26_response,  # Question Text: "You said you vaped more than 1 puff since the last assessment. What time was it when you last vaped?"
         Q27_response,  # Question Text: "You said you vaped more than 1 puff since the last assessment. What time was it when you first vaped?"
         Q51_response,  # Question Text: "You said you vaped more than 1 puff since the last assessment. The question on this screen is asking when you first vaped, and the question on the next screen will ask when you most recently vaped. What time was it when you first vaped?"
         Q52_response,  # Question Text: "You said you vaped more than 1 puff since the last assessment, what time was it when you most recently vaped?"
         Q19_ts_finish_time_mountain,
         Q24_ts_finish_time_mountain,
         Q25_ts_finish_time_mountain,
         Q26_ts_finish_time_mountain,
         Q27_ts_finish_time_mountain,
         Q51_ts_finish_time_mountain,
         Q52_ts_finish_time_mountain)

dat_analysis[["ts_coinflip_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_coinflip_mountain")
dat_analysis[["ts_ema_triggered_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_ema_triggered_mountain")
dat_analysis[["ts_status_survey_ema_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_status_survey_ema_mountain")
dat_analysis[["Q19_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q19_ts_finish_time_mountain")
dat_analysis[["Q24_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q24_ts_finish_time_mountain")
dat_analysis[["Q25_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q25_ts_finish_time_mountain")
dat_analysis[["Q26_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q26_ts_finish_time_mountain")
dat_analysis[["Q27_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q27_ts_finish_time_mountain")
dat_analysis[["Q51_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q51_ts_finish_time_mountain")
dat_analysis[["Q52_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q52_ts_finish_time_mountain")

################################################################################
# Create an indicator for any substance use and an indicator for any
# vaping
################################################################################

dat_analysis <- clean_response_substance_use_multiple_select(dat_analysis)

################################################################################
# Create a variable capturing number of puffs smoked
################################################################################

dat_analysis[["Q24_response_cleaned"]] <- clean_response_vape_puffs(dat_analysis)

################################################################################
# Parse reported smoking time
################################################################################

# Note that EMA item wording changed during the study.
# The code below creates a variable that tags whether an EMA used the early or
# late version of the EMA item wording.
dat_analysis[["item_version_vape_when_puffed"]] <- identify_item_version_vape_when_puffed(dat_analysis)

dat_analysis <- dat_analysis %>%
  mutate(Q25_response_parsed = hms(Q25_response, quiet = TRUE), 
         Q26_response_parsed = hms(Q26_response, quiet = TRUE), 
         Q27_response_parsed = hms(Q27_response, quiet = TRUE),
         Q51_response_parsed = hms(Q51_response, quiet = TRUE),
         Q52_response_parsed = hms(Q52_response, quiet = TRUE)) 

dat_analysis[["puffs_one_hms_local"]] <- dat_analysis[["Q25_response_parsed"]]
dat_analysis[["puffs_many_first_hms_local"]] <- coalesce(dat_analysis[["Q27_response_parsed"]], dat_analysis[["Q51_response_parsed"]])
dat_analysis[["puffs_many_last_hms_local"]] <- coalesce(dat_analysis[["Q26_response_parsed"]], dat_analysis[["Q52_response_parsed"]])

dat_analysis[["puffs_one_datetime_local"]] <- dat_analysis[["ts_coinflip_local"]]
dat_analysis[["puffs_many_first_datetime_local"]] <- dat_analysis[["ts_coinflip_local"]]
dat_analysis[["puffs_many_last_datetime_local"]] <- dat_analysis[["ts_coinflip_local"]]

hour(dat_analysis[["puffs_one_datetime_local"]]) <- hour(dat_analysis[["puffs_one_hms_local"]])
minute(dat_analysis[["puffs_one_datetime_local"]]) <- minute(dat_analysis[["puffs_one_hms_local"]])
second(dat_analysis[["puffs_one_datetime_local"]]) <- second(dat_analysis[["puffs_one_hms_local"]])

hour(dat_analysis[["puffs_many_first_datetime_local"]]) <- hour(dat_analysis[["puffs_many_first_hms_local"]])
minute(dat_analysis[["puffs_many_first_datetime_local"]]) <- minute(dat_analysis[["puffs_many_first_hms_local"]])
second(dat_analysis[["puffs_many_first_datetime_local"]]) <- second(dat_analysis[["puffs_many_first_hms_local"]])

hour(dat_analysis[["puffs_many_last_datetime_local"]]) <- hour(dat_analysis[["puffs_many_last_hms_local"]])
minute(dat_analysis[["puffs_many_last_datetime_local"]]) <- minute(dat_analysis[["puffs_many_last_hms_local"]])
second(dat_analysis[["puffs_many_last_datetime_local"]]) <- second(dat_analysis[["puffs_many_last_hms_local"]])

################################################################################
# Rename columns
################################################################################

lookup <- c(olson = "olson",
            ts_coinflip_mountain = "ts_coinflip_mountain",
            ts_coinflip_local = "ts_coinflip_local",
            status_survey_ema = "status_survey_ema",
            status_survey_ema_collapsed = "status_survey_ema_collapsed",
            item_version_vape_when_puffed = "item_version_vape_when_puffed",
            ts_ema_triggered_mountain = "ts_ema_triggered_mountain",
            ts_status_survey_ema_mountain = "ts_status_survey_ema_mountain",
            Q19_ts_finish_time_local = "Q19_ts_finish_time_local",
            Q24_ts_finish_time_local = "Q24_ts_finish_time_local",
            Q25_ts_finish_time_local = "Q25_ts_finish_time_local",
            Q26_ts_finish_time_local = "Q26_ts_finish_time_local",
            Q27_ts_finish_time_local = "Q27_ts_finish_time_local",
            Q51_ts_finish_time_local = "Q51_ts_finish_time_local",
            Q52_ts_finish_time_local = "Q52_ts_finish_time_local",
            substance_is_vape_juul_or_ecigarettes = "Q19_response_cleaned_vape_juul_or_ecigarettes",
            vape_puffs = "Q24_response_cleaned",
            puffs_one_hms_local = "puffs_one_hms_local",
            puffs_many_first_hms_local = "puffs_many_first_hms_local",
            puffs_many_last_hms_local = "puffs_many_last_hms_local",
            puffs_one_datetime_local = "puffs_one_datetime_local",
            puffs_many_first_datetime_local = "puffs_many_first_datetime_local",
            puffs_many_last_datetime_local = "puffs_many_last_datetime_local")

dat_analysis <- rename(dat_analysis, all_of(lookup))

################################################################################
# Save intermediate dataset
################################################################################

dat_analysis <- dat_analysis %>% select(mars_id, decision_point, all_of(names(lookup)))

saveRDS(dat_analysis, file = file.path(path_proximal_smoking_pipeline_data, "parsed_vape_puff_times.rds"))

