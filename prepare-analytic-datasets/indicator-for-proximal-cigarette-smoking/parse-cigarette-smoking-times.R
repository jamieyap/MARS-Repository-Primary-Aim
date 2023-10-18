################################################################################
# In this script, we parse reported cigarette smoking times and create 
# variables that will aid in implementing a decision rule for creating a
# binary proximal indicator of cigarette smoking within ~1 hour of
# micro-randomization.
#
# The decision rule will be developed in consideration of conflicting
# information between when participants said they smoked cigarettes and 
# when EMA was assessed. More specifically, when conflicting information arises
# (i.e., measurement error) then it is handled by invoking a deterministic
# decision rule.
#
# Note that by contrast, the substance use variables created by the script
# create-time-varying-moderator-variable-dataset.R
# takes participant reported responses at face value (i.e., does not consider
# measurement error).
################################################################################

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
         Q20_response,  # Question Text: "Since the last assessment, how many cigarettes did you smoke?"
         Q21_response,  # Question Text: "You said you smoked 1 cigarette since the last assessment. What time was it when you smoked that cigarette?"
         Q22_response,  # Question Text: "You said you smoked more than 1 cigarette since the last assessment. What time was it when you smoked your last cigarette?"
         Q23_response,  # Question Text: "You said you smoked more than 1 cigarette since the last assessment. What time was it when you smoked your first cigarette?"
         Q48_response,  # Question Text: "You said you smoked 1 cigarette since the last assessment. First cigarette?"
         Q49_response,  # Question Text: "You said you smoked more than 1 cigarette since the last assessment. The question on this screen is asking when you smoked your first cigarette, and the question on the next screen will ask when you smoked your most recent cigarette. What time was it when you smoked your first cigarette?"
         Q50_response,  # Question Text: "You said you smoked more than 1 cigarette since the last assessment, what time was it when you smoked your most recent cigarette?"
         Q19_ts_finish_time_mountain,
         Q20_ts_finish_time_mountain,
         Q21_ts_finish_time_mountain, 
         Q22_ts_finish_time_mountain, 
         Q23_ts_finish_time_mountain,
         Q48_ts_finish_time_mountain, 
         Q49_ts_finish_time_mountain, 
         Q50_ts_finish_time_mountain)  

dat_analysis[["ts_coinflip_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_coinflip_mountain")
dat_analysis[["ts_ema_triggered_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_ema_triggered_mountain")
dat_analysis[["ts_status_survey_ema_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_status_survey_ema_mountain")

dat_analysis[["Q19_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q19_ts_finish_time_mountain")
dat_analysis[["Q20_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q20_ts_finish_time_mountain")
dat_analysis[["Q21_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q21_ts_finish_time_mountain")
dat_analysis[["Q22_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q22_ts_finish_time_mountain")
dat_analysis[["Q23_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q23_ts_finish_time_mountain")
dat_analysis[["Q48_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q48_ts_finish_time_mountain")
dat_analysis[["Q49_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q49_ts_finish_time_mountain")
dat_analysis[["Q50_ts_finish_time_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "Q50_ts_finish_time_mountain")

################################################################################
# Create an indicator for any substance use and an indicator for any
# cigarette use
################################################################################

dat_analysis <- clean_response_substance_use_multiple_select(dat_analysis)

# Sanity check: was it possible to simultaneously report none and cigarettes?
# There was no occurrence of this scenario based on the check below.
if(FALSE){
  dat_analysis %>% 
    group_by(Q19_response_cleaned_none, Q19_response_cleaned_cigarettes) %>% 
    summarise(n())
}

################################################################################
# Create a variable capturing number of cigarettes smoked
################################################################################

dat_analysis[["Q20_response_cleaned"]] <- clean_response_cigarette_counts(dat_analysis)

if(FALSE){
  dat_analysis %>% 
    group_by(Q19_response_cleaned_none, Q19_response_cleaned_cigarettes, Q20_response_cleaned==0) %>% 
    summarise(n())
}

# Note:
# In addition, we also see that:
# * (a) there are 2 EMAs where Q19_response_cleaned_cigarettes has
#   a value of NA but Q20_response_cleaned has a value of 0
# * (b) there are 28 EMAs where Q19_response_cleaned_cigarettes has
#   a value of 1 but Q20_response_cleaned has a value of 0
#
# We tackle case (a) and (b) in another script (implement-decision-rule.R).

################################################################################
# Parse reported smoking time
################################################################################

# Note that EMA item wording changed during the study.
# The code below creates a variable that tags whether an EMA used the early or
# late version of the EMA item wording.
dat_analysis[["item_version_cigarette_when_smoked"]] <- identify_item_version_cigarette_when_smoked(dat_analysis)

dat_analysis <- dat_analysis %>%
  mutate(Q21_response_parsed = hms(Q21_response, quiet = TRUE), 
         Q22_response_parsed = hms(Q22_response, quiet = TRUE), 
         Q23_response_parsed = hms(Q23_response, quiet = TRUE),
         Q48_response_parsed = hms(Q48_response, quiet = TRUE),
         Q49_response_parsed = hms(Q49_response, quiet = TRUE),
         Q50_response_parsed = hms(Q50_response, quiet = TRUE)) 

dat_analysis[["cigs_one_hms_local"]] <- coalesce(dat_analysis[["Q21_response_parsed"]], dat_analysis[["Q48_response_parsed"]])
dat_analysis[["cigs_many_first_hms_local"]] <- coalesce(dat_analysis[["Q23_response_parsed"]], dat_analysis[["Q49_response_parsed"]])
dat_analysis[["cigs_many_last_hms_local"]] <- coalesce(dat_analysis[["Q22_response_parsed"]], dat_analysis[["Q50_response_parsed"]])

dat_analysis[["cigs_one_datetime_local"]] <- dat_analysis[["ts_coinflip_local"]]
dat_analysis[["cigs_many_first_datetime_local"]] <- dat_analysis[["ts_coinflip_local"]]
dat_analysis[["cigs_many_last_datetime_local"]] <- dat_analysis[["ts_coinflip_local"]]

hour(dat_analysis[["cigs_one_datetime_local"]]) <- hour(dat_analysis[["cigs_one_hms_local"]])
minute(dat_analysis[["cigs_one_datetime_local"]]) <- minute(dat_analysis[["cigs_one_hms_local"]])
second(dat_analysis[["cigs_one_datetime_local"]]) <- second(dat_analysis[["cigs_one_hms_local"]])

hour(dat_analysis[["cigs_many_first_datetime_local"]]) <- hour(dat_analysis[["cigs_many_first_hms_local"]])
minute(dat_analysis[["cigs_many_first_datetime_local"]]) <- minute(dat_analysis[["cigs_many_first_hms_local"]])
second(dat_analysis[["cigs_many_first_datetime_local"]]) <- second(dat_analysis[["cigs_many_first_hms_local"]])

hour(dat_analysis[["cigs_many_last_datetime_local"]]) <- hour(dat_analysis[["cigs_many_last_hms_local"]])
minute(dat_analysis[["cigs_many_last_datetime_local"]]) <- minute(dat_analysis[["cigs_many_last_hms_local"]])
second(dat_analysis[["cigs_many_last_datetime_local"]]) <- second(dat_analysis[["cigs_many_last_hms_local"]])

################################################################################
# Rename columns
################################################################################

lookup <- c(olson = "olson",
            ts_coinflip_mountain = "ts_coinflip_mountain",
            ts_coinflip_local = "ts_coinflip_local",
            status_survey_ema = "status_survey_ema",
            status_survey_ema_collapsed = "status_survey_ema_collapsed",
            item_version_cigarette_when_smoked = "item_version_cigarette_when_smoked",
            ts_ema_triggered_mountain = "ts_ema_triggered_mountain",
            ts_status_survey_ema_mountain = "ts_status_survey_ema_mountain",
            Q19_ts_finish_time_local = "Q19_ts_finish_time_local",
            Q20_ts_finish_time_local = "Q20_ts_finish_time_local",
            Q21_ts_finish_time_local = "Q21_ts_finish_time_local",
            Q22_ts_finish_time_local = "Q22_ts_finish_time_local",
            Q23_ts_finish_time_local = "Q23_ts_finish_time_local",
            Q48_ts_finish_time_local = "Q48_ts_finish_time_local",
            Q49_ts_finish_time_local = "Q49_ts_finish_time_local",
            Q50_ts_finish_time_local = "Q50_ts_finish_time_local",
            substance_is_cigarettes = "Q19_response_cleaned_cigarettes",
            cigarette_counts = "Q20_response_cleaned",
            cigs_one_hms_local = "cigs_one_hms_local",
            cigs_many_first_hms_local = "cigs_many_first_hms_local",
            cigs_many_last_hms_local = "cigs_many_last_hms_local",
            cigs_one_datetime_local = "cigs_one_datetime_local",
            cigs_many_first_datetime_local = "cigs_many_first_datetime_local",
            cigs_many_last_datetime_local = "cigs_many_last_datetime_local")

dat_analysis <- rename(dat_analysis, all_of(lookup))

################################################################################
# Save intermediate dataset
################################################################################

dat_analysis <- dat_analysis %>% select(mars_id, decision_point, all_of(names(lookup)))

saveRDS(dat_analysis, file = file.path(path_proximal_smoking_pipeline_data, "parsed_cigarette_smoking_times.rds"))

