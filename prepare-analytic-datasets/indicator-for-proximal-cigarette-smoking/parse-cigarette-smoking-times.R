################################################################################
# In this script, we parse reported cigarette smoking times and create 
# variables that will aid in implementing a decision rule for creating a
# binary proximal indicator of cigarette smoking within ~1 hour of
# micro-randomization.
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
# Grab the columns you will need
################################################################################
dat_analysis <- dat_analysis %>% 
  select(mars_id, 
         decision_point, 
         olson, 
         ts_coinflip_mountain,
         Q19_response,  # Substance use: select all that apply
         Q20_response,  # Since the last assessment, how many cigarettes did you smoke?
         Q21_response,  # "You said you smoked 1 cigarette since the last assessment. What time was it when you smoked that cigarette?"
         Q22_response,  # "You said you smoked more than 1 cigarette since the last assessment. What time was it when you smoked your last cigarette?"
         Q23_response,  # "You said you smoked more than 1 cigarette since the last assessment. What time was it when you smoked your first cigarette?"
         Q48_response,  # "You said you smoked 1 cigarette since the last assessment. First cigarette?"
         Q49_response,  # "You said you smoked more than 1 cigarette since the last assessment. The question on this screen is asking when you smoked your first cigarette, and the question on the next screen will ask when you smoked your most recent cigarette. What time was it when you smoked your first cigarette?"
         Q50_response,  # "You said you smoked more than 1 cigarette since the last assessment, what time was it when you smoked your most recent cigarette?"
         Q19_ts_finish_time_mountain,
         Q20_ts_finish_time_mountain,
         Q21_ts_finish_time_mountain, 
         Q22_ts_finish_time_mountain, 
         Q23_ts_finish_time_mountain,
         Q48_ts_finish_time_mountain, 
         Q49_ts_finish_time_mountain, 
         Q50_ts_finish_time_mountain)  

dat_analysis[["ts_coinflip_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_coinflip_mountain")
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

# Since grepl evaluates to FALSE when given NA as input we need to call the second
# mutate statement to preserve the NA's in the original variable
dat_analysis <- dat_analysis %>%
  mutate(Q19_response_cleaned_none = 1*grepl(pattern = "None", x = Q19_response, fixed = TRUE),
         Q19_response_cleaned_cigarettes = 1*grepl(pattern = "Cigarettes", x = Q19_response, fixed = TRUE)) %>%
  mutate(Q19_response_cleaned_none = replace(Q19_response_cleaned_none, is.na(Q19_response), NA),
         Q19_response_cleaned_cigarettes = replace(Q19_response_cleaned_cigarettes, is.na(Q19_response), NA))

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

dat_analysis <- dat_analysis %>% 
  mutate(Q20_response_cleaned = case_when(
    Q20_response == "0 (I did not smoke)" ~ 0,
    Q20_response == "Less than 1" ~ 0.5,
    Q20_response == "1" ~ 1,
    Q20_response == "2" ~ 2,
    Q20_response == "3" ~ 3,
    Q20_response == "4" ~ 4,
    Q20_response == "5" ~ 5,
    Q20_response == "6" ~ 6,
    Q20_response == "7" ~ 7,
    Q20_response == "8" ~ 8,
    Q20_response == "9" ~ 9,
    Q20_response == "10" ~ 10,
    Q20_response == "More than 10" ~ 11,
    .default = NULL))

# Note that the EMA item on number of cigarettes smoked (Q20) was skipped if the
# participant did not report to have smoked cigarettes in the EMA item (Q19)
# that asked participants to report the specific substances used.
# When Q20 was skipped, the value of Q20_response was an NA. 
# This can be seen empirically by running the code snippet below.
if(FALSE){
  dat_analysis %>% 
    group_by(Q19_response_cleaned_none,
             Q19_response_cleaned_cigarettes, Q20_response_cleaned, 
             is.na(Q21_response), is.na(Q22_response), is.na(Q23_response),
             is.na(Q48_response), is.na(Q49_response), is.na(Q50_response)) %>% 
    summarise(n())
}

# In this code, set the value of Q20_response_cleaned to zero 
# if the value of Q19_response_cleaned_cigarettes is zero.
dat_analysis <- dat_analysis %>% 
  mutate(Q20_response_cleaned = replace(Q20_response_cleaned, Q19_response_cleaned_cigarettes == 0, 0))

# Note:
# In addition, we also see that:
# * (a) there are 2 EMAs where Q19_response_cleaned_cigarettes has
#   a value of NA but Q20_response_cleaned has a value of 0
# * (b) there are 28 EMAs where Q19_response_cleaned_cigarettes has
#   a value of 1 but Q20_response_cleaned has a value of 0
# * In (a) and (b), no associated smoking time was recorded in the raw data,
#   i.e., Q21_response, Q22_response, Q23_response, Q48_response, Q49_response, Q50_response 
#   all have a value of NA.
#
# We tackle case (a) and (b) in another script (implement-decision-rule.R).

################################################################################
# Parse reported smoking time
################################################################################

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
# Create a 'daybefore' variable to help with checking when cigarettes smoked in
# relation to micro-randomization
################################################################################

dat_analysis[["cigs_one_datetime_local_daybefore"]] <- dat_analysis[["cigs_one_datetime_local"]] - days(1)
dat_analysis[["cigs_many_first_datetime_local_daybefore"]] <- dat_analysis[["cigs_many_first_datetime_local"]] - days(1)
dat_analysis[["cigs_many_last_datetime_local_daybefore"]] <- dat_analysis[["cigs_many_last_datetime_local"]] - days(1)

################################################################################
# Rename columns
################################################################################

lookup <- c(ts_coinflip_local = "ts_coinflip_local",
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
            cigs_many_last_datetime_local = "cigs_many_last_datetime_local",
            cigs_one_datetime_local_daybefore = "cigs_one_datetime_local_daybefore",
            cigs_many_first_datetime_local_daybefore = "cigs_many_first_datetime_local_daybefore",
            cigs_many_last_datetime_local_daybefore = "cigs_many_last_datetime_local_daybefore")

dat_analysis <- rename(dat_analysis, all_of(lookup))

################################################################################
# Save intermediate dataset
################################################################################

dat_analysis <- dat_analysis %>% select(mars_id, decision_point, all_of(names(lookup)))

saveRDS(dat_analysis, file = file.path(path_manipulated_data, "smoking-lapse-indicators-pipeline-data", "parsed_cigarette_smoking_times.rds"))

