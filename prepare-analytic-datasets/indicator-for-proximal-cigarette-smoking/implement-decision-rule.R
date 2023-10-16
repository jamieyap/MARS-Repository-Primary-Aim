################################################################################
# In this script, we use parsed data as a starting point for implementing a 
# decision rule for creating a binary proximal indicator of cigarette smoking 
# within ~1 hour of micro-randomization.
################################################################################

rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################

parsed_cigarette_smoking_times <- readRDS(file = file.path(path_manipulated_data, "smoking-lapse-indicators-pipeline-data", "parsed_cigarette_smoking_times.rds"))
dat_analysis <- parsed_cigarette_smoking_times

################################################################################
# Decision rule for determining date-time to associate with response
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(ts_ema_local = case_when(
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!is.na(Q21_ts_finish_time_local)) ~ Q21_ts_finish_time_local,  # EMA item used by one batch of participants to report when they smoked "that" cigarette 
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!is.na(Q48_ts_finish_time_local)) ~ Q48_ts_finish_time_local,  # EMA item used by another batch of participants to report when they smoked "that" cigarette
    (cigarette_counts >= 2) & (!is.na(Q22_ts_finish_time_local)) ~ Q22_ts_finish_time_local,  # EMA item used by one batch of participants to report when the moked the "most recent" cigarette
    (cigarette_counts >= 2) & (!is.na(Q50_ts_finish_time_local)) ~ Q50_ts_finish_time_local,  # EMA item used by another batch of participants to report when the moked the "most recent" cigarette
    .default = NULL
  ))

################################################################################
# Employ decision rule which uses information from reported time when
# cigarettes smoked
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(substance_is_cigarettes_new = case_when(
    cigarette_counts == 0 ~ 0,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (cigs_one_datetime_local > ts_coinflip_local) & (cigs_one_datetime_local <= ts_ema_local) ~ 1,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!((cigs_one_datetime_local > ts_coinflip_local) & (cigs_one_datetime_local <= ts_ema_local))) ~ 0,
    (cigarette_counts >= 2) & (((cigs_many_first_datetime_local > ts_coinflip_local) & (cigs_many_first_datetime_local <= ts_ema_local)) | ((cigs_many_last_datetime_local > ts_coinflip_local) & (cigs_many_last_datetime_local <= ts_ema_local))) ~ 1,
    (cigarette_counts >= 2) & ((!((cigs_many_first_datetime_local > ts_coinflip_local) & (cigs_many_first_datetime_local <= ts_ema_local))) & (!((cigs_many_last_datetime_local > ts_coinflip_local) & (cigs_many_last_datetime_local <= ts_ema_local)))) ~ 0,
    .default = NA
  ))

################################################################################
# Save data frame
################################################################################
dat_analysis_with_metadata <- dat_analysis
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, substance_is_cigarettes_new)

saveRDS(dat_analysis_with_metadata, file = file.path(path_manipulated_data, "smoking-lapse-indicators-pipeline-data", "dat_mars_proximal_cigarette_smoking_with_metadata.rds"))
saveRDS(dat_analysis, file = file.path(path_manipulated_data, "smoking-lapse-indicators-pipeline-data", "dat_mars_proximal_cigarette_smoking.rds"))

