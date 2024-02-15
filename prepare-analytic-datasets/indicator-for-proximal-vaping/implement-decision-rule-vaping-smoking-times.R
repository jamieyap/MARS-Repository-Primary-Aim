################################################################################
# In this script, we use parsed data as a starting point for implementing a 
# decision rule for creating a binary proximal indicator of vape smoking 
# within ~1 hour of micro-randomization.
################################################################################

rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################

parsed_vape_puff_times <- readRDS(file = file.path(path_proximal_smoking_pipeline_data, "parsed_vape_puff_times.rds"))
dat_analysis <- parsed_vape_puff_times

################################################################################
# Decision rule for determining date-time to associate with response
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(ts_ema_local = case_when(
    (vape_puffs > 0) & (vape_puffs <= 1) & (!is.na(Q25_ts_finish_time_local)) ~ Q25_ts_finish_time_local,  # EMA item used to report when they smoked "that" puff
    (vape_puffs >= 2) & (!is.na(Q26_ts_finish_time_local)) ~ Q26_ts_finish_time_local,  # EMA item used by one batch of participants to report when they had "most recent" puff
    (vape_puffs >= 2) & (!is.na(Q52_ts_finish_time_local)) ~ Q52_ts_finish_time_local,  # EMA item used by another batch of participants to report when they had the "most recent" puff
    .default = NULL
  ))

################################################################################
# Employ decision rule which uses information from reported time when
# they vaped
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(substance_is_vape_juul_or_ecigarettes_new = case_when(
    vape_puffs == 0 ~ 0,
    (vape_puffs > 0) & (vape_puffs <= 1) & (puffs_one_datetime_local > ts_coinflip_local) & (puffs_one_datetime_local <= ts_ema_local) ~ 1,
    (vape_puffs > 0) & (vape_puffs <= 1) & (!((puffs_one_datetime_local > ts_coinflip_local) & (puffs_one_datetime_local <= ts_ema_local))) ~ 0,
    (vape_puffs >= 2) & (((puffs_many_first_datetime_local > ts_coinflip_local) & (puffs_many_first_datetime_local <= ts_ema_local)) | ((puffs_many_last_datetime_local > ts_coinflip_local) & (puffs_many_last_datetime_local <= ts_ema_local))) ~ 1,
    (vape_puffs >= 2) & ((!((puffs_many_first_datetime_local > ts_coinflip_local) & (puffs_many_first_datetime_local <= ts_ema_local))) & (!((puffs_many_last_datetime_local > ts_coinflip_local) & (puffs_many_last_datetime_local <= ts_ema_local)))) ~ 0,
    .default = NA
  ))

################################################################################
# Save data frame
################################################################################
dat_analysis_with_metadata <- dat_analysis
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, substance_is_vape_juul_or_ecigarettes_new)

saveRDS(dat_analysis_with_metadata, file = file.path(path_proximal_smoking_pipeline_data, "dat_mars_proximal_vaping_with_metadata.rds"))
saveRDS(dat_analysis, file = file.path(path_proximal_smoking_pipeline_data, "dat_mars_proximal_vaping.rds"))

