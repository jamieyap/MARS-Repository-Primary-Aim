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
# Calculate summary statistics before implementing the next set of decision
# rules
################################################################################

dat_summary <- dat_analysis %>%
  group_by((cigarette_counts == 0), substance_is_cigarettes) %>%
  summarise(total = n())

print(dat_summary)

################################################################################
# View cigarette counts as a more reliable indicator of cigarette smoking 
# than substance_is_cigarettes.
################################################################################

# Based on this summary, we implement the following rule:
# If reported cigarette counts is equal to zero but either
# (1) reported cigarette smoking in the prior item within the same EMA; or
# (2) had no reported value in the prior item within the same EMA (i.e., NA)
# then we set the value for the binary indicator for any cigarette smoking
# to be equal to zero.
#
# Else if reported cigarette counts is equal to zero and in the prior item
# within the same EMA, the participant reported NO CIGARETTE SMOKING
# then set the value for the binary indicator for any cigarette smoking
# to be equal to zero.
#
# Else (if reported cigarette counts is greater than zero and in the prior item
# within the same EMA they reported CIGARETTE SMOKING) then set the value of the
# binary indicator for any cigarette smoking to be equal to 1 (at least for now;
# the actual value of the binary indicator in this case could change, pending
# the implementation of the decision rules below).

dat_analysis <- dat_analysis %>%
  mutate(substance_is_cigarettes_new = substance_is_cigarettes) %>%
  mutate(substance_is_cigarettes_new = replace(substance_is_cigarettes_new, (cigarette_counts == 0) & (substance_is_cigarettes_new == 1), 0)) %>%
  mutate(substance_is_cigarettes_new = replace(substance_is_cigarettes_new, (cigarette_counts == 0) & is.na((substance_is_cigarettes_new)), 0))

################################################################################
# Coalesce timestamps associated with several EMA items into one variable
# which represents the date time when a participant reported the time at which
# they smoked cigarettes prior to the current EMA.
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
# Create various flags for conflicting information when no more than 1 cigarette
# was smoked prior to the current EMA
#
# Recall that EMA permitted participants to say that they smoked "less than
# one cigarette". In terms of how this response was handled by EMA skip logic,
# participants were only subsequently asked to report when they smoked
# that cigarette (they were NOT asked to report when the first and most recent 
# cigarette smoked happened)
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(flag_one_cig = case_when(
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (cigs_one_datetime_local > ts_ema_local) ~ 1,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (cigs_one_datetime_local <= ts_ema_local) ~ 0,
    .default = NULL
  ))

################################################################################
# Create various flags for conflicting information when 2 or more
# cigarettes were smoked prior to the current EMA
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(flag_first_cig = case_when(
    (cigarette_counts >= 2) & (cigs_many_first_datetime_local > ts_ema_local) ~ 1,
    (cigarette_counts >= 2) & (cigs_many_first_datetime_local <= ts_ema_local) ~ 0,
    .default = NULL
  )) %>%
  mutate(flag_last_cig = case_when(
    (cigarette_counts >= 2) & (cigs_many_last_datetime_local > ts_ema_local) ~ 1,
    (cigarette_counts >= 2) & (cigs_many_last_datetime_local <= ts_ema_local) ~ 0,
    .default = NULL
  ))

dat_analysis <- dat_analysis %>%
  mutate(flag_switched_cig = case_when(
    (cigarette_counts >= 2) & (cigs_many_first_datetime_local > cigs_many_last_datetime_local) ~ 1,
    (cigarette_counts >= 2) & (cigs_many_first_datetime_local <= cigs_many_last_datetime_local) ~ 0,
    .default = NULL
  ))

################################################################################
# Calculate summary statistics before implementing the next set of decision
# rules
################################################################################

dat_summary2 <- dat_analysis %>%
  group_by((cigarette_counts == 0), ((cigarette_counts > 0) & (cigarette_counts <= 1)), (cigarette_counts >= 2)) %>%
  summarise(total = n())

dat_summary3 <- dat_analysis %>%
  filter((cigarette_counts > 0) & (cigarette_counts <= 1)) %>%
  group_by(flag_one_cig) %>%
  summarise(total = n())

dat_summary4 <- dat_analysis %>%
  filter(cigarette_counts >= 2) %>%
  group_by(flag_first_cig, flag_last_cig, flag_switched_cig) %>%
  summarise(total = n())

print(dat_summary2)
print(dat_summary3)
print(dat_summary4)

dat_summary3_by_count <- dat_analysis %>%
  filter((cigarette_counts > 0) & (cigarette_counts <= 1)) %>%
  group_by(cigarette_counts, flag_one_cig) %>%
  summarise(total = n())

dat_summary4_by_count <- dat_analysis %>%
  filter(cigarette_counts >= 2) %>%
  group_by(cigarette_counts, flag_first_cig, flag_last_cig, flag_switched_cig) %>%
  summarise(total = n())

print(dat_summary3_by_count)
print(dat_summary4_by_count)

################################################################################
# Implement decision rules for EMAs in which the participant reported smoking
# These set of rules use the working assumption that reported time when
# cigarette smoking happened is accurate but since the response options allow
# participants to report the hour and minute but not calendar date when smoking
# happened, we may use working assumptions on calendar date to employ the
# reported hour and minute cigarette smoking happened as we create the
# proximal indicator for any cigarette smoking.
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(cigs_many_first_datetime_local_new = case_when(
    (cigarette_counts >= 2) & (flag_first_cig == 0) & (flag_last_cig == 0) & (flag_switched_cig == 0) ~ cigs_many_first_datetime_local,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 0) & (flag_last_cig == 0) & (flag_switched_cig == 1) ~ cigs_many_first_datetime_local_daybefore,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 0) & (flag_last_cig == 1) & (flag_switched_cig == 0) ~ cigs_many_first_datetime_local_daybefore,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 1) & (flag_last_cig == 0) & (flag_switched_cig == 1) ~ cigs_many_first_datetime_local_daybefore,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 1) & (flag_last_cig == 1) & (flag_switched_cig == 0) ~ cigs_many_first_datetime_local_daybefore,  # this is ok
    .default = NULL
  ))

dat_analysis <- dat_analysis %>%
  mutate(cigs_many_last_datetime_local_new = case_when(
    (cigarette_counts >= 2) & (flag_first_cig == 0) & (flag_last_cig == 0) & (flag_switched_cig == 0) ~ cigs_many_last_datetime_local, # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 0) & (flag_last_cig == 0) & (flag_switched_cig == 1) ~ cigs_many_last_datetime_local,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 0) & (flag_last_cig == 1) & (flag_switched_cig == 0) ~ cigs_many_last_datetime_local_daybefore,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 1) & (flag_last_cig == 0) & (flag_switched_cig == 1) ~ cigs_many_last_datetime_local,  # this is ok
    (cigarette_counts >= 2) & (flag_first_cig == 1) & (flag_last_cig == 1) & (flag_switched_cig == 0) ~ cigs_many_last_datetime_local_daybefore,  # this is ok
    .default = NULL
  ))

dat_analysis <- dat_analysis %>%
  mutate(cigs_one_datetime_local_new = case_when(
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (flag_one_cig == 0) ~ cigs_one_datetime_local, 
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (flag_one_cig == 1) ~ cigs_one_datetime_local_daybefore, 
    .default = NULL
  ))

dat_analysis <- dat_analysis %>%
  mutate(substance_is_cigarettes_new = case_when(
    cigarette_counts == 0 ~ 0,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (ts_coinflip_local <= cigs_one_datetime_local_new) & (cigs_one_datetime_local_new < ts_ema_local) ~ 1,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!((ts_coinflip_local <= cigs_one_datetime_local_new) & (cigs_one_datetime_local_new < ts_ema_local))) ~ 0,
    (cigarette_counts >= 2) & (ts_coinflip_local <= cigs_many_last_datetime_local_new) & (cigs_many_last_datetime_local_new < ts_ema_local) ~ 1,
    (cigarette_counts >= 2) & (!((ts_coinflip_local <= cigs_many_last_datetime_local_new) & (cigs_many_last_datetime_local_new < ts_ema_local))) ~ 0,
    .default = substance_is_cigarettes_new
  ))

################################################################################
# Create various flags for conflicting information when more than 0 
# cigarettes were reported to have been smoked
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(cigarette_counts_categorical = case_when(
    (cigarette_counts == 0) ~ "no",
    (cigarette_counts > 0) & (cigarette_counts <= 1) ~ "yes - less than one or one",
    (cigarette_counts >= 2) ~ "yes - two or more",
    .default = NULL
  ))

################################################################################
# Calculate summary statistics before saving data frame
################################################################################

dat_summary5 <- dat_analysis %>% 
  group_by(substance_is_cigarettes, substance_is_cigarettes_new) %>% 
  summarise(total = n())

################################################################################
# Recode the EMA item asking about when participants smoked that 
# ONE cigarette whenever cigarettes smoked was 2 or more
################################################################################

# Whenever cigarettes smoked was 2 or more, an hour-minute value was recorded in
# the raw data for the EMA item asking about when participants smoked that 
# ONE cigarette. However, this value does not represent participant responses.
# Unusually, whenever the EMA item asking about when participants smoked that 
# ONE item was not applicable, the default value recorded was the hour-minute 
# when an EMA item was completed in the participant's local time.
# To remove any ambiguity, we code not applicable as NA, 
# rather than an hour-minute value.
dat_analysis <- dat_analysis %>%
  mutate(cigs_one_datetime_local_new = replace(cigs_one_datetime_local_new, cigarette_counts >= 2, NA))

################################################################################
# Recode the EMA item asking about when participants smoked that 
# FIRST or LAST cigarette whenever cigarettes smoked was less than 2
################################################################################

# Whenever cigarettes smoked was less than 2, we do not expect that 
# an hour-minute value be recorded in the raw data for the EMA item asking 
# about when participants smoked that FIRST or LAST cigarette.
# To remove any ambiguity, we code not applicable as NA, 
# rather than an hour-minute value.
dat_analysis <- dat_analysis %>%
  mutate(cigs_many_first_datetime_local_new = replace(cigs_many_first_datetime_local_new, (cigarette_counts > 0) & (cigarette_counts <= 1), NA)) %>%
  mutate(cigs_many_last_datetime_local_new = replace(cigs_many_last_datetime_local_new, (cigarette_counts > 0) & (cigarette_counts <= 1), NA))

################################################################################
# Select only the columns you will need
################################################################################

# Note that cigarette_counts_new and cigarette_counts_new_categorical
# should be identical to what is produced by the script.
# prepare-analytic-datasets/create-time-varying-moderator-variable-dataset.R
# Adding variables suffixed by _new is just a way to facilitate a sanity check later on.

dat_analysis <- dat_analysis %>%
  rename(cigarette_counts_new = cigarette_counts,
         cigarette_counts_new_categorical = cigarette_counts_categorical)

dat_analysis <- dat_analysis %>%
  select(mars_id, decision_point, 
         ts_coinflip_local, ts_ema_local,
         cigarette_counts_new, cigarette_counts_new_categorical,
         substance_is_cigarettes_new, cigs_one_datetime_local_new, cigs_many_first_datetime_local_new, cigs_many_last_datetime_local_new,
         substance_is_cigarettes, cigs_one_datetime_local, cigs_many_first_datetime_local, cigs_many_last_datetime_local)

################################################################################
# Save data frame
################################################################################
dat_analysis_with_metadata <- dat_analysis

dat_analysis <- dat_analysis %>%
  select(mars_id, decision_point,
         substance_is_cigarettes_new, 
         cigs_one_datetime_local_new, 
         cigs_many_first_datetime_local_new, cigs_many_last_datetime_local_new)

saveRDS(dat_analysis_with_metadata, file = file.path(path_manipulated_data, "smoking-lapse-indicators-pipeline-data", "dat_mars_proximal_cigarette_smoking_with_metadata.rds"))
saveRDS(dat_analysis, file = file.path(path_manipulated_data, "smoking-lapse-indicators-pipeline-data", "dat_mars_proximal_cigarette_smoking.rds"))


