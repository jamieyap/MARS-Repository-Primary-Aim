###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

# Recall that dat_primary_aim_replicated.rds is an output of the script
# create-replicated-dataset.R
dat_long <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))
dat_long <- dat_long %>%
  group_by(replicate_id, participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  ungroup(.)

cond1 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 0)"
cond2 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 1)"
cond3 <- "(eligibility == 1 & eligibility_lag1 == 1)"

dat_for_imputation <- dat_long %>% filter(!!rlang::parse_expr(cond1))