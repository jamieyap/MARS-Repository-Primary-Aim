source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# This dataset contains data from only those sequences which began
# within the 10-day MRT period.
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
# The following participants were excluded from all analyses:
# - Pilot participants
# - Among participants who have any data from sequences which began within the
#   10-day MRT period, those participants who did not complete
#   at least 3 EMA between the 2nd and 9th day inclusive.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_mars_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

################################################################################
# Next, create the primary proximal outcome using EMA responses
################################################################################
dat_mars_analysis[["Y"]] <- construct_primary_proximal_outcome(cleaned_data_frame = dat_mars_analysis, q1_var_name = "Q1_response", q2_var_name = "Q2_response", q3_var_name = "Q3_response")

################################################################################
# Next, create other variables needed in data analysis
################################################################################
# Participant ID must be is numeric format (not character) when performing analysis
dat_mars_analysis <- dat_mars_analysis %>% mutate(participant_id = as.numeric(substring(mars_id, first = 6))) 
# Treatment indicators must be in numeric format (not character) when performing analysis
dat_mars_analysis <- dat_mars_analysis %>% 
  mutate(eligibility = if_else(!is.na(A), 1, 0)) %>%
  mutate(coinflip = if_else(A != "none", 1, 0),
         is_high_effort = if_else(A == "mars", 1, 0),
         is_low_effort = if_else(A == "low_effort", 1, 0))

################################################################################
# Select the most basic columns needed for data analysis
################################################################################
dat_mars_analysis <- dat_mars_analysis %>%
  select(mars_id, participant_id, cluster_id, decision_point,
         eligibility, coinflip, is_high_effort, is_low_effort, Y)

# Save output
saveRDS(dat_mars_analysis, file = file.path(path_manipulated_data, "dat_mars_basic.rds"))

