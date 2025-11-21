rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# Read in dataset for demographic variables
dat_demogs <- readRDS(file = file.path(path_manipulated_data, "dat_demogs.rds"))

# Read in datasets for the distal outcomes assessed at Visit 3 and Visit 4
v3_quest <- readRDS(file.path(path_visit_outcomes, "2025-6-11-MARS_quest_v1.0.1", "v3_quest.rds"))
v4_quest_6_month <- readRDS(file.path(path_visit_outcomes, "2025-6-11-MARS_quest_v1.0.1", "v4_quest_6_month.rds"))

# Among participants who have any data from sequences which began within the
# 10-day MRT period, we identified those participants who did not complete
# at least 3 EMA between the 2nd and 9th day inclusive.
# These participant ID's are saved in the variable below.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))
mars_ids_pilot <- readRDS(file = file.path(path_manipulated_data, "mars_ids_pilot.rds"))

################################################################################
# Some data pre-processing for the Visit 3 and Visit 4 outcomes
################################################################################

# Select the columns needed to construct the following distal outcomes using Visit 3 and Visit 4 data:
# Visit 3:
#   Cigarette Abstinence: C_V3_7dayPP
#   Tobacco Abstinence: T_V3_7dayPP
#   Nicotine Abstinence: N_V3_7dayPP
#   Cigarette Relapse: C_V3_relapse
#
# Visit 4:
#   Cigarette Abstinence: C_V4_7dayPP
#   Cigarette Relapse: C_V4_relapse

dat_v3 <- v3_quest %>% select(mars_id, tac_v3_1, tac_v3_2, tac_v3_3, tac_v3_4___1, tac_v3_4___2, tac_v3_4___3, tac_v3_4___4, tac_v3_4___5, tac_v3_4___6, tac_v3_4___7, tac_v3_4___8)
dat_v4 <- v4_quest_6_month %>% select(mars_id, tac1_1_v5, tac1a_1_v5, tac1b_1_v5, tac2_1_v5, tac3_1_v5___1, tac3_1_v5___2, tac3_1_v5___3, tac3_1_v5___4, tac3_1_v5___5, tac3_1_v5___6, tac3_1_v5___7, tac3_1_v5___8)

# Inputs:
#   tac_v3_1: Have you smoked any cigarettes, even a puff, since we last saw you? (1 = Yes, 0 = No)
#   tac_v3_3: Have you used any other tobacco products since we last saw you? (1 = Yes, 0 = No; this item is asked regardless of response to tac_v3_1)
#   tac_v3_2: On how many days did you smoke? (this item is only asked if the response to tac_v3_1 was 'Yes')

# Variables created:
#   C_V3_7dayPP: 7-day cigarette abstinence
#   T_V3_7dayPP: 7-day tobacco abstinence
#   N_V3_7dayPP: 7-day nicotine abstinence
#   C_V3_relapse: Cigarette only relapse

dat_v3 <- dat_v3 %>%
  mutate(C_V3_7dayPP = if_else(tac_v3_1 == 0, 1, 0)) %>%
  mutate(T_V3_7dayPP = case_when(
    tac_v3_1 == 1 ~ 0,
    (tac_v3_1 == 0) & (tac_v3_3 == 0) ~ 1, 
    (tac_v3_1 == 0) & (tac_v3_3 == 1) & (tac_v3_4___1 + tac_v3_4___2 + tac_v3_4___4 + tac_v3_4___5 + tac_v3_4___6 + tac_v3_4___7 + tac_v3_4___8 == 0) ~ 1,
    (tac_v3_1 == 0) & (tac_v3_3 == 1) & (tac_v3_4___1 + tac_v3_4___2 + tac_v3_4___4 + tac_v3_4___5 + tac_v3_4___6 + tac_v3_4___7 + tac_v3_4___8 > 0) ~ 0,
    .default = NULL
  )) %>%
  mutate(N_V3_7dayPP = case_when(
    (tac_v3_1 == 0) & (tac_v3_3 == 0) ~ 1,
    (tac_v3_1 == 0) & (tac_v3_3 == 1) ~ 0,
    (tac_v3_1 == 1) & (tac_v3_3 == 0) ~ 0,
    (tac_v3_1 == 1) & (tac_v3_3 == 1) ~ 0,
    .default = NULL
  )) %>%
  mutate(C_V3_relapse = case_when(
    tac_v3_1 == 0 ~ 0,
    (tac_v3_1 == 1) & (tac_v3_2 < 7) ~ 0,
    (tac_v3_1 == 1) & (tac_v3_2 >= 7) ~ 1,
    .default = NULL
  ))

dat_demogs <- left_join(x = dat_demogs, y = dat_v3 %>% select(mars_id, C_V3_7dayPP, T_V3_7dayPP, N_V3_7dayPP, C_V3_relapse), by = join_by(mars_id == mars_id))

# Create a binary indicator for whether we have complete cases for analyses pertaining to a particular Visit 3 outcome
dat_demogs[["is_cc_C_V3_7dayPP"]] <- dat_demogs %>%
  select(C_V3_7dayPP) %>%
  complete.cases(.)

dat_demogs[["is_cc_T_V3_7dayPP"]] <- dat_demogs %>%
  select(T_V3_7dayPP) %>%
  complete.cases(.)

dat_demogs[["is_cc_N_V3_7dayPP"]] <- dat_demogs %>%
  select(N_V3_7dayPP) %>%
  complete.cases(.)

dat_demogs[["is_cc_C_V3_relapse"]] <- dat_demogs %>%
  select(C_V3_relapse) %>%
  complete.cases(.)

# Input:
#   tac1_1_v5: Have you smoked, even a puff, since we last saw you?
#   tac1a_1_v5: Have you smoked, even a puff, in the last 7 days? (1 = Yes, 0 = No)
#   tac1b_1_v5: You said you smoked since we last saw you in the last 7 days, was there a time when you smoked every day for 7 consecutive days? (1 = Yes, 0 = No)

# Variables created:
#   C_V4_7dayPP: 7-day cigarette abstinence
#   C_V4_relapse: Cigarette only relapse

dat_v4 <- dat_v4 %>%
  mutate(C_V4_7dayPP = case_when(
    (tac1_1_v5 == 0) & (tac1a_1_v5 == -99) ~ 1,
    (tac1_1_v5 == 1) & (tac1a_1_v5 == 0) ~ 1,
    (tac1_1_v5 == 1) & (tac1a_1_v5 == 1) ~ 0,
    .default = NULL
  )) %>%
  mutate(C_V4_relapse = case_when(
    (tac1_1_v5 == 0) & (tac1a_1_v5 == -99) & (tac1b_1_v5 == -99) ~ 0,
    (tac1_1_v5 == 1) & (tac1a_1_v5 == 0) & (tac1b_1_v5 == 0) ~ 0,
    (tac1_1_v5 == 1) & (tac1a_1_v5 == 1) & (tac1b_1_v5 == 0) ~ 0,
    (tac1_1_v5 == 1) & (tac1a_1_v5 == 1) & (tac1b_1_v5 == 1) ~ 1,
    .default = NULL
  ))


dat_demogs <- left_join(x = dat_demogs, y = dat_v4 %>% select(mars_id, C_V4_7dayPP, C_V4_relapse), by = join_by(mars_id == mars_id))

# Create a binary indicator for whether we have complete cases for analyses pertaining to a particular Visit 4 outcome
dat_demogs[["is_cc_C_V4_7dayPP"]] <- dat_demogs %>%
  select(C_V4_7dayPP) %>%
  complete.cases(.)

dat_demogs[["is_cc_C_V4_relapse"]] <- dat_demogs %>%
  select(C_V4_relapse) %>%
  complete.cases(.)

#-------------------------------------------------------------------------------
# At this point: What we need for clinicaltrials.gov
#-------------------------------------------------------------------------------

################################################################################
# First, drop data from participants which we will not be using in the
# calculation of the summary statistics.
################################################################################
dat_ctgov <- dat_demogs %>% filter(!(mars_id %in% mars_ids_pilot))

dat_ctgov %>% 
  group_by(is_cc_C_V4_7dayPP) %>%
  summarise(n())

dat_ctgov %>%
  filter(is_cc_C_V4_7dayPP == FALSE) %>%
  .[["mars_id"]] %>%
  matrix(., ncol = 1, dimnames = list(NULL, c("mars_id")))




