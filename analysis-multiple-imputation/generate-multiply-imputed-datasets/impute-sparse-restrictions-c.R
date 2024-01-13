###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters for generating the current completed dataset
###############################################################################
mi_dataset_num <- .__par_mi_number
use_maxit_value <- .__par_maxit_value

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
  group_by(participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  ungroup(.)

dat_long[["Y"]] <- as_factor(dat_long[["Y"]])
dat_long[["quick_survey_response"]] <- as_factor(dat_long[["quick_survey_response"]])
dat_long[["has_partner"]] <- as_factor(dat_long[["has_partner"]])
dat_long <- dat_long %>% filter(decision_point >= 3)

###############################################################################
# Imputation for participant-decision points which are currently eligible
# for micro-randomization, but not eligible for micro-randomization in the 
# past 24 hours prior to the current decision point
###############################################################################

# STEP 1: Set up dataset for imputation ---------------------------------------

cond1 <- "(eligibility == 1 & eligibility_lag1 == 1 & elig24hrs == 0)"
cond2 <- "(eligibility == 1 & eligibility_lag1 == 0 & elig24hrs == 1)"
cond3 <- "(eligibility == 1 & eligibility_lag1 == 0 & elig24hrs == 0)"
restriction_meet_string <- paste(cond1, cond2, cond3, sep = " | ")
dat_for_imputation <- dat_long %>% filter(!!rlang::parse_expr(restriction_meet_string))

dat_for_imputation <- dat_for_imputation %>%
  select(replicate_id, participant_id, decision_point,
         is_high_effort, is_low_effort,
         quick_survey_response, Y, cigarette_counts, src_scored)

dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
# Note that the variables specified below would not have missing values anymore
# since any missing values would have been imputed at a previous step
historical_vars_to_use <- c("income_val", "is_female", "mdes_pos_mean", "mdes_neg_mean", "gratitude", "baseline_tobacco_history", "srq_mean")
# Note that we separate out replicate_id and participant_id because we will retain these columns but drop all the variables listed in historical_vars_to_use
these_columns <- c("replicate_id", "participant_id", historical_vars_to_use)
dat_baseline_for_merging <- dat_wide_completed_baseline %>% select(all_of(these_columns))
dat_for_imputation <- left_join(x = dat_for_imputation, y = dat_baseline_for_merging, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

# STEP 2: Specify imputation models -------------------------------------------

my_list <- list()
LHS <- "quick_survey_response"
RHS <- paste("baseline_tobacco_history",
             "is_female",
             "income_val",
             "mdes_pos_mean",
             "mdes_neg_mean",
             "gratitude",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

LHS <- "Y"
RHS <- paste("baseline_tobacco_history",
             "gratitude",
             "is_high_effort",
             "is_low_effort",
             "cigarette_counts",
             "src_scored",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

LHS <- "cigarette_counts"
RHS <- paste("baseline_tobacco_history",
             "srq_mean",
             "income_val",
             "quick_survey_response",
             "Y",
             "src_scored",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

LHS <- "src_scored"
RHS <- paste("baseline_tobacco_history",
             "srq_mean",
             "Y",
             "cigarette_counts",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

# STEP 3: Create completed dataset --------------------------------------------

imp <- mice(data = dat_for_imputation, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_sparse_restrictions", ".rds", sep = "")))

dat_for_imputation_completed <- complete(imp, 1) 

###############################################################################
# Save
###############################################################################
dat_for_imputation_completed <- dat_for_imputation_completed %>% select(-any_of(historical_vars_to_use))

saveRDS(dat_for_imputation_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_completed_sparse_restrictions", ".rds", sep = "")))

