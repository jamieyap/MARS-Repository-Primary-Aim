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

dat_long <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_long.rds")) 

###############################################################################
# Imputation for participant-decision points which are currently eligible
# for micro-randomization, but not eligible for micro-randomization in the 
# past 24 hours prior to the current decision point
###############################################################################

these_columns <- c("ffmq_nonjudge", "income_val")

dat_for_imputation <- dat_long %>%
  filter(decision_point >= 3) %>%
  filter(eligibility == 1) %>%
  filter(elig24hrs == 0) %>%
  select(participant_id, decision_point,
         is_high_effort, is_low_effort,
         quick_survey_response, Y, cigarette_counts, src_scored,
         all_of(these_columns))

my_list <- list()
LHS <- "quick_survey_response"
RHS <- paste("ffmq_nonjudge",
             "income_val",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

LHS <- "Y"
RHS <- paste("is_high_effort",
             "is_low_effort",
             "quick_survey_response",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

LHS <- "cigarette_counts"
RHS <- paste("is_high_effort",
             "is_low_effort",
             "quick_survey_response",
             "Y",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

LHS <- "src_scored"
RHS <- paste("is_high_effort",
             "is_low_effort",
             "quick_survey_response",
             "Y",
             "cigarette_counts",
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = dat_for_imputation, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_sparse_restrictions", ".rds", sep = "")))

dat_for_imputation_completed <- complete(imp, 1) 

###############################################################################
# Save
###############################################################################

dat_for_imputation_completed <- dat_for_imputation_completed %>% select(-any_of(these_columns))

saveRDS(dat_for_imputation_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_completed_sparse_restrictions", ".rds", sep = "")))

