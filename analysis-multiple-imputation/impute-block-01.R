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

dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with("_dp1"))

dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
dat_wide_init <- dat_wide

###############################################################################
# Step 1. Impute responses to 2qs
#
# Specify a regression model for each variable individually.
# Since we only wish to impute responses to 2qs at this step, our list of 
# formulas only consist of the regression model for response to 2qs
# Other variables have missing values but since we did not specify a model 
# for them at this step, the mice package will not impute them
###############################################################################
my_list <- list()
my_list[["quick_survey_response_dp1"]] <- as.formula(paste("quick_survey_response_dp1 ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + ffmq_nonjudge"))

# Specifying the correct restriction is key to imputing MRT data.
# At the first decision point ever of the trial, we do not really have any
# EMAs assessed prior. Thus, the relevant restriction to respect is simply
# to impute missing values at blocks which had a micro-randomization.
restriction_meet_string <- "eligibility_dp1 == 1"
restriction_violate_string <- "eligibility_dp1 == 0"

# We employ a nifty coding trick that will allow us the option to put all these
# computations in a loop or function at a later time.
# This coding trick allows us to filter based on a string by calling the
# parse_expr function from the rlang package.
#
# More about this coding trick here:
# https://www.r-bloggers.com/2020/09/using-dplyrfilter-when-the-condition-is-a-string/
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 1,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
dat_wide_completed <- rbind(rows_meet_restriction_completed, rows_violate_restriction)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_quick_survey_response_dp1.rds"))

###############################################################################
# Step 2. Impute primary proximal outcome (engagement in 
# self-regulatory strategies) and other variables assessed in EMA
###############################################################################
dat_wide <- dat_wide_completed

my_list <- list()
my_list[["Y_dp1"]] <- as.formula(paste("Y_dp1 ~ is_low_effort_dp1 + is_high_effort_dp1 + cigarette_counts_dp1 + src_scored_dp1"))
my_list[["cigarette_counts_dp1"]] <- as.formula(paste("cigarette_counts_dp1 ~ is_low_effort_dp1 + is_high_effort_dp1 + Y_dp1 + src_scored_dp1 + quick_survey_response_dp1 + baseline_tobacco_history + income_val"))
my_list[["src_scored_dp1"]] <- as.formula(paste("src_scored_dp1 ~ is_low_effort_dp1 + is_high_effort_dp1 + Y_dp1 + cigarette_counts_dp1"))

# Specifying the correct restriction is key to imputing MRT data.
# At the first decision point ever of the trial, we do not really have any
# EMAs assessed prior. Thus, the relevant restriction to respect is simply
# to impute missing values of Y_1 at blocks which had a micro-randomization.
restriction_meet_string <- "eligibility_dp1 == 1"
restriction_violate_string <- "eligibility_dp1 == 0"

# We employ a nifty coding trick that will allow us the option to put all these
# computations in a loop or function at a later time.
# This coding trick allows us to filter based on a string by calling the
# parse_expr function from the rlang package.
#
# More about this coding trick here:
# https://www.r-bloggers.com/2020/09/using-dplyrfilter-when-the-condition-is-a-string/
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 

dat_wide_completed <- rbind(rows_meet_restriction_completed, rows_violate_restriction)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_ema_response_dp1.rds"))

###############################################################################
# Save
###############################################################################
saveRDS(dat_wide_init, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_init_block1.rds"))
saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_block1.rds"))

