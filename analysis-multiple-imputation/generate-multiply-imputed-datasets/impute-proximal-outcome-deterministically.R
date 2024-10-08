rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Imputation number
###############################################################################
mi_dataset_num <- .__current_idx  # Change the right hand side of this line if not running within a loop
use_maxit_value <- .__par_maxit_value

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(pROC)
library(MASS)
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

# A function for throwing an error
check_throw_error <- function(x) {
  stopifnot(x == TRUE)
}

# Read in completed dataset from previous time-point
dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
these_cols_baseline <- colnames(dat_wide_completed_baseline)
these_cols_baseline <- these_cols_baseline[!(these_cols_baseline %in% c("replicate_id", "participant_id"))]

###############################################################################
#                                                                             #
#                 Impute missing proximal outcome deterministically           #
#                                                                             #
###############################################################################

# Recall that dat_primary_aim_replicated.rds is an 
# output of the script create-replicated-dataset.R
dat_long <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))
dat_long <- dat_long %>% select(-any_of(these_cols_baseline))
dat_long_merged <- left_join(x = dat_wide_completed_baseline, y = dat_long, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

dat_long_merged <- dat_long_merged %>%
  mutate(Y_conservative = Y,
         Y_liberal = Y) %>%
  mutate(Y_conservative = replace(Y_conservative, (eligibility == 1) & (is.na(Y_conservative)), 0),
         Y_liberal = replace(Y_liberal, (eligibility == 1) & (is.na(Y_liberal)), 1))

###############################################################################
# Save
###############################################################################
saveRDS(dat_long_merged, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_impute_proximal_outcome_deterministically.rds", sep = "")))

