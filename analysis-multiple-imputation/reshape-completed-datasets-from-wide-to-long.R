###############################################################################
# Input arguments to this script
###############################################################################
rm(list = ls())
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

################################################################################
# Load datasets
################################################################################
source("paths.R")
library(tidyverse)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
# When running this script within imputation-pipeline.R, the package MASS may still be loaded in the global environment
select <- dplyr::select 

mi_dataset_num <- .__current_idx
total_decision_points <- .__maximum_march_forward

cols_to_drop <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_drop.rds"))
cols_to_keep_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
cols_to_keep_timevarying <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))
dat_wide_completed_final_dp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_completed_dp", total_decision_points, ".rds", sep = "")))
dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))

###############################################################################
# Variables that are time-varying
###############################################################################
dat_for_reshaping <- dat_wide_completed_final_dp %>% select(-any_of(cols_to_keep_baseline))

list_all <- list()

for(i in 1:length(cols_to_keep_timevarying)){
  current_cols <- paste(cols_to_keep_timevarying[i], 1:total_decision_points, sep = "_dp")
  these_cols_only <- c("replicate_id", "participant_id", current_cols)
  
  dat_long_current_cols <- dat_for_reshaping %>% select(all_of(these_cols_only)) %>%
    pivot_longer(
      cols = all_of(current_cols),
      names_to = "decision_point",
      names_prefix = cols_to_keep_timevarying[i],
      values_to = cols_to_keep_timevarying[i],
      values_drop_na = FALSE) %>%
    mutate(decision_point = as.numeric(substring(.[["decision_point"]], first = 4)))
  
  list_all <- append(list_all, list(dat_long_current_cols))
}

dat_reshaped_done <- list_all %>% reduce(left_join, by = join_by("replicate_id", "participant_id", "decision_point"))

###############################################################################
# Merge into a single dataset
###############################################################################
dat_long_completed <- left_join(x = dat_wide_completed_baseline, y = dat_reshaped_done, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

dat_long_completed <- dat_long_completed %>% mutate(engagement_most_recent_eligible = replace(engagement_most_recent_eligible, any_recent_eligible_dp == 0, 0))

###############################################################################
# Save
###############################################################################
saveRDS(dat_long_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))

