source("paths.R")
library(tidyverse)

mi_dataset_num <- .__par_mi_number

cols_to_drop <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_drop.rds"))
cols_to_keep_control <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_control.rds"))
cols_to_keep_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
cols_to_keep_timevarying <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))
dat_wide_completed_dp60 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_completed_dp", 60, ".rds", sep = "")))

###############################################################################
# Variables that are not time-varying
###############################################################################
cols_to_keep_nontimevarying <- c("replicate_id", "participant_id", cols_to_keep_control, cols_to_keep_baseline)
dat_completed_nontimevarying_variables <- dat_wide_completed_dp60 %>% select(all_of(cols_to_keep_nontimevarying)) %>% unique(.)

###############################################################################
# Variables that are time-varying
###############################################################################
dat_for_reshaping <- dat_wide_completed_dp60 %>% select(-any_of(cols_to_keep_control)) %>% select(-any_of(cols_to_keep_baseline))

list_all <- list()

for(i in 1:length(cols_to_keep_timevarying)){
  current_cols <- paste(cols_to_keep_timevarying[i], 1:60, sep = "_dp")
  these_cols_only <- c("replicate_id", "participant_id", current_cols)
  
  dat_long_current_cols <- dat_for_reshaping %>% select(all_of(these_cols_only)) %>%
    pivot_longer(
      cols = current_cols,
      names_to = "decision_point",
      names_prefix = cols_to_keep_timevarying[i],
      values_to = cols_to_keep_timevarying[i],
      values_drop_na = FALSE,
    ) %>%
    mutate(decision_point = as.numeric(substring(.[["decision_point"]], first = 4)))
  
  list_all <- append(list_all, list(dat_long_current_cols))
}

dat_reshaped_done <- list_all %>% reduce(left_join, by = join_by("replicate_id", "participant_id", "decision_point"))

###############################################################################
# Merge into a single dataset
###############################################################################
dat_long_completed <- left_join(x = dat_completed_nontimevarying_variables, y = dat_reshaped_done, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

###############################################################################
# Save
###############################################################################
saveRDS(dat_long_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))





