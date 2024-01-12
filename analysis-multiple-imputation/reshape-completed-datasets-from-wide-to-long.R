source("paths.R")
library(tidyverse)

cols_to_drop <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_drop.rds"))
cols_to_keep_control <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_control.rds"))
cols_to_keep_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
cols_to_keep_timevarying <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))

dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))

dat_for_reshaping <- dat_timevarying_wide

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
