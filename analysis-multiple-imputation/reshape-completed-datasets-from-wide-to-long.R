source("paths.R")
library(tidyverse)

cols_to_drop <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_drop.rds"))
cols_to_keep_control <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_control.rds"))
cols_to_keep_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_baseline.rds"))
cols_to_keep_timevarying <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "cols_to_keep_timevarying.rds"))

dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))

dat_for_reshaping <- dat_timevarying_wide

for(i in 1:length(cols_to_keep_timevarying)){
  current_cols <- paste(cols_to_keep_timevarying[i], 1:60, sep = "_dp")
  
  dat_long_current_cols <- dat_for_reshaping %>%
    select("replicate_id", "participant_id", all_of(current_cols)) %>%
    pivot_longer(
      cols = current_cols,
      names_to = "decision_point",
      names_prefix = cols_to_keep_timevarying[i],
      values_to = cols_to_keep_timevarying[i],
      values_drop_na = TRUE
    ) %>%
    mutate(decision_point = as.numeric(substring(.[["decision_point"]], first = 4)))
}

