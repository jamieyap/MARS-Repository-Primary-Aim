################################################################################
# Pipeline params
################################################################################
rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

###############################################################################
# Check logged events: imputations for strata that did not meet restrictions
###############################################################################
rm(list = ls())

source("paths.R")
library(tidyverse)
library(mice)

for(.idx_outer in 1:.__total_imputed_datasets){
  mi_dataset_num <- .idx_outer
  
  imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_completed_sparse_restrictions", ".rds", sep = "")))
  
  print(imp$loggedEvents)
}

###############################################################################
# Check logged events: baseline
###############################################################################
rm(list = ls())

source("paths.R")
library(tidyverse)
library(mice)

for(.idx_outer in 1:.__total_imputed_datasets){
  mi_dataset_num <- .idx_outer
  
  imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", "baseline", ".rds", sep = "")))
  
  print(imp$loggedEvents)
}

###############################################################################
# Check logged events: quick_survey_response
###############################################################################
rm(list = ls())

source("paths.R")
library(tidyverse)
library(mice)

for(.idx_outer in 1:.__total_imputed_datasets){
  mi_dataset_num <- .idx_outer
  
  for(.idx_inner in 1:.__maximum_march_forward){
    current_dp_value <- .idx_inner
    
    suffix <- paste("_dp" ,  current_dp_value, sep = "")
    imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", "quick_survey_response", suffix, ".rds", sep = "")))
    
    print(imp$loggedEvents)
  }
}

###############################################################################
# Check logged events: Y, cigarette_counts, src_scored
###############################################################################
rm(list = ls())

source("paths.R")
library(tidyverse)
library(mice)

for(.idx_outer in 1:.__total_imputed_datasets){
  mi_dataset_num <- .idx_outer
  
  for(.idx_inner in 1:.__maximum_march_forward){
    current_dp_value <- .idx_inner
    
    suffix <- paste("_dp" ,  current_dp_value, sep = "")
    imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", "ema_response", suffix, ".rds", sep = "")))
    
    print(imp$loggedEvents)
  }
}
