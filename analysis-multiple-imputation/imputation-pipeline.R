rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Data preparation
###############################################################################
source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "create-replicated-dataset.R"))

source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "create-wide-format-dataset-for-mi.R"))

###############################################################################
# Generate imputed datasets for baseline variables
###############################################################################
source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-baseline.R"))

###############################################################################
# Generate imputed datasets for decision points within each stratum
###############################################################################
source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-01.R"))

source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-02.R"))

source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-03.R"))

###############################################################################
# Rough check
###############################################################################
source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "display-auc.R"))

###############################################################################
# Data preparation
###############################################################################
for(.__current_idx in 1:.__total_imputed_datasets){
  source("paths.R")
  source(file.path(.__path_code, "analysis-multiple-imputation", "reshape-completed-datasets-from-wide-to-long.R"))
}

###############################################################################
# Data analysis
###############################################################################
for(.__current_idx in 1:.__total_imputed_datasets){
  source("paths.R")
  source(file.path(.__path_code, "analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source("paths.R")
  source(file.path(.__path_code, "analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-study-day-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source("paths.R")
  source(file.path(.__path_code, "analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-study-day-quadratic.R"))
}


