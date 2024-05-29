rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

###############################################################################
# Data preparation
###############################################################################
source(file.path("analysis-multiple-imputation", "create-replicated-dataset.R"))
source(file.path("analysis-multiple-imputation", "create-wide-format-dataset-for-mi.R"))

###############################################################################
# Generate imputed datasets for baseline variables
###############################################################################
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-baseline.R"))

###############################################################################
# Generate imputed datasets for decision points within each stratum
###############################################################################
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-01.R"))
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-02.R"))
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-03.R"))

###############################################################################
# Rough check
###############################################################################
source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "display-auc.R"))

###############################################################################
# Data preparation
###############################################################################
for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "reshape-completed-datasets-from-wide-to-long.R"))
}

###############################################################################
# Data analysis
###############################################################################
for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-study-day-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-study-day-quadratic.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-hour-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-hour-quadratic.R"))
}

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "ppc-time-specific-means.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-study-day-linear.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-study-day-quadratic.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-hour-linear.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-hour-quadratic.R"))

###############################################################################
# Data analysis
###############################################################################
for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-study-day-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-study-day-quadratic.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-hour-linear.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-hour-quadratic.R"))
}

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-study-day-linear.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-study-day-quadratic.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-hour-linear.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-hour-quadratic.R"))

###############################################################################
# Data analysis
###############################################################################
source(file.path("analysis-multiple-imputation", "estimate-time-specific-means-for-mi-data.R"))
source(file.path("analysis-multiple-imputation", "estimate-time-specific-means-for-replicated-data.R"))

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "ppc-time-specific-means.R"))

