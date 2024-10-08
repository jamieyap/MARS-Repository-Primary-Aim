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

.__use_all_days <- FALSE
.__sensitivity_using_deterministically_imputed_proximal_outcome <- FALSE

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

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal-risk-difference-scale.R"))
}

###############################################################################
# Data analysis (sensitivity analysis)
# Use all 10 days to estimate average main effect of prompt vs no prompt
###############################################################################

.__use_all_days <- TRUE
.__sensitivity_using_deterministically_imputed_proximal_outcome <- FALSE


for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal-risk-difference-scale.R"))
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
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-primary-risk-difference-scale.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity-pool-primary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity-pool-primary-risk-difference-scale.R"))

###############################################################################
# Data analysis
###############################################################################

.__use_all_days <- FALSE
.__sensitivity_using_deterministically_imputed_proximal_outcome <- FALSE

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

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal-risk-difference-scale.R"))
}

###############################################################################
# Data analysis (sensitivity analysis)
# Use all 10 days to estimate average main effect of more effortful prompt
# vs low effort prompt
###############################################################################

.__use_all_days <- TRUE
.__sensitivity_using_deterministically_imputed_proximal_outcome <- FALSE

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal-risk-difference-scale.R"))
}

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-study-day-linear.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-study-day-quadratic.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-hour-linear.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-hour-quadratic.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "pool-secondary-risk-difference-scale.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity-pool-secondary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity-pool-secondary-risk-difference-scale.R"))

###############################################################################
# Data analysis
###############################################################################
source(file.path("analysis-multiple-imputation", "estimate-time-specific-means-for-mi-data.R"))
source(file.path("analysis-multiple-imputation", "estimate-time-specific-means-for-replicated-data.R"))

###############################################################################
# Pooling estimates across imputed datasets and posterior predictive checking
###############################################################################
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "ppc-time-specific-means.R"))

###############################################################################
# Data analysis (sensitivity analysis)
# Impute missing values in the baseline covariates
# However, for the primary proximal outcome at eligible decision points ... 
#
#   - Sensitivity Analysis #1 (Conservative assumption): 
#     Assume that non-completion of EMA means that the participant did NOT
#     engage in self-regulatory strategies
###############################################################################

source(file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-impute-proximal-outcome-deterministically.R"))

.__use_all_days <- FALSE
.__sensitivity_using_deterministically_imputed_proximal_outcome <- TRUE
.__use_deterministic_rule_conservative <- TRUE

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal-risk-difference-scale.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal-risk-difference-scale.R"))
}


source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity2-pool-primary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity2-pool-primary-risk-difference-scale.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity2-pool-secondary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity2-pool-secondary-risk-difference-scale.R"))


###############################################################################
# Data analysis (sensitivity analysis)
# Impute missing values in the baseline covariates
# However, for the primary proximal outcome at eligible decision points ... 
#
#   - Sensitivity Analysis #2 (Liberal assumption):
#     Assume that non-completion of EMA means that the participant 
#     engaged in self-regulatory strategies
###############################################################################

.__use_all_days <- FALSE
.__sensitivity_using_deterministically_imputed_proximal_outcome <- TRUE
.__use_deterministic_rule_conservative <- FALSE

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim-marginal-risk-difference-scale.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal.R"))
}

for(.__current_idx in 1:.__total_imputed_datasets){
  source(file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-secondary-aim-marginal-risk-difference-scale.R"))
}


source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity3-pool-primary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity3-pool-primary-risk-difference-scale.R"))

source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity3-pool-secondary.R"))
source(file.path("analysis-multiple-imputation", "pool-and-ppc", "sensitivity3-pool-secondary-risk-difference-scale.R"))


