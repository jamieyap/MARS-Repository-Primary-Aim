################################################################################
# Pipeline params
################################################################################
rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

################################################################################
# These are data pre-processing steps
################################################################################

source(file = file.path("analysis-multiple-imputation", "create-vars-for-mi.R"))
rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "create-replicated-dataset.R"))
rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "create-wide-format-dataset-for-mi.R"))
rm(list = ls())

################################################################################
# Sequentially march forward in time: baseline
################################################################################
rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-baseline.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", "baseline", sep = ""))
  rm(list = ls())
}

################################################################################
# Sequentially march forward in time: block 1
################################################################################
rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-01.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", 1, sep = ""))
  rm(list = ls())
}

################################################################################
# Sequentially march forward in time: block 2
################################################################################
rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-02.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", 2, sep = ""))
  rm(list = ls())
}

################################################################################
# Two mutually exclusive strata are handled differently when imputing for
# blocks 3-60
#
# Strata 1: Does not meet restrictions -- impute all at once
# Strata 2: Meet restrictions -- impute sequentially
################################################################################

# Imputation for strata 1 -----------------------------------------------------

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-sparse-restrictions-c.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Handle sparse restrictions -- prepare for decision point no. 3 onward", sep = ""))
  rm(list = ls())
}

# Imputation for strata 2 -----------------------------------------------------

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  .__par_decision_point_now <- 3
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-03-c.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
  rm(list = ls())
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  .__par_decision_point_now <- 4
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-03-c.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
  rm(list = ls())
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  .__par_decision_point_now <- 5
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-03-c.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
  rm(list = ls())
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  .__par_decision_point_now <- 6
  
  source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-03-c.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
  rm(list = ls())
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  for(.idx_inner in 7:58){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-03-c.R"))
    print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
    rm(list = ls())
  }
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  for(.idx_inner in 59){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-04-c.R"))
    print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
    rm(list = ls())
  }
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  for(.idx_inner in 60){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-block-05-c.R"))
    print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
    rm(list = ls())
  }
}

################################################################################
# These are data post-processing steps in preparation for analysis
################################################################################

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "reshape-completed-datasets-from-wide-to-long.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, sep = ""))
  rm(list = ls())
}

################################################################################
# Perform analysis on each imputed dataset
################################################################################

rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "analyze-multiply-imputed-datasets", "mi-primary-aim-analysis.R"))

rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "analyze-multiply-imputed-datasets", "pool-primary.R"))

rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "analyze-multiply-imputed-datasets", "mi-secondary-aim-analysis.R"))

rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "analyze-multiply-imputed-datasets", "pool-secondary.R"))

rm(list = ls())


