rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "create-vars-for-mi.R"))
rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "create-replicated-dataset.R"))
rm(list = ls())

source(file = file.path("analysis-multiple-imputation", "create-wide-format-dataset-for-mi.R"))
rm(list = ls())

################################################################################
# Pipeline params
################################################################################
rm(list = ls())

.__total_imputed_datasets <- 10

################################################################################
# Sequentially march forward in time
################################################################################
rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "impute-baseline.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", "baseline", sep = ""))
  rm(list = ls())
  
  source(file = file.path("analysis-multiple-imputation", "impute-block-01.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", 1, sep = ""))
  rm(list = ls())
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "impute-block-02.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", 2, sep = ""))
  rm(list = ls())
}

rm(list = ls())
  
for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "impute-sparse-restrictions-c.R"))
  print(paste("MI dataset no.: ", .__par_mi_number, " Handle sparse restrictions -- prepare for decision point no. 3 onward", sep = ""))
  rm(list = ls())
  
  for(.idx_inner in 3:58){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "impute-block-03-c.R"))
    print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
    rm(list = ls())
  }
  
  for(.idx_inner in 59){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "impute-block-04-c.R"))
    print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
    rm(list = ls())
  }
  for(.idx_inner in 60){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "impute-block-05-c.R"))
    print(paste("MI dataset no.: ", .__par_mi_number, " Decision point no.: ", .__par_decision_point_now, sep = ""))
    rm(list = ls())
  }
}

rm(list = ls())

################################################################################
# Create diagnostics
################################################################################

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  source(file = file.path("analysis-multiple-imputation", "diagnostics-baseline.R"))
  rm(list = ls())
  
  source(file = file.path("analysis-multiple-imputation", "diagnostics-block-01.R"))
  rm(list = ls())
  
  source(file = file.path("analysis-multiple-imputation", "diagnostics-block-02.R"))
  rm(list = ls())
}

rm(list = ls())

for(.idx_outer in 1:.__total_imputed_datasets){
  .__par_mi_number <- .idx_outer
  
  for(.idx_inner in 3:.__maximum_march_forward){
    .__par_decision_point_now <- .idx_inner
    
    source(file = file.path("analysis-multiple-imputation", "diagnostics-block-03.R"))
    rm(list = ls())
  }
}

