rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Generate imputed datasets for decision points within stratum 3
###############################################################################
source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-baseline.R"))

source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-01.R"))

source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-02.R"))

for(.__current_idx in 1:.__total_imputed_datasets){
  for(.__current_dp in 2){
    source("paths.R")
    source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-stratum-03-binary-trt-01.R"))
  }
}

for(.__current_idx in 1:.__total_imputed_datasets){
  for(.__current_dp in 3:54){
    source("paths.R")
    source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-stratum-03-binary-trt-02.R"))
  }
}

for(.__current_idx in 1:.__total_imputed_datasets){
  for(.__current_dp in 55:60){
    source("paths.R")
    source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-stratum-03-binary-trt-01.R"))
  }
}


