rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Generate imputed datasets for decision points within stratum 1
###############################################################################
for(.__current_idx in 1:5){
  for(.__current_dp in 2:6){
    source("paths.R")
    source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-stratum-03-binary-trt-01.R"))
  }
  
  for(.__current_dp in 7:20){
    source("paths.R")
    source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "impute-stratum-03-binary-trt-01.R"))
  }
}

