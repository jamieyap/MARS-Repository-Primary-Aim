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

source("paths.R")
source(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "loop-stratum-03.R"))