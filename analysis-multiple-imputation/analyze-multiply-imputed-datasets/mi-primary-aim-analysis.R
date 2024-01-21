###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Analyze each completed dataset
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)
library(MRTAnalysis)

for(.__par_mi_number in 1:.__total_imputed_datasets){
 source(file = file.path("analysis-multiple-imputation", "analyze-one-completed-dataset", "analyze-one-completed-dataset-primary-aim.R")) 
 rm(list = ls())
}

