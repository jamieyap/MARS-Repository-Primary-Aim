source("paths.R")
library(tidyverse)

results_stratum01_linear <- read.csv(file.path(path_code, "analysis-multiple-imputation", "diagnostics", "stratum01_linear.csv"))
results_stratum02_linear <- read.csv(file.path(path_code, "analysis-multiple-imputation", "diagnostics", "stratum02_linear.csv"))
results_stratum03_linear <- read.csv(file.path(path_code, "analysis-multiple-imputation", "diagnostics", "stratum03_linear.csv"))

dat_results_linear <- full_join(x = results_stratum01_linear %>% select(predictor, screened_out_01 = screened_out), 
                                y = results_stratum02_linear %>% select(predictor, screened_out_02 = screened_out),
                                by = join_by(predictor == predictor))

dat_results_linear <- full_join(x = dat_results_linear,
                                y = results_stratum03_linear %>% select(predictor, screened_out_03 = screened_out),
                                by = join_by(predictor == predictor))

dat_results_linear

write.csv(dat_results_linear, file.path(path_code, "analysis-multiple-imputation", "diagnostics", "dat_results_linear.csv"), na = "--")

results_stratum01_quadratic <- read.csv(file.path(path_code, "analysis-multiple-imputation", "diagnostics", "stratum01_quadratic.csv"))
results_stratum02_quadratic <- read.csv(file.path(path_code, "analysis-multiple-imputation", "diagnostics", "stratum02_quadratic.csv"))
results_stratum03_quadratic <- read.csv(file.path(path_code, "analysis-multiple-imputation", "diagnostics", "stratum03_quadratic.csv"))

dat_results_quadratic <- full_join(x = results_stratum01_quadratic %>% select(predictor, screened_out_01 = screened_out), 
                                y = results_stratum02_quadratic %>% select(predictor, screened_out_02 = screened_out),
                                by = join_by(predictor == predictor))

dat_results_quadratic <- full_join(x = dat_results_quadratic,
                                y = results_stratum03_quadratic %>% select(predictor, screened_out_03 = screened_out),
                                by = join_by(predictor == predictor))

dat_results_quadratic

write.csv(dat_results_quadratic, file.path(path_code, "analysis-multiple-imputation", "diagnostics", "dat_results_quadratic.csv"), na = "--")
