###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Prepare for pooling
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 


calculate_pool_statistics <- function(results_obj, pool_manual){
  # Within imputation variance
  Ubar <- pool_manual$ubar
  # Between imputation variance
  B <- pool_manual$b
  # Total number of imputed datasets
  m <- pool_manual$m
  # Total variance
  total_var <- pool_manual$t
  # Proportion of variation attributable to missing data
  lambda <- (B + B/m) * (1/total_var)
  # Relative increase in variance due to non-response
  r <- (B + B/m) * (1/Ubar)
  # Calculate degrees of freedom
  v_com <- results_obj$causal_excursion_effect[1, "df"]
  v_obs <- ((v_com + 1)/(v_com + 3)) * v_com * (1 - lambda)
  v_old <- (m - 1)/(lambda * lambda)
  v <- (v_old * v_obs)/(v_old + v_obs)
  # Fraction of missing information
  gamma <- (1/(1 + r)) * (r + 2/(v + 3))
  
  output <- data.frame(Ubar = Ubar,
                       B = B,
                       m = m,
                       total_var = total_var,
                       lambda = lambda,
                       r = r,
                       v_com = v_com,
                       v_obs = v_obs,
                       v_old = v_old,
                       v = v,
                       gamma = gamma)
  
  return(output)
}

