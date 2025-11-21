rm(list = ls())

###############################################################################
# Load packages
###############################################################################
source("paths.R")
library(tidyverse)
library(geepack)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

expit <- function(x){
  exp(x)/(1+exp(x))
}

###############################################################################
# Calculate aggregate summary statistics about outcomes
###############################################################################
# This is the data frame that is the output of the very last step of our data pipeline
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 
dat_primary_aim <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

dat_summary_outcomes <- dat_primary_aim %>%
  summarise(prop_Y = mean(Y, na.rm = TRUE),
            prop_cig = mean(substance_is_cigarettes, na.rm = TRUE),
            prop_nic = mean(substance_is_any_nicotine, na.rm = TRUE),
            prop_tobacco = mean(substance_is_any_tobacco, na.rm = TRUE))

print(dat_summary_outcomes)

# > print(dat_summary_outcomes)
# prop_Y prop_cig  prop_nic prop_tobacco
# 1 0.6191042 0.334409 0.3440904      0.34167

fit <- geeglm(Y ~ 1, family = binomial, data = dat_primary_aim, id = participant_id, waves = decision_point)
Vmat <- vcov(fit)

est_logodds_scale <- fit$coefficients
std_err_logodds_scale <- sqrt(diag(Vmat))
LB95_logodds_scale <- est_logodds_scale - qnorm(0.975)*std_err_logodds_scale
UB95_logodds_scale <- est_logodds_scale + qnorm(0.975)*std_err_logodds_scale

est_probability_scale <- expit(fit$coefficients)
LB95_probability_scale <- expit(LB95_logodds_scale)
UB95_probability_scale <- expit(UB95_logodds_scale)

dat_output <- data.frame(est_logodds_scale = est_logodds_scale,
                         LB95_logodds_scale = LB95_logodds_scale,
                         UB95_logodds_scale = UB95_logodds_scale,
                         est_probability_scale = est_probability_scale,
                         LB95_probability_scale = LB95_probability_scale,
                         UB95_probability_scale = UB95_probability_scale)

print(dat_output)

# > print(dat_output)
# est_logodds_scale LB95_logodds_scale UB95_logodds_scale est_probability_scale LB95_probability_scale UB95_probability_scale
# (Intercept)         0.4857479          0.2037249          0.7677709             0.6191042              0.5507558              0.6830385

fit <- geeglm(1*(Y==0) ~ 1, family = binomial, data = dat_primary_aim, id = participant_id, waves = decision_point)
Vmat <- vcov(fit)

est_logodds_scale <- fit$coefficients
std_err_logodds_scale <- sqrt(diag(Vmat))
LB95_logodds_scale <- est_logodds_scale - qnorm(0.975)*std_err_logodds_scale
UB95_logodds_scale <- est_logodds_scale + qnorm(0.975)*std_err_logodds_scale

est_probability_scale <- expit(fit$coefficients)
LB95_probability_scale <- expit(LB95_logodds_scale)
UB95_probability_scale <- expit(UB95_logodds_scale)

dat_output <- data.frame(est_logodds_scale = est_logodds_scale,
                         LB95_logodds_scale = LB95_logodds_scale,
                         UB95_logodds_scale = UB95_logodds_scale,
                         est_probability_scale = est_probability_scale,
                         LB95_probability_scale = LB95_probability_scale,
                         UB95_probability_scale = UB95_probability_scale)

print(dat_output)

# > print(dat_output)
# est_logodds_scale LB95_logodds_scale UB95_logodds_scale est_probability_scale LB95_probability_scale UB95_probability_scale
# (Intercept)        -0.4857479         -0.7677709         -0.2037249             0.3808958              0.3169615              0.4492442

fit <- geeglm(substance_is_cigarettes ~ 1, family = binomial, data = dat_primary_aim, id = participant_id, waves = decision_point)
Vmat <- vcov(fit)

est_logodds_scale <- fit$coefficients
std_err_logodds_scale <- sqrt(diag(Vmat))
LB90_logodds_scale <- est_logodds_scale - qnorm(0.95)*std_err_logodds_scale
UB90_logodds_scale <- est_logodds_scale + qnorm(0.95)*std_err_logodds_scale

est_probability_scale <- expit(fit$coefficients)
LB90_probability_scale <- expit(LB90_logodds_scale)
UB90_probability_scale <- expit(UB90_logodds_scale)

dat_output <- data.frame(est_logodds_scale = est_logodds_scale,
                         LB90_logodds_scale = LB90_logodds_scale,
                         UB90_logodds_scale = UB90_logodds_scale,
                         est_probability_scale = est_probability_scale,
                         LB90_probability_scale = LB90_probability_scale,
                         UB90_probability_scale = UB90_probability_scale)

print(dat_output)

# > print(dat_output)
# est_logodds_scale LB90_logodds_scale UB90_logodds_scale est_probability_scale LB90_probability_scale UB90_probability_scale
# (Intercept)        -0.6883104         -0.8960187         -0.4806022              0.334409              0.2898694              0.3821099

fit <- geeglm(substance_is_any_nicotine ~ 1, family = binomial, data = dat_primary_aim, id = participant_id, waves = decision_point)
Vmat <- vcov(fit)

est_logodds_scale <- fit$coefficients
std_err_logodds_scale <- sqrt(diag(Vmat))
LB90_logodds_scale <- est_logodds_scale - qnorm(0.95)*std_err_logodds_scale
UB90_logodds_scale <- est_logodds_scale + qnorm(0.95)*std_err_logodds_scale

est_probability_scale <- expit(fit$coefficients)
LB90_probability_scale <- expit(LB90_logodds_scale)
UB90_probability_scale <- expit(UB90_logodds_scale)

dat_output <- data.frame(est_logodds_scale = est_logodds_scale,
                         LB90_logodds_scale = LB90_logodds_scale,
                         UB90_logodds_scale = UB90_logodds_scale,
                         est_probability_scale = est_probability_scale,
                         LB90_probability_scale = LB90_probability_scale,
                         UB90_probability_scale = UB90_probability_scale)

print(dat_output)

# > print(dat_output)
# est_logodds_scale LB90_logodds_scale UB90_logodds_scale est_probability_scale LB90_probability_scale UB90_probability_scale
# (Intercept)        -0.6451187         -0.8479034         -0.4423341             0.3440904              0.2998729              0.3911849

fit <- geeglm(substance_is_any_tobacco ~ 1, family = binomial, data = dat_primary_aim, id = participant_id, waves = decision_point)
Vmat <- vcov(fit)

est_logodds_scale <- fit$coefficients
std_err_logodds_scale <- sqrt(diag(Vmat))
LB90_logodds_scale <- est_logodds_scale - qnorm(0.95)*std_err_logodds_scale
UB90_logodds_scale <- est_logodds_scale + qnorm(0.95)*std_err_logodds_scale

est_probability_scale <- expit(fit$coefficients)
LB90_probability_scale <- expit(LB90_logodds_scale)
UB90_probability_scale <- expit(UB90_logodds_scale)

dat_output <- data.frame(est_logodds_scale = est_logodds_scale,
                         LB90_logodds_scale = LB90_logodds_scale,
                         UB90_logodds_scale = UB90_logodds_scale,
                         est_probability_scale = est_probability_scale,
                         LB90_probability_scale = LB90_probability_scale,
                         UB90_probability_scale = UB90_probability_scale)

print(dat_output)

# > print(dat_output)
# est_logodds_scale LB90_logodds_scale UB90_logodds_scale est_probability_scale LB90_probability_scale UB90_probability_scale
# (Intercept)        -0.6558608         -0.8583888         -0.4533329               0.34167              0.2976761              0.3885686





