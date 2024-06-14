rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages
###############################################################################
source("paths.R")
library(geepack)
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

# This is the data frame that is the output of the very last step of our data pipeline
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 
dat_primary_aim <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

use_alpha <- 0.05/2

###############################################################################
# Mean among eligible decision points micro-randomized to prompt (any type)
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(coinflip == 1)
dat_count_total <- dat %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum(!is.na(Y)))

fit <- geeglm(Y ~ 1, data = dat, id = participant_id, family = binomial)

current_fit <- fit
a <- use_alpha

# Grab results on log-odds scale
results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
est_logodds_scale <- results_logodds_scale[["Estimate"]]
var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
stderr_logodds_scale <- sqrt(var_logodds_scale)
logodds_conf_int_lb <- est_logodds_scale - qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
logodds_conf_int_ub <- est_logodds_scale + qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
# Transform results to probability scale
est_prob <- exp(est_logodds_scale)/(1 + exp(est_logodds_scale))
conf_int_lb <- exp(logodds_conf_int_lb)/(1 + exp(logodds_conf_int_lb))
conf_int_ub <- exp(logodds_conf_int_ub)/(1 + exp(logodds_conf_int_ub))
results_mu_scale <- tibble(est = est_prob, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)

dat_estimates_mu_scale <- results_mu_scale
dat_estimates_mu_scale <- cross_join(x = dat_count_total, y = dat_estimates_mu_scale)
dat_estimates_mu_scale <- dat_estimates_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_prompt.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(coinflip == 0)
dat_count_total <- dat %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum(!is.na(Y)))

fit <- geeglm(Y ~ 1, data = dat, id = participant_id, family = binomial)

current_fit <- fit
a <- use_alpha

# Grab results on log-odds scale
results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
est_logodds_scale <- results_logodds_scale[["Estimate"]]
var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
stderr_logodds_scale <- sqrt(var_logodds_scale)
logodds_conf_int_lb <- est_logodds_scale - qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
logodds_conf_int_ub <- est_logodds_scale + qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
# Transform results to probability scale
est_prob <- exp(est_logodds_scale)/(1 + exp(est_logodds_scale))
conf_int_lb <- exp(logodds_conf_int_lb)/(1 + exp(logodds_conf_int_lb))
conf_int_ub <- exp(logodds_conf_int_ub)/(1 + exp(logodds_conf_int_ub))
results_mu_scale <- tibble(est = est_prob, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)

dat_estimates_mu_scale <- results_mu_scale
dat_estimates_mu_scale <- cross_join(x = dat_count_total, y = dat_estimates_mu_scale)
dat_estimates_mu_scale <- dat_estimates_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_no_prompt.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(is_high_effort == 1)
dat_count_total <- dat %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum(!is.na(Y)))

fit <- geeglm(Y ~ 1, data = dat, id = participant_id, family = binomial)

current_fit <- fit
a <- use_alpha

# Grab results on log-odds scale
results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
est_logodds_scale <- results_logodds_scale[["Estimate"]]
var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
stderr_logodds_scale <- sqrt(var_logodds_scale)
logodds_conf_int_lb <- est_logodds_scale - qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
logodds_conf_int_ub <- est_logodds_scale + qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
# Transform results to probability scale
est_prob <- exp(est_logodds_scale)/(1 + exp(est_logodds_scale))
conf_int_lb <- exp(logodds_conf_int_lb)/(1 + exp(logodds_conf_int_lb))
conf_int_ub <- exp(logodds_conf_int_ub)/(1 + exp(logodds_conf_int_ub))
results_mu_scale <- tibble(est = est_prob, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)

dat_estimates_mu_scale <- results_mu_scale
dat_estimates_mu_scale <- cross_join(x = dat_count_total, y = dat_estimates_mu_scale)
dat_estimates_mu_scale <- dat_estimates_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_high_effort_prompt.csv"), row.names = FALSE)

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
dat <- dat_primary_aim %>% filter(eligibility == 1) %>% filter(is_low_effort == 1)
dat_count_total <- dat %>% 
  summarise(n_total = n(),
            n_observed_among_total = sum(!is.na(Y)))

fit <- geeglm(Y ~ 1, data = dat, id = participant_id, family = binomial)

current_fit <- fit
a <- use_alpha

# Grab results on log-odds scale
results_logodds_scale <- current_fit %>% summary(.) %>% .[["coefficients"]] %>% as_tibble(.)
est_logodds_scale <- results_logodds_scale[["Estimate"]]
var_logodds_scale <- current_fit %>% vcov(.) %>% c(.)
stderr_logodds_scale <- sqrt(var_logodds_scale)
logodds_conf_int_lb <- est_logodds_scale - qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
logodds_conf_int_ub <- est_logodds_scale + qnorm(a, lower.tail = FALSE) * stderr_logodds_scale
# Transform results to probability scale
est_prob <- exp(est_logodds_scale)/(1 + exp(est_logodds_scale))
conf_int_lb <- exp(logodds_conf_int_lb)/(1 + exp(logodds_conf_int_lb))
conf_int_ub <- exp(logodds_conf_int_ub)/(1 + exp(logodds_conf_int_ub))
results_mu_scale <- tibble(est = est_prob, conf_int_lb = conf_int_lb, conf_int_ub = conf_int_ub)

dat_estimates_mu_scale <- results_mu_scale
dat_estimates_mu_scale <- cross_join(x = dat_count_total, y = dat_estimates_mu_scale)
dat_estimates_mu_scale <- dat_estimates_mu_scale %>% round(., digits = 3)

write.csv(dat_estimates_mu_scale, file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_low_effort_prompt.csv"), row.names = FALSE)

