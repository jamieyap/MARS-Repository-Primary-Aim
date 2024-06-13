rm(list = ls())

library(ggplot2)

cc_est_prob_prompt <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_prompt.csv"))
cc_est_prob_no_prompt <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_no_prompt.csv"))
cc_est_prob_high_effort_prompt <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_high_effort_prompt.csv"))
cc_est_prob_low_effort_prompt <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_low_effort_prompt.csv"))

dat_all_pool_stats_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_prompt.csv"))
dat_all_pool_stats_no_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt.csv"))
dat_all_pool_stats_high_effort_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt.csv"))
dat_all_pool_stats_low_effort_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt.csv"))

dat_ppc_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_prompt.csv"))
dat_ppc_no_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt.csv"))
dat_ppc_high_effort_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt.csv"))
dat_ppc_low_effort_prompt <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt.csv"))

group_colors <- c(CC = "black", MI = "blue")

###############################################################################
# Mean among eligible decision points micro-randomized to prompt (any type)
###############################################################################
cc_results <- data.frame(where_from = "CC", est = cc_est_prob_prompt$est, lb = cc_est_prob_prompt$conf_int_lb, ub = cc_est_prob_prompt$conf_int_ub)
mi_results <- data.frame(where_from = "MI", est = dat_all_pool_stats_prompt$est_prob, lb = dat_all_pool_stats_prompt$conf_int_lb, ub = dat_all_pool_stats_prompt$conf_int_ub)

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 2) +
  scale_y_continuous(name = "Estimated proportion", limits = c(0,1), breaks = seq(0,1,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=est), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("Prompt")

ggsave(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_prompt.png"), width = 5, height = 8, units = "in", dpi = 1000)

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
cc_results <- data.frame(where_from = "CC", est = cc_est_prob_no_prompt$est, lb = cc_est_prob_no_prompt$conf_int_lb, ub = cc_est_prob_no_prompt$conf_int_ub)
mi_results <- data.frame(where_from = "MI", est = dat_all_pool_stats_no_prompt$est_prob, lb = dat_all_pool_stats_no_prompt$conf_int_lb, ub = dat_all_pool_stats_no_prompt$conf_int_ub)

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 2) +
  scale_y_continuous(name = "Estimated proportion", limits = c(0,1), breaks = seq(0,1,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=est), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("No Prompt")

ggsave(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_no_prompt.png"), width = 5, height = 8, units = "in", dpi = 1000)


###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
cc_results <- data.frame(where_from = "CC", est = cc_est_prob_high_effort_prompt$est, lb = cc_est_prob_high_effort_prompt$conf_int_lb, ub = cc_est_prob_high_effort_prompt$conf_int_ub)
mi_results <- data.frame(where_from = "MI", est = dat_all_pool_stats_high_effort_prompt$est_prob, lb = dat_all_pool_stats_high_effort_prompt$conf_int_lb, ub = dat_all_pool_stats_high_effort_prompt$conf_int_ub)

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 2) +
  scale_y_continuous(name = "Estimated proportion", limits = c(0,1), breaks = seq(0,1,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=est), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("High Effort Prompt")

ggsave(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_high_effort_prompt.png"), width = 5, height = 8, units = "in", dpi = 1000)


###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
cc_results <- data.frame(where_from = "CC", est = cc_est_prob_low_effort_prompt$est, lb = cc_est_prob_low_effort_prompt$conf_int_lb, ub = cc_est_prob_low_effort_prompt$conf_int_ub)
mi_results <- data.frame(where_from = "MI", est = dat_all_pool_stats_low_effort_prompt$est_prob, lb = dat_all_pool_stats_low_effort_prompt$conf_int_lb, ub = dat_all_pool_stats_low_effort_prompt$conf_int_ub)

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 2) +
  scale_y_continuous(name = "Estimated proportion", limits = c(0,1), breaks = seq(0,1,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=est), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("Low Effort Prompt")

ggsave(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_low_effort_prompt.png"), width = 5, height = 8, units = "in", dpi = 1000)

