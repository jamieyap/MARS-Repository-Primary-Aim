rm(list = ls())

cc_est_prob_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_prompt_by_dp.csv"))
cc_est_prob_no_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_no_prompt_by_dp.csv"))
cc_est_prob_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_high_effort_prompt_by_dp.csv"))
cc_est_prob_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "cc_est_prob_low_effort_prompt_by_dp.csv"))

dat_all_pool_stats_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_prompt_by_dp.csv"))
dat_all_pool_stats_no_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_no_prompt_by_dp.csv"))
dat_all_pool_stats_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_high_effort_prompt_by_dp.csv"))
dat_all_pool_stats_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_all_pool_stats_low_effort_prompt_by_dp.csv"))

dat_ppc_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_prompt_by_dp.csv"))
dat_ppc_no_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_no_prompt_by_dp.csv"))
dat_ppc_high_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_high_effort_prompt_by_dp.csv"))
dat_ppc_low_effort_prompt_by_dp <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "dat_ppc_low_effort_prompt_by_dp.csv"))

###############################################################################
# Mean among eligible decision points micro-randomized to prompt (any type)
###############################################################################
png(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(cc_est_prob_prompt_by_dp$decision_point, cc_est_prob_prompt_by_dp$est, type = "b", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Estimated Proportion")
lines(dat_all_pool_stats_prompt_by_dp$decision_point, dat_all_pool_stats_prompt_by_dp$est_prob, type = "b", ylim = c(0,1), lwd = 3, col = "blue")

lines(cc_est_prob_prompt_by_dp$decision_point, cc_est_prob_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5)
lines(cc_est_prob_prompt_by_dp$decision_point, cc_est_prob_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5)
lines(dat_all_pool_stats_prompt_by_dp$decision_point, dat_all_pool_stats_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5, col = "blue")
lines(dat_all_pool_stats_prompt_by_dp$decision_point, dat_all_pool_stats_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5, col = "blue")

legend("bottomright", legend = c("complete case", "MI"), col = c("black", "blue"), lty = c(1,1), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to prompt (any type)")
dev.off()

png(filename = file.path("analysis-multiple-imputation", "formatted-output", "ppc_est_prob_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(dat_ppc_prompt_by_dp$decision_point, dat_ppc_prompt_by_dp$ppc_est, type = "b", col = "red", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Proportion of times estimates obtained from replicate was greater or equal to imputed")
abline(h = 0.5, lwd = 3, lty = 2)
abline(h = 0.95, lwd = 3, lty = 2)
abline(h = 0.05, lwd = 3, lty = 2)
legend("bottomright", legend = c("PPC for estimate", "Displays 0.05, 0.50, 0.95"), col = c("red", "black"), lty = c(1,2), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to prompt (any type)")
dev.off()

###############################################################################
# Mean among eligible decision points micro-randomized to no prompt
###############################################################################
png(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_no_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(cc_est_prob_no_prompt_by_dp$decision_point, cc_est_prob_no_prompt_by_dp$est, type = "b", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Estimated Proportion")
lines(dat_all_pool_stats_no_prompt_by_dp$decision_point, dat_all_pool_stats_no_prompt_by_dp$est_prob, type = "b", ylim = c(0,1), lwd = 3, col = "blue")

lines(cc_est_prob_no_prompt_by_dp$decision_point, cc_est_prob_no_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5)
lines(cc_est_prob_no_prompt_by_dp$decision_point, cc_est_prob_no_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5)
lines(dat_all_pool_stats_no_prompt_by_dp$decision_point, dat_all_pool_stats_no_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5, col = "blue")
lines(dat_all_pool_stats_no_prompt_by_dp$decision_point, dat_all_pool_stats_no_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5, col = "blue")

legend("bottomright", legend = c("complete case", "MI"), col = c("black", "blue"), lty = c(1,1), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to no prompt")
dev.off()

png(filename = file.path("analysis-multiple-imputation", "formatted-output", "ppc_est_prob_no_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(dat_ppc_no_prompt_by_dp$decision_point, dat_ppc_no_prompt_by_dp$ppc_est, type = "b", col = "red", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Proportion of times estimates obtained from replicate was greater or equal to imputed")
abline(h = 0.5, lwd = 3, lty = 2)
abline(h = 0.95, lwd = 3, lty = 2)
abline(h = 0.05, lwd = 3, lty = 2)
legend("bottomright", legend = c("PPC for estimate", "Displays 0.05, 0.50, 0.95"), col = c("red", "black"), lty = c(1,2), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to no prompt")
dev.off()

###############################################################################
# Mean among eligible decision points micro-randomized to high effort prompt
###############################################################################
png(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_high_effort_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(cc_est_prob_high_effort_prompt_by_dp$decision_point, cc_est_prob_high_effort_prompt_by_dp$est, type = "b", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Estimated Proportion")
lines(dat_all_pool_stats_high_effort_prompt_by_dp$decision_point, dat_all_pool_stats_high_effort_prompt_by_dp$est_prob, type = "b", ylim = c(0,1), lwd = 3, col = "blue")

lines(cc_est_prob_high_effort_prompt_by_dp$decision_point, cc_est_prob_high_effort_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5)
lines(cc_est_prob_high_effort_prompt_by_dp$decision_point, cc_est_prob_high_effort_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5)
lines(dat_all_pool_stats_high_effort_prompt_by_dp$decision_point, dat_all_pool_stats_high_effort_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5, col = "blue")
lines(dat_all_pool_stats_high_effort_prompt_by_dp$decision_point, dat_all_pool_stats_high_effort_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5, col = "blue")

legend("bottomright", legend = c("complete case", "MI"), col = c("black", "blue"), lty = c(1,1), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to high effort prompt")
dev.off()

png(filename = file.path("analysis-multiple-imputation", "formatted-output", "ppc_est_prob_high_effort_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(dat_ppc_high_effort_prompt_by_dp$decision_point, dat_ppc_high_effort_prompt_by_dp$ppc_est, type = "b", col = "red", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Proportion of times estimates obtained from replicate was greater or equal to imputed")
abline(h = 0.5, lwd = 3, lty = 2)
abline(h = 0.95, lwd = 3, lty = 2)
abline(h = 0.05, lwd = 3, lty = 2)
legend("bottomright", legend = c("PPC for estimate", "Displays 0.05, 0.50, 0.95"), col = c("red", "black"), lty = c(1,2), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to high effort prompt")
dev.off()

###############################################################################
# Mean among eligible decision points micro-randomized to low effort prompt
###############################################################################
png(filename = file.path("analysis-multiple-imputation", "formatted-output", "est_prob_low_effort_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(cc_est_prob_low_effort_prompt_by_dp$decision_point, cc_est_prob_low_effort_prompt_by_dp$est, type = "b", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Estimated Proportion")
lines(dat_all_pool_stats_low_effort_prompt_by_dp$decision_point, dat_all_pool_stats_low_effort_prompt_by_dp$est_prob, type = "b", ylim = c(0,1), lwd = 3, col = "blue")

lines(cc_est_prob_low_effort_prompt_by_dp$decision_point, cc_est_prob_low_effort_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5)
lines(cc_est_prob_low_effort_prompt_by_dp$decision_point, cc_est_prob_low_effort_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5)
lines(dat_all_pool_stats_low_effort_prompt_by_dp$decision_point, dat_all_pool_stats_low_effort_prompt_by_dp$conf_int_lb, type = "b", lty = 2, lwd = 0.5, col = "blue")
lines(dat_all_pool_stats_low_effort_prompt_by_dp$decision_point, dat_all_pool_stats_low_effort_prompt_by_dp$conf_int_ub, type = "b", lty = 2, lwd = 0.5, col = "blue")

legend("bottomright", legend = c("complete case", "MI"), col = c("black", "blue"), lty = c(1,1), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to low effort prompt")
dev.off()

png(filename = file.path("analysis-multiple-imputation", "formatted-output", "ppc_est_prob_low_effort_prompt_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(dat_ppc_low_effort_prompt_by_dp$decision_point, dat_ppc_low_effort_prompt_by_dp$ppc_est, type = "b", col = "red", ylim = c(0,1), lwd = 3, xlab = "Decision Point", ylab = "Proportion of times estimates obtained from replicate was greater or equal to imputed")
abline(h = 0.5, lwd = 3, lty = 2)
abline(h = 0.95, lwd = 3, lty = 2)
abline(h = 0.05, lwd = 3, lty = 2)
legend("bottomright", legend = c("PPC for estimate", "Displays 0.05, 0.50, 0.95"), col = c("red", "black"), lty = c(1,2), lwd = c(3,3))
title(main = "Mean among eligible decision points micro-randomized to low effort prompt")
dev.off()

###############################################################################
# Fraction of missing information
###############################################################################
png(filename = file.path("analysis-multiple-imputation", "formatted-output", "fmi_est_prob_by_dp.png"), width = 10, height = 10, units = "in", res = 1000)
plot(dat_all_pool_stats_prompt_by_dp$decision_point, dat_all_pool_stats_prompt_by_dp$gamma, type = "b", ylim = c(0,1), lwd = 3, col = "cornflowerblue", xlab = "Decision Point", ylab = "Fraction of Missing Information")
lines(dat_all_pool_stats_no_prompt_by_dp$decision_point, dat_all_pool_stats_no_prompt_by_dp$gamma, type = "b", ylim = c(0,1), lwd = 3, col = "darkgreen")
lines(dat_all_pool_stats_high_effort_prompt_by_dp$decision_point, dat_all_pool_stats_high_effort_prompt_by_dp$gamma, type = "b", ylim = c(0,1), lwd = 3, col = "violet")
lines(dat_all_pool_stats_low_effort_prompt_by_dp$decision_point[-c(1:2)], dat_all_pool_stats_low_effort_prompt_by_dp$gamma[-c(1:2)], type = "b", ylim = c(0,1), lwd = 3, col = "darkgray")
legend("bottomleft", cex = 0.8, legend = c("Prompt (any)", "No Prompt","High Effort Prompt","Low Effort Prompt"), col = c("cornflowerblue", "darkgreen", "violet", "darkgray"), lty = c(1,1,1,1), lwd = c(3,3,3,3))
dev.off()


