rm(list = ls())

source("paths.R")

cc_linear_day <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "H1", "log_risk_ratio_scale_H1_linear_day.csv"))
cc_linear_hour <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "H1", "log_risk_ratio_scale_H1_linear_hour.csv"))
cc_quadratic_day <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "H1", "log_risk_ratio_scale_H1_quadratic_day.csv"))
cc_quadratic_hour <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "H1", "log_risk_ratio_scale_H1_quadratic_hour.csv"))

mi_linear_day <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal_study_day_linear.csv"))
mi_linear_hour <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal_hour_linear.csv"))
mi_quadratic_day <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal_study_day_quadratic.csv"))
mi_quadratic_hour <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal_hour_quadratic.csv"))

###############################################################################
# Workflow: Linear time trend - day in study
###############################################################################

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_study_day_linear_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1, 2))

plot(1:8, cc_linear_day[3:10,]$Estimate, type = "l", ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(1:8, cc_linear_day[3:10,]$X95..LCL, type = "l", lty = 2, lwd = 2)
lines(1:8, cc_linear_day[3:10,]$X95..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, mi_linear_day[3:10,]$Estimate, type = "l", col = "blue", lwd = 2, ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale")
lines(1:8, mi_linear_day[3:10,]$LCL, type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, mi_linear_day[3:10,]$UCL, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_study_day_linear_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1, 2))

plot(1:8, cc_linear_day[3:10,]$Estimate, type = "l", ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(1:8, cc_linear_day[3:10,]$X90..LCL, type = "l", lty = 2, lwd = 2)
lines(1:8, cc_linear_day[3:10,]$X90..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, mi_linear_day[3:10,]$Estimate, type = "l", col = "blue", lwd = 2, ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale")
lines(1:8, mi_linear_day[3:10,]$LCL90, type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, mi_linear_day[3:10,]$UCL90, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_study_day_linear_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1, 2))

plot(1:8, exp(cc_linear_day[3:10,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(1:8, exp(cc_linear_day[3:10,]$X95..LCL), type = "l", lty = 2, lwd = 2)
lines(1:8, exp(cc_linear_day[3:10,]$X95..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, exp(mi_linear_day[3:10,]$Estimate), type = "l", col = "blue", lwd = 2, ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale")
lines(1:8, exp(mi_linear_day[3:10,]$LCL), type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, exp(mi_linear_day[3:10,]$UCL), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_study_day_linear_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1, 2))

plot(1:8, exp(cc_linear_day[3:10,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(1:8, exp(cc_linear_day[3:10,]$X90..LCL), type = "l", lty = 2, lwd = 2)
lines(1:8, exp(cc_linear_day[3:10,]$X90..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, exp(mi_linear_day[3:10,]$Estimate), type = "l", col = "blue", lwd = 2, ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale")
lines(1:8, exp(mi_linear_day[3:10,]$LCL90), type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, exp(mi_linear_day[3:10,]$UCL90), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

###############################################################################
# Workflow: Quadratic time trend - day in study
###############################################################################

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_study_day_quadratic_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(1:8, cc_quadratic_day[4:11,]$Estimate, type = "l", ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(1:8, cc_quadratic_day[4:11,]$X95..LCL, type = "l", lty = 2, lwd = 2)
lines(1:8, cc_quadratic_day[4:11,]$X95..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, mi_quadratic_day[4:11,]$Estimate, type = "l", ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale", col = "blue", lwd = 2)
lines(1:8, mi_quadratic_day[4:11,]$LCL, type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, mi_quadratic_day[4:11,]$UCL, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_study_day_quadratic_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(1:8, cc_quadratic_day[4:11,]$Estimate, type = "l", ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(1:8, cc_quadratic_day[4:11,]$X90..LCL, type = "l", lty = 2, lwd = 2)
lines(1:8, cc_quadratic_day[4:11,]$X90..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "90% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, mi_quadratic_day[4:11,]$Estimate, type = "l", ylim = c(-0.10, 0.60), xlab = "Study Day", ylab = "Treatment Effect on the Log-Scale", col = "blue", lwd = 2)
lines(1:8, mi_quadratic_day[4:11,]$LCL90, type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, mi_quadratic_day[4:11,]$UCL90, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "90% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_study_day_quadratic_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(1:8, exp(cc_quadratic_day[4:11,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(1:8, exp(cc_quadratic_day[4:11,]$X95..LCL), type = "l", lty = 2, lwd = 2)
lines(1:8, exp(cc_quadratic_day[4:11,]$X95..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, exp(mi_quadratic_day[4:11,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale", col = "blue", lwd = 2)
lines(1:8, exp(mi_quadratic_day[4:11,]$LCL), type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, exp(mi_quadratic_day[4:11,]$UCL), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_study_day_quadratic_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(1:8, exp(cc_quadratic_day[4:11,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(1:8, exp(cc_quadratic_day[4:11,]$X90..LCL), type = "l", lty = 2, lwd = 2)
lines(1:8, exp(cc_quadratic_day[4:11,]$X90..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "90% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(1:8, exp(mi_quadratic_day[4:11,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Study Day", ylab = "Treatment Effect on the Risk Ratio-Scale", col = "blue", lwd = 2)
lines(1:8, exp(mi_quadratic_day[4:11,]$LCL90), type = "l", col = "blue", lty = 2, lwd = 2)
lines(1:8, exp(mi_quadratic_day[4:11,]$UCL90), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "90% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

###############################################################################
# Workflow: Linear time trend - hour of day
###############################################################################

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_hour_linear_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, cc_linear_hour[3:27,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(0:24, cc_linear_hour[3:27,]$X95..LCL, type = "l", lty = 2, lwd = 2)
lines(0:24, cc_linear_hour[3:27,]$X95..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, mi_linear_hour[3:27,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", col = "blue", lwd = 2)
lines(0:24, mi_linear_hour[3:27,]$LCL, type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, mi_linear_hour[3:27,]$UCL, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_hour_linear_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, cc_linear_hour[3:27,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(0:24, cc_linear_hour[3:27,]$X90..LCL, type = "l", lty = 2, lwd = 2)
lines(0:24, cc_linear_hour[3:27,]$X90..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, mi_linear_hour[3:27,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", col = "blue", lwd = 2)
lines(0:24, mi_linear_hour[3:27,]$LCL90, type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, mi_linear_hour[3:27,]$UCL90, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "90% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_hour_linear_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, exp(cc_linear_hour[3:27,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(0:24, exp(cc_linear_hour[3:27,]$X95..LCL), type = "l", lty = 2, lwd = 2)
lines(0:24, exp(cc_linear_hour[3:27,]$X95..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, exp(mi_linear_hour[3:27,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", col = "blue", lwd = 2)
lines(0:24, exp(mi_linear_hour[3:27,]$LCL), type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, exp(mi_linear_hour[3:27,]$UCL), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_hour_linear_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, exp(cc_linear_hour[3:27,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(0:24, exp(cc_linear_hour[3:27,]$X90..LCL), type = "l", lty = 2, lwd = 2)
lines(0:24, exp(cc_linear_hour[3:27,]$X90..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "90% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, exp(mi_linear_hour[3:27,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", col = "blue", lwd = 2)
lines(0:24, exp(mi_linear_hour[3:27,]$LCL90), type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, exp(mi_linear_hour[3:27,]$UCL90), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "90% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

###############################################################################
# Workflow: Quadratic time trend - hour of day
###############################################################################

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_hour_quadratic_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, cc_quadratic_hour[4:28,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(0:24, cc_quadratic_hour[4:28,]$X95..LCL, type = "l", lty = 2, lwd = 2)
lines(0:24, cc_quadratic_hour[4:28,]$X95..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, mi_quadratic_hour[4:28,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", col = "blue", lwd = 2)
lines(0:24, mi_quadratic_hour[4:28,]$LCL, type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, mi_quadratic_hour[4:28,]$UCL, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "log_risk_ratio_scale_primary_hour_quadratic_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, cc_quadratic_hour[4:28,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", lwd = 2)
lines(0:24, cc_quadratic_hour[4:28,]$X90..LCL, type = "l", lty = 2, lwd = 2)
lines(0:24, cc_quadratic_hour[4:28,]$X90..UCL, type = "l", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "90% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, mi_quadratic_hour[4:28,]$Estimate, type = "l", ylim = c(-0.2, 0.8), xlab = "Hour of day", ylab = "Treatment Effect on the Log-Scale", col = "blue", lwd = 2)
lines(0:24, mi_quadratic_hour[4:28,]$LCL90, type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, mi_quadratic_hour[4:28,]$UCL90, type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 0, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "90% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_hour_quadratic_CI95.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, exp(cc_quadratic_hour[4:28,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(0:24, exp(cc_quadratic_hour[4:28,]$X95..LCL), type = "l", lty = 2, lwd = 2)
lines(0:24, exp(cc_quadratic_hour[4:28,]$X95..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "95% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, exp(mi_quadratic_hour[4:28,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", col = "blue", lwd = 2)
lines(0:24, exp(mi_quadratic_hour[4:28,]$LCL), type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, exp(mi_quadratic_hour[4:28,]$UCL), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "95% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

png(file = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_hour_quadratic_CI90.png"), width = 10, height = 10, units = "in", res = 600)

par(mfrow = c(1,2))

plot(0:24, exp(cc_quadratic_hour[4:28,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", lwd = 2)
lines(0:24, exp(cc_quadratic_hour[4:28,]$X90..LCL), type = "l", lty = 2, lwd = 2)
lines(0:24, exp(cc_quadratic_hour[4:28,]$X90..UCL), type = "l", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: CC", "90% CI: CC"), col = c("black", "black"), lty = c(1,2), lwd = c(2,2))

plot(0:24, exp(mi_quadratic_hour[4:28,]$Estimate), type = "l", ylim = c(0.7, 2), xlab = "Hour of day", ylab = "Treatment Effect on the Risk Ratio-Scale", col = "blue", lwd = 2)
lines(0:24, exp(mi_quadratic_hour[4:28,]$LCL90), type = "l", col = "blue", lty = 2, lwd = 2)
lines(0:24, exp(mi_quadratic_hour[4:28,]$UCL90), type = "l", col = "blue", lty = 2, lwd = 2)
abline(h = 1, lty = 3, col = "red", lwd = 2)
legend("topright", legend = c("estimates: MI", "90% CI: MI"), col = c("blue", "blue"), lty = c(1,2), lwd = c(2,2))

dev.off()

###############################################################################
# Workflow: Marginal Means
###############################################################################

rm(list = ls())

source("paths.R")

library(ggplot2)

cc_marginal_risk_ratio <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "H1", "risk_ratio_scale_H1.csv"))
cc_marginal_risk_difference <- read.csv(file = file.path("analysis-complete-case", "formatted-output", "H1", "risk_difference_scale_H1.csv"))

mi_marginal_log_risk_ratio <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal.csv"))
mi_marginal_risk_difference <- read.csv(file = file.path("analysis-multiple-imputation", "formatted-output", "pooled_H1_causal_risk_difference_scale.csv"))

group_colors <- c(CC = "black", MI = "blue")

cc_results <- data.frame(where_from = "CC", est = cc_marginal_risk_ratio$exp_estimates, lb = cc_marginal_risk_ratio$rrLB95, ub = cc_marginal_risk_ratio$rrUB95)
mi_results <- data.frame(where_from = "MI", est = exp(mi_marginal_log_risk_ratio$Estimate), lb = exp(mi_marginal_log_risk_ratio$LCL), ub = exp(mi_marginal_log_risk_ratio$UCL))

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 2) +
  scale_y_continuous(name = "Estimated Risk Ratio", limits = c(0.5,1.5), breaks = seq(0.5,1.5,0.10)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=round(est,3)), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("Prompt vs. No Prompt") +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 2, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "risk_ratio_scale_primary_marginal_CI95.png"), width = 5, height = 8, units = "in", dpi = 1000)

cc_results <- data.frame(where_from = "CC", est = cc_marginal_risk_difference$Estimate[1], lb = cc_marginal_risk_difference$X95..LCL[1], ub = cc_marginal_risk_difference$X95..UCL[1])
mi_results <- data.frame(where_from = "MI", est = mi_marginal_risk_difference$Estimate[1], lb = mi_marginal_risk_difference$LCL[1], ub = mi_marginal_risk_difference$UCL[1])

all_results <- rbind(cc_results, mi_results)

ggplot(all_results, aes(x = where_from, y = est, color = where_from)) +
  geom_point(size = 10) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, linewidth = 2) +
  scale_y_continuous(name = "Estimated Risk Difference", limits = c(-0.5,0.5), breaks = seq(-0.5,0.5,0.1)) +
  scale_x_discrete(name = "") + 
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "none") +
  geom_text(aes(label=round(est,3)), hjust = -0.4, size=8) +
  scale_color_manual(values=group_colors) +
  ggtitle("Prompt vs. No Prompt") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 2, colour = "red")

ggsave(filename = file.path("plot-cc-and-mi-together", "risk_difference_scale_primary_marginal_CI95.png"), width = 5, height = 8, units = "in", dpi = 1000)

