# To be examined in an interactive session

library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 6:7]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)
fit_m3 <- lm2list(lm_m3)
fit_y <- lm2list(lm_y)

# Moderated: Categorical

out_mm_m3 <- mod_levels_list(c("gpgp2", "gpgp3"), fit = fit_m3, merge = TRUE)

out_m3_1 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3)
out_m3_2 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               standardized_x = TRUE)
out_m3_3 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               standardized_y = TRUE)
out_m3_4 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               standardized_x = TRUE, standardized_y = TRUE)

out_m3_5 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               boot_ci = TRUE, seed = 87415,
                               parallel = FALSE, progress = FALSE,
                               R = 40)
fit_boot_out <- lm2boot_out(fit_m3, R = 40, seed = 87415, progress = FALSE)
out_m3_6 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_m3_7 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_m3_8 <- cond_indirect_effects(wlevels = out_mm_m3, x = "x", y = "m3", fit = fit_m3,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

tmp <- capture.output(print(out_m3_1))
tmp <- capture.output(print(out_m3_2))
tmp <- capture.output(print(out_m3_3))
tmp <- capture.output(print(out_m3_4))
tmp <- capture.output(print(out_m3_5))
tmp <- capture.output(print(out_m3_6))
tmp <- capture.output(print(out_m3_7))
tmp <- capture.output(print(out_m3_8))

# Moderation: Categorical

out_mm_y <- mod_levels_list("w4", fit = fit_y, merge = TRUE)

out_y_1 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y)
out_y_2 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               standardized_x = TRUE)
out_y_3 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               standardized_y = TRUE)
out_y_4 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               standardized_x = TRUE, standardized_y = TRUE)

out_y_5 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               boot_ci = TRUE, seed = 87415,
                               parallel = FALSE, progress = FALSE,
                               R = 40)
fit_boot_out_y <- lm2boot_out(fit_y, R = 40, seed = 87415, progress = FALSE)
out_y_6 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out_y)
out_y_7 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out_y)
out_y_8 <- cond_indirect_effects(wlevels = out_mm_y, x = "x", y = "y", fit = fit_y,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out_y)

tmp <- capture.output(print(out_y_1))
tmp <- capture.output(print(out_y_2))
tmp <- capture.output(print(out_y_3))
tmp <- capture.output(print(out_y_4))
tmp <- capture.output(print(out_y_5))
tmp <- capture.output(print(out_y_6))
tmp <- capture.output(print(out_y_7))
tmp <- capture.output(print(out_y_8))

