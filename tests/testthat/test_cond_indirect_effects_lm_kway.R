# To be examined in an interactive session

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
fit <- lm2list(lm_m, lm_y)
lm2fit <- lm2ptable(fit)

boot_out <- lm2boot_out(fit, R = 100, seed = 418751)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

# Moderated mediation

out_mm_1 <- mod_levels_list("w1", "w2", fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit)
out_2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_x = TRUE)
out_3 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_y = TRUE)
out_4 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE)

out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               boot_ci = TRUE, seed = 87415)
fit_boot_out <- lm2boot_out(fit, R = 100, seed = 87415)
out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_7 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_8 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

tmp <- capture.output(print(out_1))
tmp <- capture.output(print(out_2))
tmp <- capture.output(print(out_3))
tmp <- capture.output(print(out_4))
tmp <- capture.output(print(out_5))
tmp <- capture.output(print(out_6))
tmp <- capture.output(print(out_7))
tmp <- capture.output(print(out_8))

