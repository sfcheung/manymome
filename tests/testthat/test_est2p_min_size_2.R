skip("WIP")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("Test pvalue_min_size", {

# cond_effects_math

dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)
lm2fit <- lm2ptable(fit)
boot_out <- lm2boot_out(fit, R = 40, seed = 418751, progress = FALSE)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

wv <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

# Moderated mediation with bootstrap CI, precomputed bootstrapping
out1_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out)
out2_boot <- cond_indirect(x = "x", y = "y",
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out)
out12boot <- out1_boot + out2_boot
expect_true(is.na(out12boot$boot_p))

out1b_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out,
                     internal_options = list(pvalue_min_size = 39))
out2b_boot <- cond_indirect(x = "x", y = "y",
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out,
                     internal_options = list(pvalue_min_size = 39))
out12bboot <- out1b_boot + out2b_boot
expect_true(!is.na(out12bboot$boot_p))

})
