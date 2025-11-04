library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("skip_ci", {

# Mediation only

dat <- modmed_x1m3w4y1
lm_m1m <- lm(m1 ~ x, dat)
lm_m2m <- lm(m2 ~ m1, dat)
lm_m3m <- lm(m3 ~ m2, dat)
lm_ym <- lm(y ~ m3 + x, dat)
fitm <- lm2list(lm_m1m, lm_m2m, lm_m3m, lm_ym)
lm2mfit <- lm2ptable(fitm)
boot_outm <- lm2boot_out(fitm, R = 40, seed = 418751,
                         progress = FALSE)
boot_estm <- lapply(boot_outm, function(x) x$est)

# Mediation only with bootstrap CI, precomputed bootstrapping

outm_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)
expect_false(all(is.na(outm_boot$boot_ci)))

outm_boot2 <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm,
                     internal_options = list(skip_ci = TRUE))
expect_true(all(is.na(outm_boot2$boot_ci)))

# Moderated mediation

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
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

fit_boot_out <- lm2boot_out(fit, R = 40, seed = 87415, progress = FALSE)
out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)
out_6_full <- attr(out_6, "full_output")
tmp <- sapply(out_6_full, function(x) x$boot_ci)
expect_false(all(is.na(tmp)))

out_6b <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out,
                               internal_options = list(skip_ci = TRUE))
out_6b_full <- attr(out_6b, "full_output")
tmp <- sapply(out_6b_full, function(x) x$boot_ci)
expect_true(all(is.na(tmp)))

diff_6 <- cond_indirect_diff(out_6, from = 1, to = 2)
expect_false(all(is.na(diff_6$ci)))

diff_6b <- cond_indirect_diff(out_6b, from = 1, to = 2)
expect_true(all(is.na(diff_6b$ci)))

})
