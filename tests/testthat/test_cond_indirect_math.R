library(manymome)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)
lm2fit <- lm2ptable(fit)
boot_out <- lm2boot_out(fit, R = 100, seed = 418751)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

wv <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

# Moderated mediation
out1 <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv)
out2 <- cond_indirect(x = "x", y = "y",
                     fit = fit,
                     wvalues = wv)

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

out12 <- out1 + out2
out12boot <- out1_boot + out2_boot

test_that("math for indirect: moderated mediation", {
    expect_identical(out12$indirect, out1$indirect + out2$indirect)
    expect_identical(out12boot$indirect, out1_boot$indirect + out2_boot$indirect)
  })

# Mediation only

dat <- modmed_x1m3w4y1
lm_m1m <- lm(m1 ~ x, dat)
lm_m2m <- lm(m2 ~ m1 + x, dat)
lm_m3m <- lm(m3 ~ m2, dat)
lm_ym <- lm(y ~ m3 + x, dat)
fitm <- lm2list(lm_m1m, lm_m2m, lm_m3m, lm_ym)
lm2mfit <- lm2ptable(fitm)
boot_outm <- lm2boot_out(fitm, R = 100, seed = 418751)
boot_estm <- lapply(boot_outm, function(x) x$est)
boot_implied_statsm <- lapply(boot_outm, function(x) x$implied_stats)

outm <- cond_indirect(x = "x", y = "y",
                      m = c("m1", "m2", "m3"),
                      fit = fitm)
outm2 <- cond_indirect(x = "x", y = "y",
                      m = c("m2", "m3"),
                      fit = fitm)
outm3 <- cond_indirect(x = "x", y = "y",
                      fit = fitm)
outm4 <- cond_indirect(x = "x", y = "m3",
                      m = c("m1", "m2"),
                      fit = fitm)
outm5 <- cond_indirect(x = "m2", y = "y",
                      m = c("m3"),
                      fit = fitm)

# Mediation only with bootstrap CI, precomputed bootstrapping
outm_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)
outm2_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m2", "m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)
outm3_boot <- cond_indirect(x = "x", y = "y",
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)
outm4_boot <- cond_indirect(x = "x", y = "m3",
                      m = c("m1", "m2"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)
outm5_boot <- cond_indirect(x = "m2", y = "y",
                      m = c("m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)

outm1plus2 <- outm + outm2
outm1plus3 <- outm + outm3
outm1m2m3 <- outm1plus2 + outm3
outm1m2minusm3 <- outm1plus2 - outm3
outm1minus2 <- outm - outm2
outm1minus3 <- outm - outm3
out123pm <- outm + outm2 - outm3

outm1plus2boot <- outm_boot + outm2_boot
outm1plus3boot <- outm_boot + outm3_boot
outm1m2m3boot <- outm1plus2boot + outm3_boot
out1m1m2m3boot2 <- outm_boot + outm2_boot + outm3_boot
outm1minus2boot <- outm_boot - outm2_boot
outm1minus3boot <- outm_boot - outm3_boot

test_that("math for indirect: mediation", {
    expect_identical(outm1plus2$indirect, outm$indirect + outm2$indirect)
    expect_identical(outm1plus3$indirect, outm$indirect + outm3$indirect)
    expect_identical(outm1m2m3$indirect, outm$indirect + outm2$indirect + outm3$indirect)
    expect_identical(out123pm$indirect, outm$indirect + outm2$indirect - outm3$indirect)
    expect_error(outm + outm4)
    expect_error(outm + outm5)
    expect_identical(outm1plus2boot$indirect, outm_boot$indirect + outm2_boot$indirect)
    expect_identical(outm1plus3boot$indirect, outm_boot$indirect + outm3_boot$indirect)
    expect_identical(outm1m2m3boot$indirect, outm1plus2boot$indirect + outm3_boot$indirect)
    expect_identical(out1m1m2m3boot2$indirect, outm_boot$indirect + outm2_boot$indirect + outm3_boot$indirect)
    expect_identical(outm1minus2boot$indirect, outm_boot$indirect - outm2_boot$indirect)
    expect_identical(outm1minus3boot$indirect, outm_boot$indirect - outm3_boot$indirect)
    expect_error(outm_boot + outm_boot)
    expect_equal(coef(outm1minus2), outm1minus2$indirect, ignore_attr = TRUE)
    expect_equal(coef(outm1plus3), outm1plus3$indirect, ignore_attr = TRUE)
    expect_equal(coef(outm1minus2boot), outm1minus2boot$indirect, ignore_attr = TRUE)
    expect_equal(coef(outm1plus2boot), outm1plus2boot$indirect, ignore_attr = TRUE)
    expect_equal(confint(outm1minus2boot), outm1minus2boot$boot_ci, ignore_attr = TRUE)
    expect_equal(confint(outm1plus2boot), outm1plus2boot$boot_ci, ignore_attr = TRUE)
  })

wv1 <- list(wvalues = c(a = 0, b = 3, c = 5))
wv2 <- list(wvalues = c(b = 3, e = 6))
wv3 <- list(wvalues = c(x = 1, y = 5))
wv4 <- list(wvalues = NULL)

test_that("math for indirect: helpers", {
    expect_true(check_xy(outm, outm2))
    expect_true(check_xy(outm, outm3))
    expect_error(check_xy(outm, outm4))
    expect_error(check_xy(outm, outm5))
    expect_true(check_xy(outm_boot, outm2_boot))
    expect_true(check_xy(outm_boot, outm3_boot))
    expect_error(check_xy(outm_boot, outm4_boot))
    expect_error(check_xy(outm_boot, outm5_boot))
    expect_equal(join_wvalues(wv1, wv2), c(a = 0, b = 3, c = 5, e = 6))
    expect_equal(join_wvalues(wv1, wv3), c(a = 0, b = 3, c = 5, x = 1, y = 5))
    expect_equal(join_wvalues(wv1, wv4), c(a = 0, b = 3, c = 5))
    expect_equal(join_wvalues(wv2, wv3), c(b = 3, e = 6, x = 1, y = 5))
    expect_equal(join_wvalues(wv2, wv4), c(b = 3, e = 6))
    expect_equal(join_wvalues(wv3, wv4), c(x = 1, y = 5))
  })
