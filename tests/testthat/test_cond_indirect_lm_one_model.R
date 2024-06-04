library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
fit <- lm2list(lm_m1)
lm2fit <- lm2ptable(fit)
boot_out <- lm2boot_out(fit, R = 100, seed = 418751, progress = FALSE)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

wv <- c(w1 = 5)

# Moderation
out <- cond_indirect(x = "x", y = "m1",
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect_i(x = "x", y = "m1",
                    est = lm2fit$est,
                    wvalues = wv)

# Moderation with bootstrap CI, precomputed bootstrapping
out_boot <- cond_indirect(x = "x", y = "m1",
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out)
out_boot_chk <- mapply(indirect_i,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "m1",
                                       wvalues = wv),
                       SIMPLIFY = FALSE)

# Moderated mediation with bootstrap CI, new bootstrapping
out_boot2 <- cond_indirect(x = "x", y = "m1",
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     R = 100,
                     parallel = FALSE,
                     progress = FALSE,
                     seed = 418751)

tmp <- capture.output(print(out))
tmp <- capture.output(print(out_boot))
tmp <- capture.output(print(out_boot2))

test_that("cond_indirect: lm", {
    expect_equal(out$indirect, out_chk$indirect)
    # expect_identical(out_boot$boot_indirect,
    #                  sapply(out_boot_chk, function(x) x$indirect))
  })

test_that("confint for indirect: lm", {
    expect_warning(confint(out))
    expect_equal(coef(out),  out_boot$indirect, ignore_attr = TRUE)
    expect_equal(confint(out_boot),  out_boot$boot_ci, ignore_attr = TRUE)
  })

out_boot2_preboot <- cond_indirect(x = "x", y = "m1",
                                    fit = fit,
                                    wvalues = wv,
                                    boot_ci = TRUE,
                                    boot_out = out_boot2)

test_that("cond_indirect: Use boot_out from previous run", {
    expect_equal(out_boot2_preboot$boot_ci, out_boot2$boot_ci, ignore_attr = TRUE)
  })

