
library(stdmodsem)
library(lavaan)
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

out <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect(x = "x", y = "y",
                    m = c("m1", "m2", "m3"),
                    est = lm2fit$est,
                    wvalues = wv)

out_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out)
out_boot_chk <- mapply(indirect,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1", "m2", "m3"),
                                       wvalues = wv),
                       SIMPLIFY = FALSE)

out_boot2 <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     R = 100,
                     seed = 418751)


test_that("cond_indirect: lavaan", {
    expect_identical(out$indirect, out_chk$indirect)
    expect_identical(out_boot$boot_indirect,
                     sapply(out_boot_chk, function(x) x$indirect))
    expect_identical(out_boot$boot_indirect,
                     out_boot$boot_indirect)
  })
