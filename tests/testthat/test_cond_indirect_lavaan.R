library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
set.seed(85701)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "bootstrap", bootstrap = 100, baseline = FALSE)
boot_out <- fit2boot_out(fit_boot)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

wv <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

out <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect(x = "x", y = "y",
                    m = c("m1", "m2", "m3"),
                    est = parameterEstimates(fit),
                    wvalues = wv)

out_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit_boot,
                     wvalues = wv,
                     boot_ci = TRUE)
out_boot_chk <- mapply(indirect,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1", "m2", "m3"),
                                       wvalues = wv),
                       SIMPLIFY = FALSE)

test_that("cond_indirect: lavaan", {
    expect_identical(out$indirect, out_chk$indirect)
    expect_identical(out_boot$boot_indirect,
                     sapply(out_boot_chk, function(x) x$indirect))
  })
