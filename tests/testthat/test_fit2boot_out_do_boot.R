library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
set.seed(8715)
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           se = "boot",
           bootstrap = 50,
           baseline = FALSE,
           h1 = FALSE)

out <- fit2boot_out_do_boot(fit, R = 50, seed = 8715)
out_chk_est <- boot2est(fit)
out_chk_implied <- boot2implied(fit)

out[[9]]
out_chk_est[[9]]
out_chk_implied[[9]]

test_that("fit2boot_out_do_boot", {
    expect_equal(
        out[[9]]$implied_stats$cov,
        out_chk_implied[[9]]$cov,
        tolerance = 1e-5
      )
    expect_equal(
        out[[9]]$implied_stats$mean,
        out_chk_implied[[9]]$mean,
        tolerance = 1e-5
      )
  })
