library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
set.seed(879153) # 31
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           se = "boot",
           bootstrap = 10,
           baseline = FALSE,
           h1 = FALSE,
           warn = FALSE)
boot_est <- lavInspect(fit, "boot")

fit_noboot <- sem(mod, dat, meanstructure = TRUE, fixed.x = TRUE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)

fit_0 <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)
fit_1 <- sem(mod, dat[1:100, ], meanstructure = TRUE, fixed.x = FALSE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)
p_free <- parameterTable(fit_1)$free > 0
implied <- get_implied_i(coef(fit_0), fit_1)
implied_check <- lavInspect(fit_0, "implied")
implied$cov
implied_check$cov

out <- boot2implied(fit)
out_check <- get_implied_i(unname(boot_est[9, ]), fit_1)
out[[9]]

set.seed(98751)
fit_fixedx <- sem(mod, dat, meanstructure = TRUE, fixed.x = TRUE,
                  se = "boot",
                  bootstrap = 6,
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)

out2 <- fit2boot_out(fit = fit)
out2_est_chk <- boot2est(fit)
out2_implied_chk <- boot2implied(fit)

test_that("boot2est with implied stat", {
    expect_equal(
        implied$cov,
        implied_check$cov,
        tolerance = 1e-5
      )
    expect_equal(
        unclass(implied$mean),
        unclass(implied_check$mean),
        tolerance = 1e-5
      )
    expect_equal(
        out[[9]],
        out_check
      )
    expect_error(
        boot2implied(fit_noboot)
      )
    # # It supports fixed.x now.
    # expect_error(
    #     boot2implied(fit_fixedx)
    #   )
    expect_equal(
        out2[[5]]$est,
        out2_est_chk[[5]]
      )
    expect_equal(
        out2[[5]]$implied,
        out2_implied_chk[[5]]
      )
  })
