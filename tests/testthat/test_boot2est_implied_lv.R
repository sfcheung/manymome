library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- simple_mediation_latent
mod <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * fx
fy ~ b * fm + cp * fx
indirect := a * b
"
fit <- sem(mod, dat, meanstructure = TRUE,
           se = "boot",
           bootstrap = 10,
           baseline = FALSE,
           h1 = FALSE,
           warn = FALSE)
boot_est <- lavInspect(fit, "boot")

fit_0 <- sem(mod, dat, meanstructure = TRUE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)
fit_1 <- sem(mod, dat[1:100, ], meanstructure = TRUE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)

implied <- get_implied_i(coef(fit_0), fit_1)
implied_check_cov <- lavInspect(fit_0, "cov.all")
implied_check_mean <- c(lavInspect(fit_0, "mean.ov"), lavInspect(fit_0, "mean.lv"))
implied$cov
implied_check_cov
implied$mean
implied_check_mean

out <- boot2implied(fit)
out_check <- get_implied_i(unname(boot_est[9, ]), fit_1)
out[[9]]

test_that("boot2est with implied stat: latent variables", {
    expect_equal(
        implied$cov,
        implied_check_cov,
        tolerance = 1e-5
      )
    expect_equal(
        unclass(implied$mean[1:9]),
        unclass(implied_check_mean[1:9]),
        tolerance = 1e-5
      )
    expect_identical(
        out[[9]],
        out_check
      )
  })

