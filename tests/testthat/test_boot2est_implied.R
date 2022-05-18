library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           se = "boot",
           bootstrap = 100,
           baseline = FALSE,
           h1 = FALSE)
# est <- parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)
boot_est <- lavInspect(fit, "boot")
# est_boot_9 <- parameterTable(fit)
# est_boot_9[est_boot_9$free > 0, "est"] <- unname(boot_est[9, ])
# fit0 <- fit
# fit0@ParTable$est <- unname(boot_est[9, ])
# est0 <- parameterEstimates(fit0, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)

# # waldo::compare(est, parameterEstimates(fit0))
# identical(est0[, -5], est[, -5])
# identical(est0[, 5], est[, 5])

# out <- boot2est(fit)
# out[[9]]

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

fit_fixedx <- sem(mod, dat, meanstructure = TRUE, fixed.x = TRUE,
                  se = "boot",
                  bootstrap = 100,
                  baseline = FALSE,
                  h1 = FALSE)

test_that("boot2est with implied stat", {
    expect_equal(
        implied$cov,
        implied_check$cov,
        tolerance = 1e-5
      )
    expect_equal(
        implied$mean,
        implied_check$mean,
        tolerance = 1e-5
      )
    expect_identical(
        out[[9]],
        out_check
      )
    expect_error(
        boot2implied(fit_noboot)
      )
    expect_error(
        boot2implied(fit_fixedx)
      )
  })
