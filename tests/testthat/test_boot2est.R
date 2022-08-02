
library(manymome)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
set.seed(986415)
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = TRUE,
           se = "boot",
           bootstrap = 5,
           baseline = FALSE,
           h1 = FALSE,
           warn = FALSE)
est <- parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)
boot_est <- lavInspect(fit, "boot")

i <- 3
est_boot_9 <- parameterTable(fit)
est_boot_9[est_boot_9$free > 0, "est"] <- unname(boot_est[i, ])
fit0 <- fit
fit0@ParTable$est[fit0@ParTable$free > 0] <- unname(boot_est[i, ])
est0 <- parameterEstimates(fit0, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE)

# waldo::compare(est, parameterEstimates(fit0))
identical(est0[, -5], est[, -5])
identical(est0[, 5], est[, 5])

out <- boot2est(fit)
out[[i]]

fit_noboot <- sem(mod, dat, meanstructure = TRUE, fixed.x = TRUE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)

test_that("boot2est", {
    expect_identical(
        out[[i]][1:26, -5],
        est[1:26, -5]
      )
    expect_false(
        identical(out[[i]][1:30, 5],
                  est[, 5])
      )
    expect_error(boot2est(fit_noboot))
  })
