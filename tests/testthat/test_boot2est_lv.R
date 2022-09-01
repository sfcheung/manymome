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
set.seed(986415)
fit <- sem(mod, dat, meanstructure = TRUE,
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

out <- boot2est(fit)
out[[i]]

fit_noboot <- sem(mod, dat, meanstructure = TRUE,
                  se = "standard",
                  baseline = FALSE,
                  h1 = FALSE)

test_that("boot2est: Latent variables", {
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
