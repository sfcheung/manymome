library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
set.seed(8715)
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           se = "boot",
           bootstrap = 10,
           baseline = FALSE,
           h1 = FALSE,
           warn = FALSE)

out <- suppressMessages(fit2boot_out_do_boot(fit, R = 15, seed = 8715))
out_chk_est <- boot2est(fit)
out_chk_implied <- boot2implied(fit)
out_chk <- fit2boot_out(fit = fit)

i <- 5
out[[i]]
out_chk_est[[i]]
out_chk_implied[[i]]
out_chk[[i]]

out_db <- do_boot(fit)

# # To examine in an interactive sessions.
# fit2 <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
#            baseline = FALSE,
#            h1 = FALSE,
#            warn = FALSE)
# out_parallel <- fit2boot_out_do_boot(fit2, R = 2, seed = 8715, parallel = TRUE,
#                                      ncores = 2,
#                                      progress = FALSE)
# out_db2 <- do_boot(fit2, R = 2, seed = 8715, ncores = 2, progress = FALSE)
# identical(out_parallel, out_db2)

test_that("fit2boot_out_do_boot", {
    expect_equal(
        out[[i]]$implied_stats$cov,
        out_chk_implied[[i]]$cov,
        tolerance = 1e-5
      )
    expect_equal(
        unclass(out[[i]]$implied_stats$mean),
        unclass(out_chk_implied[[i]]$mean),
        tolerance = 1e-5
      )
    expect_equal(
        out[[i]]$implied_stats$cov,
        out_chk[[i]]$implied_stats$cov,
        tolerance = 1e-5
      )
    expect_equal(
        unclass(out[[i]]$implied_stats$mean),
        unclass(out_chk[[i]]$implied_stats$mean),
        tolerance = 1e-5
      )
    expect_equal(
        out_db,
        out_chk
      )
  })
