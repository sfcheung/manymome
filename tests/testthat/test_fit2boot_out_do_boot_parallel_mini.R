skip_on_cran()
# A minimal test for coverage

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

out <- suppressMessages(fit2boot_out_do_boot(fit, R = 4, seed = 8715, ncores = 2,
                                             progress = FALSE))

test_that("A minimal test for parallel processing: lavaan", {
    expect_s3_class(out, "boot_out")
    expect_true(length(out) == 4)
  })
