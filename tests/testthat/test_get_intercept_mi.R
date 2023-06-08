skip_on_cran()
skip_if_not_installed("semTools")
# Require semTools
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

est_mi <- lav_est(fit1_mi)
coef_mi <- coef(fit1_mi)

test_that("get_intercept", {
    expect_equal(
        unname(get_intercept(x = "m1", fit = fit1_mi)),
        unname(coef_mi["m1~1"])
      )
    expect_equal(
        unname(get_intercept(x = "y", est = est_mi)),
        unname(coef_mi["y~1"])
      )
    expect_true(
        is.na(get_intercept(x = "z", est = est_mi))
      )
  })
