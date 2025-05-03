skip_on_cran()
skip_if_not_installed("lavaan.mi")
# Require lavaan.mi
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(lavaan.mi))

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

est_1_mi <- lav_est(fit1_mi)

test_that("get_b for lavaan.mi", {
    expect_equal(
        unname(get_b(x = "m1", y = "m2", fit = fit1_mi)),
        unname(coef_1_mi["a2"])
      )
    expect_equal(
        unname(get_b(x = "m3", y = "y", est = est_1_mi)),
        unname(coef_1_mi["a4"])
      )
    expect_true(
        is.na(get_b(x = "y", y = "x", est = est_1_mi))
      )
  })
