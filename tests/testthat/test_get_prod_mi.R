skip_on_cran()
skip_if_not_installed("semTools")
# Require semTools
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

out_1 <- get_prod(x = "x", y = "m1", fit = fit1_lav)
out_2 <- get_prod(x = "m3", y = "m2", fit = fit1_lav)
out_3 <- get_prod(x = "x", y = "y", fit = fit1_lav)
out_4 <- get_prod(x = "y", y = "x", fit = fit1_lav)
out_5 <- get_prod(x = "w1", y = "m1", fit = fit1_lav)
out_6 <- get_prod(x = "w2", y = "m2", fit = fit1_lav)
out_7 <- get_prod(x = "m2", y = "m3", fit = fit1_lav)

out_mi_1 <- get_prod(x = "x", y = "m1", fit = fit1_mi)
out_mi_2 <- get_prod(x = "m3", y = "m2", fit = fit1_mi)
out_mi_3 <- get_prod(x = "x", y = "y", fit = fit1_mi)
out_mi_4 <- get_prod(x = "y", y = "x", fit = fit1_mi)
out_mi_5 <- get_prod(x = "w1", y = "m1", fit = fit1_mi)
out_mi_6 <- get_prod(x = "w2", y = "m2", fit = fit1_mi)
out_mi_7 <- get_prod(x = "m2", y = "m3", fit = fit1_mi)

# No need to compare b values
out_mi_3$b <- NULL
out_3$b <- NULL
out_mi_6$b[] <- out_6$b

test_that("get_prod for lavaan.mi", {
    expect_equal(out_mi_1, out_1)
    expect_equal(out_mi_2, out_2)
    expect_equal(out_mi_3, out_3)
    expect_equal(out_mi_4, out_4)
    expect_equal(out_mi_5, out_5)
    expect_equal(out_mi_6, out_6)
    expect_equal(out_mi_7, out_7)
  })
