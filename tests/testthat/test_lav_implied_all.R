skip_on_cran()
skip_if_not_installed("lavaan.mi")
# Require lavaan.mi
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(lavaan.mi))

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

test_that("get_implied_i with ov", {
    tmp1 <- get_implied_i(coef(fit1_lav), fit1_lav)
    tmp2 <- get_implied_i(coef(fit1_lav), fit1_mi)
    expect_identical(tmp1, tmp2)
  })

test_that("lav_implied_all with ov", {
    tmpa2 <- lav_implied_all(fit1_mi)
    tmpa2_chk <- get_implied_i(methods::getMethod("coef",
                                signature = "lavaan.mi",
                                where = asNamespace("lavaan.mi"))(fit1_mi),
                               fit1_mi)
    expect_identical(tmpa2, tmpa2_chk)
  })

test_that("get_implied_i with lv", {
    tmplv1 <- get_implied_i(coef(fit_lv_lav), fit_lv_lav)
    tmplv2 <- get_implied_i(coef(fit_lv_lav), fit_lv_mi)
    expect_identical(tmplv1, tmplv2)
  })

test_that("lav_implied_all with lv", {
    tmpa2 <- lav_implied_all(fit_lv_mi)
    tmpa2_chk <- get_implied_i(methods::getMethod("coef",
                                signature = "lavaan.mi",
                                where = asNamespace("lavaan.mi"))(fit_lv_mi),
                               fit_lv_mi)
    expect_identical(tmpa2, tmpa2_chk)
  })

