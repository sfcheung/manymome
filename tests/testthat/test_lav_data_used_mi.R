skip_on_cran()
skip_if_not_installed("lavaan.mi")

library(testthat)
library(manymome)

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

test_that("lav_data_used with mi", {
    expect_true(all.equal(lav_data_used(fit_cat_mi),
                          lav_data_used(fit_cat_lav)))
    expect_true(all.equal(lav_data_used(fit_cat_mi, drop_colon = FALSE),
                          lav_data_used(fit_cat_lav, drop_colon = FALSE)))
    expect_true(all.equal(lav_data_used(fit1_mi),
                          lav_data_used(fit1_lav)))
    expect_true(all.equal(lav_data_used(fit1_mi, drop_colon = FALSE),
                          lav_data_used(fit1_lav, drop_colon = FALSE)))
    expect_true(all.equal(lav_data_used(fit_lv_mi),
                          lav_data_used(fit_lv_lav)))
    expect_true(all.equal(lav_data_used(fit_lv_mi, drop_colon = FALSE),
                          lav_data_used(fit_lv_lav, drop_colon = FALSE)))
  })
