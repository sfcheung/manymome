skip_on_cran()
skip_if_not_installed("lavaan.mi")
# Require lavaan.mi
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(lavaan.mi))

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

test_that("get_vcov", {
    tmp1 <- get_vcov(fit1_lav)
    tmp2 <- get_vcov(fit1_mi)
    expect_identical(rownames(tmp1), rownames(tmp2))
    expect_identical(colnames(tmp1), colnames(tmp2))
    expect_identical(tmp1, lavInspect(fit1_lav, "vcov"))
    expect_identical(tmp2, vcov(fit1_mi))
  })
