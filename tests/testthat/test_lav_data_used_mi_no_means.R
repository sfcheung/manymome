skip_on_cran()
# Require Amelia
library(testthat)
library(manymome)
library(lavaan)
suppressMessages(library(semTools))
suppressMessages(library(Amelia))

dat <- add_na(data_mod,
              prop = .20,
              seed = 1314)
sum(complete.cases(dat))
set.seed(879051)
dat_mi <- amelia(dat, m = 2)$imputations
mod <-
"
y ~ x + w + x:w + c1
"
fit_mi <- sem.mi(mod, dat_mi, meanstructure = FALSE)
fit_mi_dat <- lav_data_used(fit_mi)
tmpnames <- intersect(colnames(fit_mi_dat),
                      colnames(dat))

test_that("Moderation, MI, no means", {
    expect_identical(fit_mi_dat[, tmpnames],
                     as.matrix(dat[, tmpnames]))
  })

mod2 <-
"
w ~ x + c1
y ~ w + x + c2
"
fit2_mi <- sem.mi(mod2, dat_mi, meanstructure = FALSE)
fit2_mi_dat <- lav_data_used(fit2_mi)
tmpnames <- intersect(colnames(fit2_mi_dat),
                      colnames(dat))

test_that("Mediation, MI, no means", {
    expect_identical(fit2_mi_dat[, tmpnames],
                     as.matrix(dat[, tmpnames]))
  })
