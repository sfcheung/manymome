skip_on_cran()
# Require Amelia
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))
suppressMessages(library(Amelia))

dat <- modmed_x1m3w4y1[, 1:9]
n <- nrow(dat)
p <- ncol(dat)
pstar <- n * p
q <- 10
set.seed(51453)
tmp <- sample(pstar, q)
tmp2 <- list(i = row(matrix(NA, n, p))[tmp],
             j = col(matrix(NA, n, p))[tmp])
for (ii in seq_len(q)) {
    dat[tmp2$i[ii], tmp2$j[ii]] <- NA
  }

mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
fit_lav <- sem(mod, dat,
               meanstructure = TRUE,
               fixed.x = FALSE,
               missing = "fiml.x",
               baseline = FALSE,
               h1 = FALSE,
               warn = FALSE)

set.seed(235413)
dat_mi <- amelia(dat, m = 5)$imputations

fit_mi <- sem.mi(mod, dat_mi,
                 meanstructure = TRUE,
                 fixed.x = FALSE,
                 baseline = FALSE,
                 h1 = FALSE,
                 warn = FALSE)


test_that("get_implied_i with ov", {
    tmp1 <- get_implied_i(coef(fit_lav), fit_lav)
    tmp2 <- get_implied_i(coef(fit_lav), fit_mi)
    expect_identical(tmp1, tmp2)
  })

dat_lv <- simple_mediation_latent
n <- nrow(dat_lv)
p <- ncol(dat_lv)
pstar <- n * p
q <- 10
set.seed(51453)
tmp <- sample(pstar, q)
tmp2 <- list(i = row(matrix(NA, n, p))[tmp],
             j = col(matrix(NA, n, p))[tmp])
for (ii in seq_len(q)) {
    dat_lv[tmp2$i[ii], tmp2$j[ii]] <- NA
  }

mod_lv <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * fx
fy ~ b * fm + cp * fx
indirect := a * b
"
fit_lv_lav <- sem(mod_lv, dat_lv,
                  meanstructure = TRUE,
                  missing = "fiml.x",
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)

set.seed(235413)
dat_lv_mi <- amelia(dat_lv, m = 5)$imputations

fit_lv_mi <- sem.mi(mod_lv, dat_lv_mi,
                    meanstructure = TRUE,
                    missing = "fiml.x",
                    baseline = FALSE,
                    h1 = FALSE,
                    warn = FALSE)

test_that("get_implied_i with lv", {
    tmplv1 <- get_implied_i(coef(fit_lv_lav), fit_lv_lav)
    tmplv2 <- get_implied_i(coef(fit_lv_lav), fit_lv_mi)
    expect_identical(tmplv1, tmplv2)
  })
