skip_on_cran()
# Require Amelia
library(testthat)
library(manymome)
library(lavaan)
suppressMessages(library(semTools))
suppressMessages(library(Amelia))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
p <- ncol(dat)
pstar <- n * p
q <- 100
set.seed(51453)
tmp <- sample(pstar, q)
tmp2 <- list(i = row(matrix(NA, n, p))[tmp],
             j = col(matrix(NA, n, p))[tmp])
for (ii in seq_len(q)) {
    dat[tmp2$i[ii], tmp2$j[ii]] <- NA
  }

mod <-
"
m1 ~ a1 * x
m2 ~ a2 * m1 + m1:w2 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x + m3:w4 + w4
"
fit1_lav <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE, missing = "fiml", warn = FALSE)

mod2 <-
"
m1 ~ a1 * x
m2 ~ a2 * m1 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x
"
fit2_lav <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE, missing = "fiml", warn = FALSE)

set.seed(235413)
dat_mi <- amelia(dat, m = 5, noms = c(10, 11))$imputations

fit1_mi <- sem.mi(mod, dat_mi,
                  meanstructure = TRUE,
                  fixed.x = FALSE,
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)
fit2_mi <- sem.mi(mod2, dat_mi,
                  meanstructure = TRUE,
                  fixed.x = FALSE,
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)

dat_lv <- simple_mediation_latent
n <- nrow(dat_lv)
p <- ncol(dat_lv)
pstar <- n * p
q <- 100
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

test_that("lav_data_used with mi", {
    expect_true(all.equal(lav_data_used(fit1_mi),
                          lav_data_used(fit1_lav)))
    expect_true(all.equal(lav_data_used(fit1_mi, drop_colon = FALSE),
                          lav_data_used(fit1_lav, drop_colon = FALSE)))
    expect_true(all.equal(lav_data_used(fit2_mi),
                          lav_data_used(fit2_lav)))
    expect_true(all.equal(lav_data_used(fit2_mi, drop_colon = FALSE),
                          lav_data_used(fit2_lav, drop_colon = FALSE)))
    expect_true(all.equal(lav_data_used(fit_lv_mi),
                          lav_data_used(fit_lv_lav)))
    expect_true(all.equal(lav_data_used(fit_lv_mi, drop_colon = FALSE),
                          lav_data_used(fit_lv_lav, drop_colon = FALSE)))
  })
