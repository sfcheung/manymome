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
m1 ~ a1 * x
m2 ~ a2 * m1 + m1:w2 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x + m3:w4 + w4
"
fit1_lav <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

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

test_that("lav_data_used with mi", {
    expect_equal(nrow(lav_data_used(fit)), 200)
    expect_equal(nrow(lav_data_used(fitm)), 198)
    expect_equal(ncol(lav_data_used(fit)), 7)
    expect_equal(ncol(lav_data_used(fitm)), 7)
    expect_equal(ncol(lav_data_used(fit, drop_colon = FALSE)), 9)
    expect_equal(ncol(lav_data_used(fitm, drop_colon = FALSE)), 9)
    expect_equal(nrow(lav_data_used(fit2)), 199)
  })
