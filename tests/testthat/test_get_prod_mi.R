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
q <- 30
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
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4 + cp * x + b3 * w3 + d3 * x:w3 + b4x * x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE, missing = "fiml.x")
est <- parameterEstimates(fit)

out_1 <- get_prod(x = "x", y = "m1", fit = fit)
out_2 <- get_prod(x = "m3", y = "m2", fit = fit)
out_3 <- get_prod(x = "x", y = "y", fit = fit)
out_4 <- get_prod(x = "y", y = "x", fit = fit)
out_5 <- get_prod(x = "w1", y = "m1", fit = fit)
out_6 <- get_prod(x = "w2", y = "m2", fit = fit)
out_7 <- get_prod(x = "m2", y = "m3", fit = fit)

set.seed(235413)
dat_mi <- amelia(dat, m = 5)$imputations

fit_mi <- sem.mi(mod, dat_mi,
                 meanstructure = TRUE,
                 fixed.x = FALSE,
                 baseline = FALSE,
                 h1 = FALSE,
                 warn = FALSE)

out_mi_1 <- get_prod(x = "x", y = "m1", fit = fit_mi)
out_mi_2 <- get_prod(x = "m3", y = "m2", fit = fit_mi)
out_mi_3 <- get_prod(x = "x", y = "y", fit = fit_mi)
out_mi_4 <- get_prod(x = "y", y = "x", fit = fit_mi)
out_mi_5 <- get_prod(x = "w1", y = "m1", fit = fit_mi)
out_mi_6 <- get_prod(x = "w2", y = "m2", fit = fit_mi)
out_mi_7 <- get_prod(x = "m2", y = "m3", fit = fit_mi)

# No need to compare b values
out_mi_1$b[] <- out_1$b
out_mi_3$b[] <- out_3$b
out_mi_5$b[] <- out_5$b
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
