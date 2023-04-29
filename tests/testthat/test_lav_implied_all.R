skip("Require Amelia")
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

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
           fixed.x = TRUE,
           baseline = FALSE,
           h1 = FALSE,
           warn = FALSE)

suppressMessages(library(Amelia))
set.seed(235413)
dat_mi <- amelia(dat, m = 5)$imputations

fit_mi <- sem.mi(mod, dat_mi,
                 meanstructure = TRUE, fixed.x = TRUE,
                 baseline = FALSE,
                 h1 = FALSE,
                 warn = FALSE)

tmp1 <- get_implied_i(coef(fit_lav), fit_lav)
tmp2 <- get_implied_i(coef(fit_mi), fit_mi)
