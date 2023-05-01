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
m1 ~ a1 * x
m2 ~ a2 * m1
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE, missing = "fiml.x")
est <- parameterEstimates(fit)

set.seed(235413)
dat_mi <- amelia(dat, m = 5)$imputations

fit_mi <- sem.mi(mod, dat_mi,
                 meanstructure = TRUE,
                 fixed.x = FALSE,
                 baseline = FALSE,
                 h1 = FALSE,
                 warn = FALSE)
est_mi <- lav_est(fit_mi)
coef_mi <- coef(fit_mi)

test_that("get_b for lavaan.mi", {
    expect_equal(
        unname(get_b(x = "m1", y = "m2", fit = fit_mi)),
        unname(coef_mi["a2"])
      )
    expect_equal(
        unname(get_b(x = "m3", y = "y", est = est_mi)),
        unname(coef_mi["a4"])
      )
    expect_true(
        is.na(get_b(x = "y", y = "x", est = est_mi))
      )
  })
