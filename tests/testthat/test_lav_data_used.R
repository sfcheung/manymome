library(testthat)
library(manymome)
library(lavaan)

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x
m2 ~ a2 * m1 + m1:w2 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x + m3:w4 + w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
datm <- dat
datm[1, ] <- NA
datm[2, 1:5] <- NA
fitm <- sem(mod, datm, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

mod2 <-
"
m1 ~ a1 * x
m2 ~ a2 * m1 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x
"
fit2 <- sem(mod2, datm, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE, missing = "fiml", warn = FALSE)

test_that("lav_data_used", {
    expect_equal(nrow(lav_data_used(fit)), 200)
    expect_equal(nrow(lav_data_used(fitm)), 198)
    expect_equal(ncol(lav_data_used(fit)), 7)
    expect_equal(ncol(lav_data_used(fitm)), 7)
    expect_equal(ncol(lav_data_used(fit, drop_colon = FALSE)), 9)
    expect_equal(ncol(lav_data_used(fitm, drop_colon = FALSE)), 9)
    expect_equal(nrow(lav_data_used(fit2)), 199)
  })
