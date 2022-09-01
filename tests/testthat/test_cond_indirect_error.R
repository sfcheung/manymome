library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit_lav <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit_lm <- list(lm_m1, lm_m2, lm_m3, lm_y)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

test_that("cond_indirect: Check error", {
    expect_error(cond_indirect(fit = mod))
    expect_error(cond_indirect(fit = list(lm_m1, mod)))
    expect_error(cond_indirect(fit = fit_lav, boot_ci = TRUE))
  })

test_that("cond_indirect: Check error", {
    expect_error(cond_indirect("x", "m3", "y", fit = fit_lm))
    expect_error(cond_indirect("m3", "m2", fit = fit_lm))
  })


