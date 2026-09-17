skip_on_cran()
# Require lavaan.mi
library(testthat)
library(manymome)

test_that("get_vcov: lm", {
dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit <- auto_lm2list(list(lm_m1, lm_m2, lm_m3, lm_y))
out0 <- get_vcov(fit)
expect_equal(
  sum(out0),
  4.06101986378025,
  tolerance = 1e-7
)
})
