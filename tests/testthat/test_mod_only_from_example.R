library(lavaan)
library(testthat)

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x  + d1 * w1 + e1 * x:w1
m2 ~ a2 * x
y  ~ b1 * m1 + b2 * m2 + cp * x
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

w1levels <- mod_levels("w1", fit = fit)
w1levels
tmp <- cond_indirect_effects(x = "x", y = "m1",
                             wlevels = w1levels, fit = fit)
test_that("SE None", {
    expect_warning(confint(tmp),
                   "not", fixed = TRUE)
    expect_true(suppressWarnings(all(is.na(confint(tmp)))))
  })
