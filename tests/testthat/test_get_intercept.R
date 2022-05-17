library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x
m2 ~ a2 * m1
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
est <- parameterEstimates(fit)

test_that("get_intercept", {
    expect_equal(
        get_intercept(x = "m1", fit = fit),
        est[(est$lhs == "m1") & (est$op == "~1"), "est"]
      )
    expect_equal(
        get_intercept(x = "y", est = est),
        est[(est$lhs == "y") & (est$op == "~1"), "est"]
      )
    expect_true(
        is.na(get_intercept(x = "z", est = est))
      )
  })