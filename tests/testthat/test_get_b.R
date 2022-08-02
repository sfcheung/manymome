library(manymome)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x
m2 ~ a2 * m1
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

test_that("get_b", {
    expect_equal(
        get_b(x = "m1", y = "m2", fit = fit),
        est[est$label == "a2", "est"]
      )
    expect_equal(
        get_b(x = "m3", y = "y", est = est),
        est[est$label == "a4", "est"]
      )
    expect_true(
        is.na(get_b(x = "y", y = "x", est = est))
      )
  })