library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4 + cp * x + b3 * w3 + d3 * x:w3 + b4x * x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

out_1 <- get_prod(x = "x", y = "m1", fit = fit)
out_2 <- get_prod(x = "m3", y = "m2", fit = fit)
out_3 <- get_prod(x = "x", y = "y", fit = fit)
out_4 <- get_prod(x = "y", y = "x", fit = fit)
out_5 <- get_prod(x = "w1", y = "m1", fit = fit)
out_6 <- get_prod(x = "w2", y = "m2", fit = fit)
out_7 <- get_prod(x = "m2", y = "m3", fit = fit)

test_that("get_prod", {
    expect_equal(
        out_1$prod,
        "x:w1"
      )
    expect_equal(
        unname(out_1$b),
        est[est$label == "d1", "est"]
      )
    expect_equal(
        length(out_3$b),
        2
      )
    expect_true(
        is.na(out_4)
      )
    expect_equal(
        out_5$prod,
        "x:w1"
      )
    expect_equal(
        out_6$prod,
        "m1:w2"
      )
    expect_true(
        is.na(out_7)
      )
  })