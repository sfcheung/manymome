library(stdmodsem)
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

test_that("check path: Interrupted path", {
  expect_false(
      check_path(fit = fit, x = "x", y = "y", m = c("m1", "m2"))
    )
  expect_false(
      check_path(fit = fit, x = "m1", y = "y", m = c("m2"))
    )
  expect_false(
      check_path(est = est, x = "x", y = "m3", m = c("m2"))
    )
  })

test_that("check path: Valid path", {
  expect_true(
      check_path(fit = fit, x = "x", y = "y", m = c("m1", "m2", "m3"))
    )
  expect_true(
      check_path(est = est, x = "m1", y = "y", m = c("m2", "m3"))
    )
  expect_true(
      check_path(fit = fit, x = "x", y = "m3", m = c("m1", "m2"))
    )
  })

test_that("check path: Incorrect order", {
  expect_false(
      check_path(est = est, x = "x", y = "y", m = c("m2", "m1", "m3"))
    )
  expect_false(
      check_path(fit = fit, x = "m1", y = "x", m = c("m2", "m3"))
    )
  expect_false(
      check_path(fit = fit, x = "m3", y = "x", m = c("m2", "m1"))
    )
  })

test_that("check path: No m", {
  expect_true(
      check_path(fit = fit, x = "x", y = "y")
    )
  expect_false(
      check_path(fit = fit, x = "y", y = "x")
    )
  expect_true(
      check_path(est = est, x = "m2", y = "m3")
    )
  })