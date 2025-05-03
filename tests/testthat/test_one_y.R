skip("WIP")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# lavaan

dat <- modmed_x1m3w4y1
mod1 <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
fit_lv_1 <- sem(mod1, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

mod2 <-
"
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
fit_lv_2 <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

test_that("lavaan", {
  expect_error(get_one_response(fit_lv_1))
  expect_equal(get_one_response(fit_lv_2), "y")
  out1 <- cond_effects(wlevels = "m1", x = "w4", fit = fit_lv_2)
  out1_chk <- cond_effects(wlevels = "m1", x = "w4", y = "y", fit = fit_lv_2)
  expect_equal(coef(out1), coef(out1_chk))
  expect_error(cond_effects(wlevels = "m1", x = "w4", fit = fit_lv_1))
  out1 <- cond_indirect(wvalues = c(m1 = 1), x = "w4", fit = fit_lv_2)
  out1_chk <- cond_indirect(wvalues = c(m1 = 1), x = "w4", y = "y", fit = fit_lv_2)
  expect_equal(coef(out1), coef(out1_chk))
  expect_error(cond_indirect(wvalues = c(m1 = 1), x = "w4", fit = fit_lv_1))
})

# lm

dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit_lm_1 <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)
fit_lm_2 <- lm2list(lm_m1)

test_that("lm", {
  expect_error(get_one_response(fit_lm_1))
  expect_equal(get_one_response(fit_lm_2), "m1")
  out1 <- cond_effects(wlevels = "x", x = "w1", fit = fit_lm_2)
  out1_chk <- cond_effects(wlevels = "x", x = "w1", y = "m1", fit = fit_lm_2)
  expect_equal(coef(out1), coef(out1_chk))
  expect_error(cond_effects(wlevels = "x", x = "w1", fit = fit_lm_1))
  out1 <- cond_indirect(wvalues = c(x = 1), x = "w1", fit = fit_lm_2)
  out1_chk <- cond_indirect(wvalues = c(x = 1), x = "w1", y = "m1", fit = fit_lm_2)
  expect_equal(coef(out1), coef(out1_chk))
  expect_error(cond_indirect(wvalues = c(x = 1), x = "w1", fit = fit_lm_1))
})

# skip_on_cran()
# skip_if_not_installed("lavaan.mi")
# skip_if_not_installed("Amelia")
# skip("To be examined in an interactive session")
# # It works if run in an interactive mode.
# # Something's about the methods in lavaan.mi
# # Require lavaan.mi
# library(testthat)
# library(manymome)
# suppressMessages(library(lavaan))
# suppressMessages(library(lavaan.mi))

# fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
# load(fn)
