library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

lm_y_all_1 <- refit_x_i(x = "x", ys = c("m1", "m2", "m3"), xvalue = -1, fit = fit)
lm_y_all_2 <- refit_x_i(x = "x", ys = c("m1", "m2", "m3"), xvalue =  1, fit = fit)
y_effect_chk <- (get_intercept("y", lm_y_all_2) - get_intercept("y", lm_y_all_1)) / (1 - -1)

y_effect <- cond_effect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit)
y_effect$effect

wv <- c(w1 = 2, w2 = 4)
lm_y_all_b1 <- refit_x_i(x = "x", ys = c("m1", "m2", "m3"), xvalue = -1, fit = fit, wvalues = wv)
lm_y_all_b2 <- refit_x_i(x = "x", ys = c("m1", "m2", "m3"), xvalue =  1, fit = fit, wvalues = wv)
y_effect_bchk <- (get_intercept("y", lm_y_all_b2) - get_intercept("y", lm_y_all_b1)) / (1 - -1)

y_effect_b <- cond_effect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit, wvalues = wv)
y_effect_b$effect

test_that("check cond_effect using refit_x_i", {
    expect_equal(
        y_effect$effect,
        y_effect_chk
      )
    expect_equal(
        y_effect_b$effect,
        y_effect_bchk
      )
  })