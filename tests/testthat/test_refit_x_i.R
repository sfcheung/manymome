# Testlibrary(stdmodsem)
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

wv <- c(w1 = 2, w2 = 4)
out <- refit_x_i(x = "x", ys = c("m1", "m2"), xvalue = -.5, fit = fit, wvalues = wv)

out_1 <- refit_1_i(x = "x", xvalue = -.5, fit = fit, wvalues = wv)
out_2 <- refit_1_i(x = "m1", xvalue =  get_intercept("m1", out_1), fit = fit, wvalues = wv)
out_3 <- refit_1_i(x = "m2", xvalue =  get_intercept("m2", out_2), fit = fit, wvalues = wv)

isTRUE(all.equal(parameterEstimates(out_3), parameterEstimates(out)))

test_that("refit_x_i", {
    expect_equal(parameterEstimates(out),
                parameterEstimates(out_3))
  })