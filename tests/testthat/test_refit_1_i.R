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

out_1 <- refit_1_i(x = "x", xvalue = -.5, fit = fit, wvalues =  c(w1 = 2, w2 = 4))
out_2 <- refit_1_i(x = "x", xvalue =  2, fit = fit)
out_3 <- refit_1_i(x = "m2", xvalue = 3, fit = fit, wvalues =  c(w2 = -3))
out_4 <- refit_1_i(x = "m3", xvalue = -4, fit = fit)

dat1 <- dat
dat1$x <- dat$x - (-.5)
dat1$w1 <- dat$w1 - 2
dat1$w2 <- dat$w2 - 4
fit1 <- update(fit, data = dat1)
isTRUE(all.equal(parameterEstimates(fit1), parameterEstimates(out_1)))

dat2 <- dat
dat2$x <- dat$x - 2
dat2$w1 <- dat$w1 - 0
dat2$w2 <- dat$w2 - 0
fit2 <- update(fit, data = dat2)
isTRUE(all.equal(parameterEstimates(fit2), parameterEstimates(out_2)))

dat3 <- dat
dat3$m2 <- dat$m2 - (3)
dat3$w1 <- dat$w1
dat3$w2 <- dat$w2 - (-3)
fit3 <- update(fit, data = dat3)
isTRUE(all.equal(parameterEstimates(fit3), parameterEstimates(out_3)))

dat4 <- dat
dat4$m3 <- dat$m3 - (-4)
dat4$w1 <- dat$w1
dat4$w2 <- dat$w2
fit4 <- update(fit, data = dat4)
isTRUE(all.equal(parameterEstimates(fit4), parameterEstimates(out_4)))

test_that("refit_1_i", {
    expect_equal(parameterEstimates(out_1),
                parameterEstimates(fit1))
    expect_equal(parameterEstimates(out_2),
                parameterEstimates(fit2))
    expect_equal(parameterEstimates(out_3),
                parameterEstimates(fit3))
    expect_equal(parameterEstimates(out_4),
                parameterEstimates(fit4))
  })