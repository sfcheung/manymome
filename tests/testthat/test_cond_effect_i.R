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
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
ce_1 <- cond_effect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues)
ce_1$indirect

dat0 <- dat
dat0[, names(wvalues)] <- dat[, names(wvalues)] - t(replicate(nrow(dat), wvalues))
fit0 <- update(fit, data = dat0)
ce_1_chk <- indirect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit0)

test_that("check cond_effect_i using indirect", {
    expect_equal(
        ce_1$indirect,
        ce_1_chk[1]
      )
  })