skip("WIP")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("More tests", {
dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x, dat)
lm_y <- lm(y ~ m + x*w1, dat)
lm_out <- lm2list(lm_m, lm_y)
b_m <- coef(lm_m)
b_y <- coef(lm_y)
out <- cond_indirect_effects(x = "m", y = "y", wlevels = c("w1"), fit = lm_out)
out <- cond_indirect(x = "m", y = "y",  wvalues = c(w1 = 5), fit = lm_out)
out <- cond_indirect(x = "x", y = "m",  wvalues = c(w1 = 5), fit = lm_out)
out <- cond_indirect(x = "x", y = "y",  wvalues = c(w1 = 5), fit = lm_out)
})