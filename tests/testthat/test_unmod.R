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
vcov_y <- vcov(lm_y)
vcov_m <- vcov(lm_m)
out <- cond_indirect(x = "m", y = "y",  wvalues = c(w1 = 5), fit = lm_out)
expect_equal(out$original_se,
             sqrt(vcov_y["m", "m"]))
out <- cond_indirect(x = "x", y = "m",  wvalues = c(w1 = 5), fit = lm_out)
expect_equal(out$original_se,
             sqrt(vcov_m["x", "x"]))
out <- cond_indirect_effects(x = "m", y = "y", wlevels = c("w1"), fit = lm_out)
tmp <- range(sapply(attr(out, "full_output"), function(xx) xx$original_se))
expect_equal(mean(tmp), tmp[1])
out <- cond_indirect_effects(x = "x", y = "m", wlevels = c("w1"), fit = lm_out)
tmp <- range(sapply(attr(out, "full_output"), function(xx) xx$original_se))
expect_equal(mean(tmp), tmp[1])
expect_equal(attr(out, "full_output")[[1]]$original_se,
             sqrt(vcov_m["x", "x"]))
})

test_that("More tests", {
dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x, dat)
lm_y <- lm(y ~ m + x*w1*w2, dat)
lm_out <- lm2list(lm_m, lm_y)
b_m <- coef(lm_m)
b_y <- coef(lm_y)
vcov_y <- vcov(lm_y)
vcov_m <- vcov(lm_m)
out <- cond_indirect(x = "m", y = "y",  wvalues = c(w1 = 5), fit = lm_out)
expect_equal(out$original_se,
             sqrt(vcov_y["m", "m"]))
out <- cond_indirect(x = "x", y = "m",  wvalues = c(w1 = 5), fit = lm_out)
expect_equal(out$original_se,
             sqrt(vcov_m["x", "x"]))
out <- cond_indirect_effects(x = "m", y = "y", wlevels = c("w1"), fit = lm_out)
tmp <- range(sapply(attr(out, "full_output"), function(xx) xx$original_se))
expect_equal(mean(tmp), tmp[1])
expect_equal(attr(out, "full_output")[[1]]$original_se,
             sqrt(vcov_y["m", "m"]))
})