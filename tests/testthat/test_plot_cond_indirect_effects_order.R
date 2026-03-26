test_that("Two moderators", {

library(testthat)
library(manymome)

suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1

lm_m3 <- lm(m3 ~ m1 + x*w4, dat)

cond_out <- cond_effects(wlevels = "w4", x = "x", fit = lm_m3)

# Check manually
expect_no_error(plot(cond_out))

lm_out <- lm(y ~ x*w1, data_med_mod_parallel_cat)

cond_out <- cond_effects(wlevels = "w1",
                        x = "x",
                        fit = lm_out)

# Check manually
expect_no_error(plot(cond_out))

})

