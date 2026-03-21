test_that("Two moderators", {

library(testthat)
library(manymome)

suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1

lm_m3 <- lm(m3 ~ m1 + x*w4, dat)

cond_out <- cond_effects(wlevels = "w4", x = "x", fit = lm_m3)

expect_no_error(plot(cond_out))

})

