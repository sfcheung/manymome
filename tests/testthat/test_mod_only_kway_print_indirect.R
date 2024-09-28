library(testthat)
library(manymome)

test_that("Piint with original SE", {
dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)

out1 <- cond_effects(x = "x", y = "m", wlevels = "w1", fit = lm_m)
out11 <- get_one_cond_effect(out1, 1)
expect_output(print(out11),
              "Test statistic", fixed = TRUE)
})
