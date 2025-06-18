library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("kway plot", {

dat <- data_med_mod_b_mod
mod <-
"
m ~ x + w1 + c1 + c2 + x:w1 + c1:x + c2:x
y ~ x + w1 + m + w2 + x:m + m:w1 + m:w2
"

fit <- sem(mod, dat)

cond_out1 <- cond_effects(wlevels = c("w1", "c1", "c2"),
                          x = "x",
                          y = "m",
                          fit = fit)
cond_out2 <- cond_effects(wlevels = c("w1", "w2", "x"),
                          x = "m",
                          y = "y",
                          fit = fit)

expect_no_error(p <- plot(cond_out1))
expect_no_error(p <- plot(cond_out2))

})

