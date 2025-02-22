skip("WIP")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("do_mc: Faster", {

data(data_med_mod_ab1)
dat <- data_med_mod_ab1
mod <-
"
m ~ x + w + x:w + c1 + c2
y ~ m + w + m:w + x + c1 + c2
"
fit <- sem(mod, dat)

system.time(
mc_out <- do_mc(fit, R = 10000, seed = 1234)
)

system.time(
mc_out <- do_mc(fit, R = 10000, seed = 1234, compute_implied_stats = FALSE)
)

})