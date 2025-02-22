skip("WIP")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("do_boot: Faster", {

data(data_med_mod_ab1)
dat <- data_med_mod_ab1
mod <-
"
m ~ x + w + x:w + c1 + c2
y ~ m + w + m:w + x + c1 + c2
"
fit <- sem(mod, dat)

system.time(
boot_out <- do_boot(fit, R = 500, seed = 1234, parallel = FALSE)
)

system.time(
boot_out <- do_boot(fit, R = 50, seed = 1234, parallel = FALSE, compute_implied_stats = FALSE)
)

lm_m <- lm(m ~ x + w + x:w + c1 + c2, dat)
lm_y <- lm(y ~ m + w + m:w + x + c1 + c2, dat)
lm_all <- list(lm_m, lm_y)

system.time(
boot_out <- do_boot(lm_all, R = 500, seed = 1234, parallel = FALSE)
)

system.time(
boot_out <- do_boot(lm_all, R = 50, seed = 1234, parallel = FALSE, compute_implied_stats = FALSE)
)

})