library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("cond_effects: labels with space", {

dat <- data_med_mod_parallel_cat

data_med_mod_parallel_cat$w1 <- gsub("group1", "group 1", data_med_mod_parallel_cat$w1)
data_med_mod_parallel_cat$w1 <- gsub("group2", "group 2", data_med_mod_parallel_cat$w1)
data_med_mod_parallel_cat$w1 <- gsub("group3", "group 3", data_med_mod_parallel_cat$w1)

lm_out <- lm(y ~ x*w1, data_med_mod_parallel_cat)

expect_no_warning(out <- cond_effects(wlevels = "w1",
                   x = "x",
                   fit = lm_out))
})

