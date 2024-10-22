library(testthat)
library(manymome)

test_that("merge_mod_levels: one level", {
data(data_mod2)
lm_out <- lm(y ~ x * w1 + x * w2 + x * c2, data_mod2)

wv1 <- mod_levels(w = "w1", fit = lm_out, sd_from_mean = -1)
wv2 <- mod_levels(w = "w2", fit = lm_out, sd_from_mean =  0)
wv3 <- mod_levels(w = "c2", fit = lm_out, sd_from_mean = -1)

expect_no_error(wvall <- merge_mod_levels(wv1, wv2, wv3))
expect_true(nrow(wvall) == 1)
})
