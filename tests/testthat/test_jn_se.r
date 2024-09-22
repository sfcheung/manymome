skip("WIP")
skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Test

dat <- data_med_mod_a
dat$wx <- dat$x * dat$w
dat$w2 <- -1 * dat$w
dat$w2x <- dat$x * dat$w2
mod <-
"
m ~ x + w + wx
y  ~ m + x
"
mod2 <-
"
m ~ x + w2 + w2x
y  ~ m + x
"
fit1 <- lm2list(lm(m ~ x*w + c1 + c2, dat))
fit2 <- lm2list(lm(m ~ x*w2 + c1 + c2, dat))

# Create levels of w1, the moderators
wlevels1 <- mod_levels("w", fit = fit1, sd_from_mean = c(-10, 0, 10))
wlevels2 <- mod_levels("w2", fit = fit2, sd_from_mean = c(-10, 0, 10))

out1 <- cond_indirect_effects(x = "x",
                              y = "m",
                              wlevels = wlevels1,
                              fit = fit1)
out2 <- cond_indirect_effects(x = "x",
                              y = "m",
                              wlevels = wlevels2,
                              fit = fit2)

jn_out1 <- pseudo_johnson_neyman(out1)
jn_out2 <- pseudo_johnson_neyman(out2)


test_that("pseudo_johnson_neyman", {
    w_range <- pseudo_johnson_neyman(out1)
    expect_equal(confint(w_range$cond_effects)[1, 1], 0,
                 tolerance = 1e-4)
    expect_equal(confint(w_range$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_stdx <- pseudo_johnson_neyman(out_stdx)
    expect_equal(confint(w_range_stdx$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range_stdx$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_stdxy <- pseudo_johnson_neyman(out_stdxy)
    expect_equal(confint(w_range_stdxy$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range_stdxy$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_not_found1 <- pseudo_johnson_neyman(out, w_lower = -5)
    expect_equal(confint(w_range_not_found1$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_false(confint(w_range_not_found1$cond_effects)[2, 1] > 0)

    w_range_not_found1_ext <- pseudo_johnson_neyman(out, w_lower = -5, extendInt = "yes")
    expect_equal(confint(w_range_not_found1_ext$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range_not_found1_ext$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_not_found2 <- pseudo_johnson_neyman(out, w_upper = 0)
    expect_false(confint(w_range_not_found2$cond_effects)[1, 1] > 0)
    expect_equal(confint(w_range_not_found2$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_not_found3 <- pseudo_johnson_neyman(out,
                                                w_lower = -5, w_upper = 0)
    expect_false(confint(w_range_not_found3$cond_effects)[1, 1] > 0)
    expect_false(confint(w_range_not_found3$cond_effects)[2, 2] < 0)

    w_range2 <- pseudo_johnson_neyman(out2)
    expect_equal(confint(w_range2$cond_effects)[1, 2], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range2$cond_effects)[2, 1], 0,
                 tolerance = 1e-5)

    w_range_direct <- pseudo_johnson_neyman(out_direct)
    expect_equal(confint(w_range_direct$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range_direct$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_mc <- pseudo_johnson_neyman(out_mc)
    expect_equal(confint(w_range_mc$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_false(confint(w_range_mc$cond_effects)[2, 1] > 0)
    w_range_mc_direct <- pseudo_johnson_neyman(out_mc_direct)
    expect_equal(confint(w_range_mc_direct$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_false(confint(w_range_mc_direct$cond_effects)[2, 1] > 0)
})
