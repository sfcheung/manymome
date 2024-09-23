skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Test

set.seed(1234)
n <- 500
x <- rnorm(n)
w <- rnorm(n)
y <- .3 * x * w + .2 * rnorm(n)
dat1 <- data.frame(x, w, y)

fit1 <- lm2list(lm(y ~ x*w, dat1))

# Create levels of w1, the moderators
wlevels1 <- mod_levels("w", fit = fit1, sd_from_mean = c(-10, 0, 10))
wlevels1up <- mod_levels("w", fit = fit1, sd_from_mean = c(0, 5, 10))
wlevels1lo <- mod_levels("w", fit = fit1, sd_from_mean = c(-10, -5, 0))

out1 <- cond_indirect_effects(x = "x",
                              y = "y",
                              wlevels = wlevels1,
                              fit = fit1)
out1stdxy <- cond_indirect_effects(x = "x",
                                   y = "y",
                                   wlevels = wlevels1,
                                   fit = fit1,
                                   standardized_x = TRUE,
                                   standardized_y = TRUE)
out1up <- cond_indirect_effects(x = "x",
                                y = "y",
                                wlevels = wlevels1up,
                                fit = fit1)
out1lo <- cond_indirect_effects(x = "x",
                                y = "y",
                                wlevels = wlevels1lo,
                                fit = fit1)

test_that("pseudo_johnson_neyman", {
    w_range <- pseudo_johnson_neyman(out1)
    expect_equal(confint(w_range$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    expect_error(pseudo_johnson_neyman(out1stdxy))

    w_range <- pseudo_johnson_neyman(out1up)
    expect_equal(confint(w_range$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)

    w_range_not_found1 <- pseudo_johnson_neyman(out1up, w_lower = 5)
    expect_true(confint(w_range_not_found1$cond_effects)[1, 1] > 0)

    w_range_not_found2 <- pseudo_johnson_neyman(out1up, w_upper = -10)
    expect_true(confint(w_range_not_found2$cond_effects)[2, 2] < 0)

    w_range_not_found1_ext <- pseudo_johnson_neyman(out1up, w_lower = -5, extendInt = "yes")
    expect_equal(confint(w_range_not_found1_ext$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
    expect_equal(confint(w_range_not_found1_ext$cond_effects)[2, 2], 0,
                 tolerance = 1e-5)
})
