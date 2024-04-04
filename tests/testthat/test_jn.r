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
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
fit_mc <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           baseline = FALSE)
fit2 <- sem(mod2, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)

# Create levels of w1, the moderators
wlevels <- mod_levels("w", fit = fit, sd_from_mean = c(-5, 0, 5))
wlevels2 <- mod_levels("w2", fit = fit2, sd_from_mean = c(-5, 0, 5))

# Conditional effects from x to y when w1 is equal to each of the levels
boot_out <- do_boot(fit,
                    R = 50,
                    seed = 4314,
                    parallel = FALSE,
                    progress = FALSE)
boot_out2 <- do_boot(fit2,
                    R = 50,
                    seed = 4314,
                    parallel = FALSE,
                    progress = FALSE)
mc_out <- do_mc(fit_mc,
                R = 100,
                seed = 1234,
                parallel = FALSE,
                progress = FALSE)
out <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels,
                             fit = fit,
                             boot_ci = TRUE,
                             boot_out = boot_out)
out_stdx <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels,
                             fit = fit,
                             standardized_x = TRUE,
                             boot_ci = TRUE,
                             boot_out = boot_out)
out_stdxy <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels,
                             fit = fit,
                             standardized_x = TRUE,
                             standardized_y = TRUE,
                             boot_ci = TRUE,
                             boot_out = boot_out)
out2 <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels2,
                             fit = fit2,
                             boot_ci = TRUE,
                             boot_out = boot_out2)
out_mc <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels,
                             fit = fit_mc,
                             mc_ci = TRUE,
                             mc_out = mc_out)
out_direct <- cond_indirect_effects(x = "x", y = "m",
                             wlevels = wlevels,
                             fit = fit,
                             boot_ci = TRUE,
                             boot_out = boot_out)
out_mc_direct <- cond_indirect_effects(x = "x", y = "m",
                             wlevels = wlevels,
                             fit = fit_mc,
                             mc_ci = TRUE,
                             mc_out = mc_out)
out_nb <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels,
                             fit = fit)

test_that("pseudo_johnson_neyman", {
    w_range <- pseudo_johnson_neyman(out)
    expect_equal(confint(w_range$cond_effects)[1, 1], 0,
                 tolerance = 1e-5)
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

# Trap error

date <- data_med_mod_a
date$wx <- dat$x * dat$w
date$c1m <- dat$c1 * dat$m
mode <-
"
m ~ x + w + wx
y  ~ m + x + c1 + c1m
"
suppressWarnings(fite <- sem(mode, date,
                            meanstructure = TRUE, fixed.x = FALSE,
                            se = "none", baseline = FALSE))
oute <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = c("w", "c1"),
                             fit = fite)

test_that("pseudo_johnson_neyman: expect error", {
  expect_error(pseudo_johnson_neyman(oute))
  expect_error(pseudo_johnson_neyman(out_nb))
  expect_error(pseudo_johnson_neyman(1))
})
