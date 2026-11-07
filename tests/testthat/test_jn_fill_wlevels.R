skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("pseudo_johnson_neyman", {

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

make_jn_old <- function(x) {
  x$cond_out_call <- NULL
  x
}

w_range <- pseudo_johnson_neyman(out)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out_stdx)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_stdx, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out_stdxy)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_stdxy, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out, w_lower = -5)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out, w_lower = -5, extendInt = "yes")
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out, w_upper = 0)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out,
                                            w_lower = -5, w_upper = 0)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out2)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out2, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out_direct)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_direct, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out_mc)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_mc, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out_mc_direct)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_mc_direct, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

# Check level: bootstrap
w_range <- pseudo_johnson_neyman(out, level = .90)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

# Check level: MC
w_range <- pseudo_johnson_neyman(out_mc, level = .80)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_mc, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

w_range <- pseudo_johnson_neyman(out_mc, level = .60)
w_range_old <- make_jn_old(w_range)
w_range_filled0 <- fill_wlevels(w_range, k = 5)
w_range_filled1 <- fill_wlevels(w_range_old, cond_out = out_mc, k = 5)

expect_equal(
  coef(w_range_filled0),
  coef(w_range_filled1),
  tolerance = 1e-2
)

})
