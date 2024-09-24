skip_on_cran()
# The plots should be examined in an interactive session

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_med_mod_a
dat$xw <- dat$x * dat$w
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)
mod <-
"
m ~ x + w + xw + c1 + c2
y ~ m + x + c1 + c2
"
fit_lav <- sem(mod, dat, fixed.x = FALSE)

boot_out_lm <- do_boot(fit_lm,
                       R = 50,
                       seed = 54532,
                       parallel = FALSE,
                       progress = FALSE)
boot_out_lav <- do_boot(fit_lav,
                        R = 50,
                        seed = 54532,
                        parallel = FALSE,
                        progress = FALSE)
mc_out_lav <- do_mc(fit_lav,
                    R = 50,
                    parallel = FALSE,
                    progress = FALSE)

# Indirect path

out_lm <- cond_indirect_effects(wlevels = "w",
                                x = "x",
                                y = "y",
                                m = "m",
                                fit = fit_lm,
                                sd_from_mean = seq(-2, 2, .2),
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)
out_lm_no_ci <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "y",
                                      m = "m",
                                      fit = fit_lm,
                                      sd_from_mean = seq(-2, 2, .2))
out_lav <- cond_indirect_effects(wlevels = "w",
                                 x = "x",
                                 y = "y",
                                 m = "m",
                                 fit = fit_lav,
                                 sd_from_mean = seq(-2, 2, .2),
                                 boot_ci = TRUE,
                                 boot_out = boot_out_lav)
out_lav_no_ci <- cond_indirect_effects(wlevels = "w",
                                       x = "x",
                                       y = "y",
                                       m = "m",
                                       fit = fit_lav,
                                       sd_from_mean = seq(-2, 2, .2))
out_lav_mc <- cond_indirect_effects(wlevels = "w",
                                    x = "x",
                                    y = "y",
                                    m = "m",
                                    fit = fit_lav,
                                    sd_from_mean = seq(-2, 2, .2),
                                    mc_ci = TRUE,
                                    mc_out = mc_out_lav)

# No zero

out_lm_no_zero1 <- cond_indirect_effects(wlevels = "w",
                                         x = "x",
                                         y = "y",
                                         m = "m",
                                         fit = fit_lm,
                                         sd_from_mean = seq(-.5, .5, .2) + 5,
                                         boot_ci = TRUE,
                                         boot_out = boot_out_lm)
out_lm_no_zero2 <- cond_indirect_effects(wlevels = "w",
                                         x = "x",
                                         y = "y",
                                         m = "m",
                                         fit = fit_lm,
                                         sd_from_mean = seq(-.5, .5, .2) - 50,
                                         boot_ci = TRUE,
                                         boot_out = boot_out_lm)

# Std

out_lm_stdx <- cond_indirect_effects(wlevels = "w",
                                x = "x",
                                y = "y",
                                m = "m",
                                fit = fit_lm,
                                sd_from_mean = seq(-2, 2, .2),
                                standardized_x = TRUE,
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)
out_lm_stdxy <- cond_indirect_effects(wlevels = "w",
                                x = "x",
                                y = "y",
                                m = "m",
                                fit = fit_lm,
                                sd_from_mean = seq(-2, 2, .2),
                                standardized_x = TRUE,
                                standardized_y = TRUE,
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)

# Direct path

out_lm_direct <- cond_indirect_effects(wlevels = "w",
                                x = "x",
                                y = "m",
                                fit = fit_lm,
                                sd_from_mean = seq(-2, 2, .2),
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)
out_lm_no_ci_direct <- cond_indirect_effects(wlevels = "w",
                                      x = "x",
                                      y = "m",
                                      fit = fit_lm,
                                      sd_from_mean = seq(-2, 2, .2))
out_lav_direct <- cond_indirect_effects(wlevels = "w",
                                 x = "x",
                                 y = "m",
                                 fit = fit_lav,
                                 sd_from_mean = seq(-2, 2, .2),
                                 boot_ci = TRUE,
                                 boot_out = boot_out_lav)
out_lav_no_ci_direct <- cond_indirect_effects(wlevels = "w",
                                       x = "x",
                                       y = "m",
                                       fit = fit_lav,
                                       sd_from_mean = seq(-2, 2, .2))
out_lav_mc_direct <- cond_indirect_effects(wlevels = "w",
                                    x = "x",
                                    y = "m",
                                    fit = fit_lav,
                                    sd_from_mean = seq(-2, 2, .2),
                                    mc_ci = TRUE,
                                    mc_out = mc_out_lav)

out_lm_direct_auto <- cond_indirect_effects(wlevels = "w",
                                x = "x",
                                y = "m",
                                fit = fit_lm,
                                sd_from_mean = c(-2, 2),
                                boot_ci = TRUE,
                                boot_out = boot_out_lm)

test_that("plot_effect_vs_w", {
  expect_no_error(p1 <- plot_effect_vs_w(out_lm,
                                         w = "w"))
  p1
  expect_no_error(p1b <- plot_effect_vs_w(out_lm))
  p1b
  expect_no_error(p2 <- plot_effect_vs_w(out_lm_no_ci,
                                         w = "w",
                                         add_zero_line = FALSE))
  p2
  expect_no_error(p3 <- plot_effect_vs_w(out_lav,
                                         w = "w",
                                         zero_line_color = "red"))
  p3
  expect_no_error(p4 <- plot_effect_vs_w(out_lav_no_ci,
                                         w = "w"))
  p4
  expect_no_error(p5 <- plot_effect_vs_w(out_lav_mc,
                                         w = "w"))
  p5
  expect_no_error(p6 <- plot_effect_vs_w(out_lav_mc,
                                         w = "w",
                                         line_color = "red",
                                         line_linewidth = 5))
  p6
  expect_no_error(p7 <- plot_effect_vs_w(out_lav_mc,
                                         w = "w",
                                         shade_the_band = FALSE))
  p7
  expect_no_error(p8 <- plot_effect_vs_w(out_lav_mc,
                                         w = "w",
                                         draw_the_intervals = FALSE))
  p8
  expect_no_error(p9 <- plot_effect_vs_w(out_lav_mc,
                                         w = "w",
                                         band_fill_color = "green",
                                         band_alpha = 1))
  p9
  expect_no_error(p10 <- plot_effect_vs_w(out_lav_mc,
                                          w = "w",
                                          intervals_linetype = "solid",
                                          intervals_linewidth = 5))
  p10
  expect_no_error(p11 <- plot_effect_vs_w(out_lav_mc_direct,
                                          w = "w",
                                          intervals_linetype = "solid",
                                          intervals_linewidth = 5))
  p11
  expect_no_error(p12 <- plot_effect_vs_w(out_lm_stdx,
                                          w = "w"))
  p12
  expect_no_error(p13 <- plot_effect_vs_w(out_lm_stdxy,
                                          w = "w"))
  p13
  expect_no_error(p14 <- plot_effect_vs_w(out_lm_no_zero1,
                                          w = "w"))
  p14
  expect_no_error(p15 <- plot_effect_vs_w(out_lm_no_zero1,
                                          w = "w",
                                          always_draw_zero_line = TRUE))
  p15
  expect_no_error(p16 <- plot_effect_vs_w(out_lm_no_zero2,
                                          w = "w",
                                          always_draw_zero_line = TRUE))
  p16
  expect_error(plot_effect_vs_w(out_lm_no_zero2,
                                w = "abc"))
  expect_no_error(p17 <- plot_effect_vs_w(out_lm_no_zero2,
                                          always_draw_zero_line = TRUE))
  p17
})

wrange <- pseudo_johnson_neyman(out_lm_direct_auto)
wrange_extended <- fill_wlevels(wrange,
                                out_lm_direct_auto,
                                k = 11)
wrange_extended2 <- fill_wlevels(out_lm_direct_auto,
                                 k = 7)

test_that("Expand wlevels", {
  expect_no_error(p1 <- plot_effect_vs_w(wrange_extended))
  p1
  expect_no_error(p1 <- plot_effect_vs_w(wrange_extended2))
  p1
})

# Multigroup

# On hold. cond_indirect_effects() does not yet support
#   multigroup models that have within-group moderators.

# dat_mg <- modmed_x1m3w4y1
# n <- nrow(dat_mg)
# set.seed(860314)
# dat_mg$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
# dat_mg$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)

# dat_mg <- cbind(dat_mg, factor2var(dat_mg$gp, prefix = "gp", add_rownames = FALSE))
# dat_mg <- cbind(dat_mg, factor2var(dat_mg$city, prefix = "city", add_rownames = FALSE))

# mod_mg <-
# "
# m3 ~ m1 + x
# y ~ m2 + m3 + x + w4 + m3:w4
# "

# fit_mg <- sem(mod_mg, dat_mg, fixed.x = FALSE,
#               group = "gp")

# boot_out_lav_mg <- do_boot(fit_mg,
#                            R = 50,
#                            seed = 54532,
#                            parallel = FALSE,
#                            progress = FALSE)
# mc_out_lav_mg <- do_mc(fit_lav,
#                        R = 50,
#                        parallel = FALSE,
#                        progress = FALSE)

# Plot
