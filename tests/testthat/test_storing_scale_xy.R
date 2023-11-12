library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_y <- lm(y ~ m1 + x, dat)
fit <- list(lm_m1, lm_y)
dat$xw1 <- dat$x * dat$w1
mod <-
"
m1 ~ x + w1 + xw1
y ~ m1 + x
"
fit_lav <- sem(mod, dat, fixed.x = FALSE)
boot_out <- do_boot(fit, R = 10, seed = 12345, parallel = FALSE, progress = FALSE)
mc_out <- do_mc(fit_lav, R = 10, seed = 12345, parallel = FALSE, progress = FALSE)
suppressWarnings(ind <- cond_indirect_effects(wlevels = "w1",
                             x = "x",
                             m = "m1",
                             y = "y",
                             fit = fit,
                             boot_ci = TRUE,
                             boot_out = boot_out))
suppressWarnings(stdx <- cond_indirect_effects(wlevels = "w1",
                              x = "x",
                              m = "m1",
                              y = "y",
                              standardized_x = TRUE,
                              fit = fit,
                              boot_ci = TRUE,
                              boot_out = boot_out))
suppressWarnings(stdy <- cond_indirect_effects(wlevels = "w1",
                              x = "x",
                              m = "m1",
                              y = "y",
                              standardized_y = TRUE,
                              fit = fit,
                              boot_ci = TRUE,
                              boot_out = boot_out))
suppressWarnings(stdxy <- cond_indirect_effects(wlevels = "w1",
                               x = "x",
                               m = "m1",
                               y = "y",
                               standardized_x = TRUE,
                               standardized_y = TRUE,
                               fit = fit,
                               boot_ci = TRUE,
                               boot_out = boot_out))

suppressWarnings(indl <- cond_indirect_effects(wlevels = "w1",
                             x = "x",
                             m = "m1",
                             y = "y",
                             fit = fit_lav,
                             mc_ci = TRUE,
                             mc_out = mc_out))
suppressWarnings(stdxl <- cond_indirect_effects(wlevels = "w1",
                              x = "x",
                              m = "m1",
                              y = "y",
                              standardized_x = TRUE,
                              fit = fit_lav,
                              mc_ci = TRUE,
                              mc_out = mc_out))
suppressWarnings(stdyl <- cond_indirect_effects(wlevels = "w1",
                              x = "x",
                              m = "m1",
                              y = "y",
                              standardized_y = TRUE,
                              fit = fit_lav,
                              mc_ci = TRUE,
                              mc_out = mc_out))
suppressWarnings(stdxyl <- cond_indirect_effects(wlevels = "w1",
                               x = "x",
                               m = "m1",
                               y = "y",
                               standardized_x = TRUE,
                               standardized_y = TRUE,
                               fit = fit_lav,
                               mc_ci = TRUE,
                               mc_out = mc_out))

ind_1 <- attr(ind, "full_output")[[1]]
stdx_1 <- attr(stdx, "full_output")[[1]]
stdy_1 <- attr(stdy, "full_output")[[1]]
stdxy_1 <- attr(stdxy, "full_output")[[1]]

indl_1 <- attr(indl, "full_output")[[1]]
stdxl_1 <- attr(stdxl, "full_output")[[1]]
stdyl_1 <- attr(stdyl, "full_output")[[1]]
stdxyl_1 <- attr(stdxyl, "full_output")[[1]]

ind_boot <- ind_1$boot_indirect
stdx_boot <- stdx_1$boot_indirect
stdx_scale_x <- stdx_1$boot_scale_x
stdx_scale_y <- stdx_1$boot_scale_y
stdy_boot <- stdy_1$boot_indirect
stdy_scale_x <- stdy_1$boot_scale_x
stdy_scale_y <- stdy_1$boot_scale_y
stdxy_boot <- stdxy_1$boot_indirect
stdxy_scale_x <- stdxy_1$boot_scale_x
stdxy_scale_y <- stdxy_1$boot_scale_y

indl_mc <- indl_1$mc_indirect
stdxl_mc <- stdxl_1$mc_indirect
stdxl_scale_x <- stdxl_1$mc_scale_x
stdxl_scale_y <- stdxl_1$mc_scale_y
stdyl_mc <- stdyl_1$mc_indirect
stdyl_scale_x <- stdyl_1$mc_scale_x
stdyl_scale_y <- stdyl_1$mc_scale_y
stdxyl_mc <- stdxyl_1$mc_indirect
stdxyl_scale_x <- stdxyl_1$mc_scale_x
stdxyl_scale_y <- stdxyl_1$mc_scale_y

test_that("Recover unstd", {
    expect_equal(ind_boot,
                 stdx_boot * stdx_scale_y / stdx_scale_x)
    expect_equal(ind_boot,
                 stdy_boot * stdy_scale_y / stdy_scale_x)
    expect_equal(ind_boot,
                 stdxy_boot * stdxy_scale_y / stdxy_scale_x)
    expect_equal(indl_mc,
                 stdxl_mc * stdxl_scale_y / stdxl_scale_x)
    expect_equal(indl_mc,
                 stdyl_mc * stdyl_scale_y / stdyl_scale_x)
    expect_equal(indl_mc,
                 stdxyl_mc * stdxyl_scale_y / stdxyl_scale_x)
  })
