skip_on_cran()
# This test is long and should be tested locally.
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
dat$m1w4 <- dat$m1 * dat$w4

modmo1 <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
y ~ b * m1 + cp * x
indmome := d1 * b
"
fitmo1 <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmo1_boot_out <- do_boot(fitmo1, R = 50, seed = 1234, parallel = FALSE, progress = !is_testing())
fitmo1_mc_out <- do_mc(fitmo1, R = 200, seed = 1234)

modmo2 <-
"
m1 ~ a * x
y ~ b * m1 + g4 * w4 + d4 * m1w4 + cp * x
indmome := d4 * a
"
fitmo2 <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmo2_boot_out <- do_boot(fitmo2, R = 50, seed = 1234, parallel = FALSE, progress = !is_testing())
fitmo2_mc_out <- do_mc(fitmo2, R = 200, seed = 1234)

modmomo <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
y ~ b * m1 + g4 * w4 + d4 * m1w4 + cp *x
indmome := d1 * d4
"
fitmomo <- sem(modmomo, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmomo_boot_out <- do_boot(fitmomo, R = 50, seed = 1234, parallel = FALSE, progress = !is_testing())
fitmomo_mc_out <- do_mc(fitmomo, R = 200, seed = 1234)

# Adapt to a change in lavaan 0.6-13
if (packageVersion("lavaan") > "0.6.12") {
    fitmo1_boot <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                    warn = FALSE, iseed = 1234)
    fitmo2_boot <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                    warn = FALSE, iseed = 1234)
    fitmomo_boot <- sem(modmomo, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                    warn = FALSE, iseed = 1234)
  } else {
    set.seed(1234)
    fitmo1_boot <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                    warn = FALSE)
    set.seed(1234)
    fitmo2_boot <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                    warn = FALSE)
    set.seed(1234)
    fitmomo_boot <- sem(modmomo, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                    warn = FALSE)
  }

test_that("index_of_mome and index_of_momome: SD as the unit", {

ind_mome1 <- index_of_mome(x = "x", y = "y", m = "m1", w = "w1",
                           fit = fitmo1, boot_ci = TRUE, boot_out = fitmo1_boot_out,
                           w_unit = "sd")
ind_mome1b <- z_index_of_mome(x = "x", y = "y", m = "m1", w = "w1",
                           fit = fitmo1, boot_ci = TRUE, boot_out = fitmo1_boot_out)
dat_tmp <- lavInspect(fitmo1, "data")[, "w1"]
w1_mean <-  mean(dat_tmp)
w1_sd <- sd(dat_tmp)
ind_mome1_chk <- index_of_mome(x = "x", y = "y", m = "m1", w = "w1",
                           fit = fitmo1, boot_ci = TRUE, boot_out = fitmo1_boot_out,
                           increase_from = w1_mean, increase_to = w1_mean + w1_sd)
expect_equal(
  coef(ind_mome1),
  coef(ind_mome1_chk),
  tolerance = 1e-4,
  ignore_attr = TRUE
)
expect_equal(
  coef(ind_mome1b),
  coef(ind_mome1_chk),
  tolerance = 1e-4,
  ignore_attr = TRUE
)

ind_mome2 <- index_of_mome(x = "x", y = "y", m = "m1", w = "w4",
                           fit = fitmo2, boot_ci = TRUE, boot_out = fitmo2_boot_out,
                           w_unit = "sd")
ind_mome2b <- z_index_of_mome(x = "x", y = "y", m = "m1", w = "w4",
                           fit = fitmo2, boot_ci = TRUE, boot_out = fitmo2_boot_out)
dat_tmp <- lavInspect(fitmo2, "data")[, "w4"]
w4_mean <-  mean(dat_tmp)
w4_sd <- sd(dat_tmp)
ind_mome2_chk <- index_of_mome(x = "x", y = "y", m = "m1", w = "w4",
                           fit = fitmo2, boot_ci = TRUE, boot_out = fitmo2_boot_out,
                           increase_from = w4_mean, increase_to = w4_mean + w4_sd)
expect_equal(
  coef(ind_mome2),
  coef(ind_mome2_chk),
  tolerance = 1e-4,
  ignore_attr = TRUE
)
expect_equal(
  coef(ind_mome2b),
  coef(ind_mome2_chk),
  tolerance = 1e-4,
  ignore_attr = TRUE
)

ind_momome <- index_of_momome(x = "x", y = "y", m = "m1", w = "w1", z = "w4",
                           fit = fitmomo, boot_ci = TRUE, boot_out = fitmomo_boot_out,
                           w_unit = "sd", z_unit = "sd")
ind_momome_b <- z_index_of_momome(x = "x", y = "y", m = "m1", w = "w1", z = "w4",
                           fit = fitmomo, boot_ci = TRUE, boot_out = fitmomo_boot_out)
dat_tmp <- lavInspect(fitmomo, "data")[, "w1"]
w1_mean <-  mean(dat_tmp)
w1_sd <- sd(dat_tmp)
dat_tmp <- lavInspect(fitmomo, "data")[, "w4"]
w4_mean <-  mean(dat_tmp)
w4_sd <- sd(dat_tmp)
ind_momome_chk <- index_of_momome(x = "x", y = "y", m = "m1", w = "w1", z = "w4",
                           fit = fitmomo, boot_ci = TRUE, boot_out = fitmomo_boot_out,
                           w_increase_from = w1_mean, w_increase_to = w1_mean + w1_sd,
                           z_increase_from = w4_mean, z_increase_to = w4_mean + w4_sd)
expect_equal(
  coef(ind_momome),
  coef(ind_momome_chk),
  tolerance = 1e-4,
  ignore_attr = TRUE
)
expect_equal(
  coef(ind_momome_b),
  coef(ind_momome_chk),
  tolerance = 1e-4,
  ignore_attr = TRUE
)

})
