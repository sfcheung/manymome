skip("WIP")

skip("Long tests: Test in interactive sections")

# To he examined in an interactive sessions.
# Bootstrapping CIs and Monte Carlo CIs are expected
# to be different, especially when MLR is used.
# The goal is to identify any large discrepancy.

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# From vignette "manymome"

dat <- data_med_mod_ab

mod <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
"

lm_m <- lm(
  m ~ x*w1 + c1 + c2,
  dat
)
lm_y <- lm(
  y ~ m*w2 + x + c1 + c2,
  dat
)
fit <- lm2list(
  lm_m,
  lm_y
)

fit_boot <- do_boot(fit = fit,
                    R = 5000,
                    seed = 53253)

fit_mc <- do_mc(fit = fit,
                R = 10000,
                seed = 53253)

out_cond_boot <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                       x = "x",
                                       y = "y",
                                       m = "m",
                                       fit = fit,
                                       boot_ci = TRUE,
                                       boot_out = fit_boot)

out_cond_mc <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                     x = "x",
                                     y = "y",
                                     m = "m",
                                     fit = fit,
                                     mc_ci = TRUE,
                                     mc_out = fit_mc)

confint(out_cond_boot)
confint(out_cond_mc)

# Should throw an error
out_cond_stdxy_mc <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                        x = "x",
                                        y = "y",
                                        m = "m",
                                        fit = fit,
                                        mc_ci = TRUE,
                                        mc_out = fit_mc,
                                        standardized_x = TRUE,
                                        standardized_y = TRUE)

out_momome_boot <- index_of_momome(x = "x",
                              y = "y",
                              m = "m",
                              w = "w1",
                              z = "w2",
                              fit = fit,
                              boot_ci = TRUE,
                              boot_out = fit_boot)

out_momome_mc <- index_of_momome(x = "x",
                              y = "y",
                              m = "m",
                              w = "w1",
                              z = "w2",
                              fit = fit,
                              mc_ci = TRUE,
                              mc_out = fit_mc)

out_momome_boot
out_momome_mc

# Moderation Mediation

mod2 <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + x + c1 + c2
"
lm2_m <- lm(
  m ~ x*w1 + c1 + c2,
  dat
)
lm2_y <- lm(
  y ~ m + x + c1 + c2,
  dat
)
fit2 <- lm2list(
  lm2_m,
  lm2_y
)

fit2_boot <- do_boot(fit = fit2,
                    R = 5000,
                    seed = 53253)

fit2_mc <- do_mc(fit = fit2,
                    R = 10000,
                    seed = 53253)

out_mome_boot <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w1",
                          fit = fit2,
                          boot_ci = TRUE,
                          boot_out = fit2_boot)

out_mome_mc <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w1",
                          fit = fit2,
                          mc_ci = TRUE,
                          mc_out = fit2_mc)

out_mome_boot
out_mome_mc

# Mediation Only

dat <- data_serial
print(head(dat), digits = 3)

mod_med <- "
m1 ~ x + c1 + c2
m2 ~ m1 + x + c1 + c2
y ~ m2 + m1 + x + c1 + c2
"
lm_m1 <- lm(
  m1 ~ x + c1 + c2,
  dat
)
lm_m2 <- lm(
  m2 ~ m1 + x + c1 + c2,
  dat
)
lm_y <- lm(
  y ~ m2 + m1 + x + c1 + c2,
  dat
)
fit_med <- lm2list(
  lm_m1,
  lm_m2,
  lm_y
)

out_med_boot <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           boot_ci = TRUE,
                           R = 5000,
                           seed = 43143)

out_med_mc <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           mc_ci = TRUE,
                           R = 5000,
                           seed = 43143)

confint(out_med_boot)
confint(out_med_mc)

# Should throw an error
out_med_stdxy_mc <- indirect_effect(x = "x",
                                 y = "y",
                                 m = c("m1", "m2"),
                                 fit = fit_med,
                                 mc_ci = TRUE,
                                 mc_out = out_med_mc,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE)

out_x_m2_y_boot <- indirect_effect(x = "x",
                              y = "y",
                              m = "m2",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = out_med_boot)
out_x_m2_y_mc <- indirect_effect(x = "x",
                              y = "y",
                              m = "m2",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = out_med_mc)

confint(out_x_m2_y_boot)
confint(out_x_m2_y_mc)

out_x_m1_y_boot <- indirect_effect(x = "x",
                              y = "y",
                              m = "m1",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = out_med_boot)
out_x_m1_y_mc <- indirect_effect(x = "x",
                              y = "y",
                              m = "m1",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = out_med_mc)

confint(out_x_m1_y_boot)
confint(out_x_m1_y_mc)

total_ind_boot <- out_med_boot + out_x_m1_y_boot + out_x_m2_y_boot
total_ind_mc <- out_med_mc + out_x_m1_y_mc + out_x_m2_y_mc

confint(total_ind_boot)
confint(total_ind_mc)

out_x_direct_boot <- indirect_effect(x = "x",
                                y = "y",
                                fit = fit_med,
                                boot_ci = TRUE,
                                boot_out = out_med_boot)
out_x_direct_mc <- indirect_effect(x = "x",
                                y = "y",
                                fit = fit_med,
                                mc_ci = TRUE,
                                mc_out = out_med_mc)

confint(out_x_direct_boot)
confint(out_x_direct_mc)

total_effect_boot <- out_med_boot + out_x_m1_y_boot + out_x_m2_y_boot + out_x_direct_boot
total_effect_mc <- out_med_mc + out_x_m1_y_mc + out_x_m2_y_mc + out_x_direct_mc

confint(total_effect_boot)
confint(total_effect_mc)

