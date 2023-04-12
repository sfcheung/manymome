skip("Long tests: Test in interactive sections")

# To he examined in an interactive sessions.
# Bootstrapping CIs and Monte Carlo CIs are expected
# to be different, especially when MLR is used.
# The goal is to identify any large discrepancy.

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# From vignette "med_lav"

dat <- data_sem

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~  f1 + f2
f4 ~  f1 + f3
"
fit_med <- sem(model = mod,
               data = dat)

boot_out_med <- do_boot(fit_med,
                        R = 5000,
                        seed = 98171,
                        ncores = 9)
mc_out_med <- do_mc(fit_med,
                        R = 10000,
                        seed = 98171)

out_f1f3f4_boot <- indirect_effect(x = "f1",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med)
out_f1f3f4_mc <- indirect_effect(x = "f1",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = mc_out_med)

out_f1f3f4_boot
out_f1f3f4_mc

out_f2f3f4_boot <- indirect_effect(x = "f2",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med)
out_f2f3f4_mc <- indirect_effect(x = "f2",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = mc_out_med)

out_f2f3f4_boot
out_f2f3f4_mc

std_f1f3f4_boot <- indirect_effect(x = "f1",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med,
                              standardized_x = TRUE,
                              standardized_y = TRUE)
std_f1f3f4_mc <- indirect_effect(x = "f1",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = mc_out_med,
                              standardized_x = TRUE,
                              standardized_y = TRUE)

std_f1f3f4_boot
std_f1f3f4_mc

std_f2f3f4_boot <- indirect_effect(x = "f2",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = boot_out_med,
                              standardized_x = TRUE,
                              standardized_y = TRUE)
std_f2f3f4_mc <- indirect_effect(x = "f2",
                              y = "f4",
                              m = "f3",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = mc_out_med,
                              standardized_x = TRUE,
                              standardized_y = TRUE)

std_f2f3f4_boot
std_f2f3f4_mc

out_f1f4_boot <- indirect_effect(x = "f1",
                            y = "f4",
                            fit = fit_med,
                            boot_ci = TRUE,
                            boot_out = boot_out_med)
out_f1f4_mc <- indirect_effect(x = "f1",
                            y = "f4",
                            fit = fit_med,
                            mc_ci = TRUE,
                            mc_out = mc_out_med)

out_f1f4_boot
out_f1f4_mc

out_f1_total_boot <- out_f1f3f4_boot + out_f1f4_boot
out_f1_total_mc <- out_f1f3f4_mc + out_f1f4_mc

out_f1_total_boot
out_f1_total_mc

out_f1_diff_boot <- out_f1f4_boot - out_f1f3f4_boot
out_f1_diff_mc <- out_f1f4_mc - out_f1f3f4_mc

out_f1_diff_boot
out_f1_diff_mc

all_paths <- all_indirect_paths(fit = fit_med)
all_paths

out_all_boot <- many_indirect_effects(paths = all_paths,
                                 fit = fit_med,
                                 boot_ci = TRUE,
                                 boot_out = boot_out_med)
out_all_mc <- many_indirect_effects(paths = all_paths,
                                 fit = fit_med,
                                 mc_ci = TRUE,
                                 mc_out = mc_out_med)

out_all_boot
out_all_mc

out1_boot <- out_all_boot[[1]]
out1_mc <- out_all_mc[[1]]

out1_boot
out1_mc

out2_boot <- out_all_boot[["f2 -> f3 -> f4"]]
out2_mc <- out_all_mc[["f2 -> f3 -> f4"]]

out2_boot
out2_mc
