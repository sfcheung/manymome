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

# Form the product terms
dat$w1x <- dat$w1 * dat$x
dat$w2m <- dat$w2 * dat$m
mod <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
# Covariances of the error term of m with w2m and w2
m ~~ w2m + w2
# Covariance between other variables
# They need to be added due to the covariances added above
# See Kwan and Chan (2018) and Miles et al. (2015)
w2m ~~ w2 + x + w1 + w1x + c1 + c2
w2  ~~ x + w1 + w1x + c1 + c2
x   ~~ w1 + w1x + c1 + c2
w1  ~~ w1x + c1 + c2
w1x ~~ c1 + c2
c1  ~~ c2
"
fit <- sem(model = mod,
           data = dat,
           fixed.x = FALSE,
           estimator = "MLR")

fit_boot <- do_boot(fit = fit,
                    R = 5000,
                    seed = 53253,
                    ncores = 9)

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

out_cond_stdxy_boot <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                        x = "x",
                                        y = "y",
                                        m = "m",
                                        fit = fit,
                                        boot_ci = TRUE,
                                        boot_out = fit_boot,
                                        standardized_x = TRUE,
                                        standardized_y = TRUE)

out_cond_stdxy_mc <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                        x = "x",
                                        y = "y",
                                        m = "m",
                                        fit = fit,
                                        mc_ci = TRUE,
                                        mc_out = fit_mc,
                                        standardized_x = TRUE,
                                        standardized_y = TRUE)

round(confint(out_cond_stdxy_boot), 3)
round(confint(out_cond_stdxy_mc), 3)

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

dat$w1x <- dat$w1 * dat$x
mod2 <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + x + c1 + c2
"
fit2 <- sem(model = mod2,
           data = dat,
           fixed.x = FALSE,
           estimator = "MLR")

fit2_boot <- do_boot(fit = fit2,
                    R = 5000,
                    seed = 53253,
                    ncores = 9)

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
fit_med <- sem(model = mod_med,
               data = dat,
               fixed.x = TRUE)

out_med_boot <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           boot_ci = TRUE,
                           R = 5000,
                           seed = 43143,
                           ncores = 9)

out_med_mc <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           mc_ci = TRUE,
                           R = 5000,
                           seed = 43143)

out_med_boot
out_med_mc

out_med_stdxy_boot <- indirect_effect(x = "x",
                                 y = "y",
                                 m = c("m1", "m2"),
                                 fit = fit_med,
                                 boot_ci = TRUE,
                                 boot_out = out_med_boot,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE)
out_med_stdxy_mc <- indirect_effect(x = "x",
                                 y = "y",
                                 m = c("m1", "m2"),
                                 fit = fit_med,
                                 mc_ci = TRUE,
                                 mc_out = out_med_mc,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE)

out_med_stdxy_boot
out_med_stdxy_mc

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

out_x_m2_y_boot
out_x_m2_y_mc

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

out_x_m1_y_boot
out_x_m1_y_mc

total_ind_boot <- out_med_boot + out_x_m1_y_boot + out_x_m2_y_boot
total_ind_mc <- out_med_mc + out_x_m1_y_mc + out_x_m2_y_mc

total_ind_boot
total_ind_mc

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

out_x_direct_boot
out_x_direct_mc

total_effect_boot <- out_med_boot + out_x_m1_y_boot + out_x_m2_y_boot + out_x_direct_boot
total_effect_mc <- out_med_mc + out_x_m1_y_mc + out_x_m2_y_mc + out_x_direct_mc
total_effect_boot
total_effect_mc

# MI

dat_miss <- data_med_mod_ab
n <- nrow(dat_miss)
p <- ncol(dat_miss)
pstar <- n * p
q <- 30
set.seed(51453)
tmp <- sample(pstar, q)
tmp2 <- list(i = row(matrix(NA, n, p))[tmp],
             j = col(matrix(NA, n, p))[tmp])
for (ii in seq_len(q)) {
    dat_miss[tmp2$i[ii], tmp2$j[ii]] <- NA
  }

# Form the product terms
dat_miss$w1x <- dat_miss$w1 * dat_miss$x
dat_miss$w2m <- dat_miss$w2 * dat_miss$m
mod <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
# Covariances of the error term of m with w2m and w2
m ~~ w2m + w2
# Covariance between other variables
# They need to be added due to the covariances added above
# See Kwan and Chan (2018) and Miles et al. (2015)
w2m ~~ w2 + x + w1 + w1x + c1 + c2
w2  ~~ x + w1 + w1x + c1 + c2
x   ~~ w1 + w1x + c1 + c2
w1  ~~ w1x + c1 + c2
w1x ~~ c1 + c2
c1  ~~ c2
"
fit <- sem(model = mod,
           data = dat_miss,
           fixed.x = FALSE,
           missing = "fiml.x")

set.seed(235413)
dat_mi <- amelia(dat_miss, m = 5)$imputations

fit_mi <- sem.mi(mod, dat_mi,
                 meanstructure = TRUE,
                 fixed.x = FALSE,
                 baseline = FALSE,
                 h1 = FALSE,
                 warn = FALSE)

fit_mc <- do_mc(fit = fit_mi,
                R = 10000,
                seed = 53253)

