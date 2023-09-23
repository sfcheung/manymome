skip("WIP: Testing lav_obs")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- simple_mediation_latent
dat$x <- rowMeans(dat[, c("x1", "x2", "x3")])
dat$m <- rowMeans(dat[, c("m1", "m2", "m3")])
dat$y <- rowMeans(dat[, c("y1", "y2", "y3")])

mod_x <-
"
# fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * x
fy ~ b * fm + cp * x
indirect := a * b
"
fit_x <- sem(mod_x, dat, se = "none", baseline = FALSE)

mod_m <-
"
fx =~ x1 + x2 + x3
# fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
m ~ a * fx
fy ~ b * m + cp * fx
indirect := a * b
"
fit_m <- sem(mod_m, dat, se = "none", baseline = FALSE)

mod_y <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
# fy =~ y1 + y2 + y3
fm ~ a * fx
y ~ b * fm + cp * fx
indirect := a * b
"
fit_y <- sem(mod_y, dat, se = "none", baseline = FALSE)

est <- parameterEstimates(fit)
std <- standardizedSolution(fit)
est[c(10:12, 25), ]

out_x <- indirect_i(x = "x", y = "fy", m = c("fm"), fit = fit_x,
                    allow_mixing_lav_and_obs = TRUE)
out_x
out_m <- indirect_i(x = "fx", y = "fy", m = c("m"), fit = fit_m)
out_m
out_y <- indirect_i(x = "fx", y = "y", m = c("fm"), fit = fit_y)
out_y


out_std <- indirect_i(x = "fx", y = "fy", m = c("fm"), fit = fit,
                    standardized_x = TRUE,
                    standardized_y = TRUE)
out_std

std[c(10:12, 25), ]

# Test against cond_indirect
out_cond <- cond_indirect(x = "fx", y = "fy", m = c("fm"), fit = fit)
out_cond$indirect
out$indirect

# Bootstrapping

set.seed(85701)
fit_boot <- sem(mod, dat,
                se = "bootstrap", bootstrap = 50, baseline = FALSE,
                h1 = FALSE, warn = FALSE)
# boot_out <- fit2boot_out(fit_boot)
boot_out <- suppressMessages(fit2boot_out_do_boot(fit, R = 50, seed = 85701))
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

out_cond_boot <- cond_indirect(x = "fx", y = "fy", m = c("fm"), fit = fit,
                               boot_ci = TRUE,
                               boot_out = boot_out)
out_cond_boot_chk <- mapply(indirect_i,
                            est = boot_est,
                            implied_stats = boot_implied_stats,
                            MoreArgs = list(x = "fx",
                                            y = "fy",
                                            m = "fm"),
                            SIMPLIFY = FALSE)

test_that("indirect: latent variable", {
    expect_equal(out$indirect,
                 est[25, "est"])
    expect_equal(out_std$indirect,
                 std[25, "est.std"])
    expect_equal(out_cond$indirect,
                 out$indirect)
    expect_equal(out_cond_boot$boot_indirect,
                     sapply(out_cond_boot_chk, function(x) x$indirect))
  })
