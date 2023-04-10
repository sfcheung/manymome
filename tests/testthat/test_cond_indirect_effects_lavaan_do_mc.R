skip("WIP")
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)

mc_out <- do_mc(fit, R = 100, seed = 597)

# TO PROCESS

set.seed(4456)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 5,
                warn = FALSE)

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

suppressWarnings(out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415,
                               R = 10, parallel = FALSE, progress = FALSE))

fit_mc_out <- do_mc(fit, R = 10, seed = 87415)
class(fit_mc_out) <- c("boot_out", class(fit_mc_out))
suppressWarnings(out_5b <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, boot_out = fit_mc_out))
# suppressWarnings(out_5c <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
#                                boot_ci = TRUE, seed = 87415, R = 10, parallel = FALSE, progress = FALSE))

test_that("cond_indirect_effects: do_boot", {
  expect_equal(out_5[-c(1:5)], out_5b[-c(1:5)])
  expect_equal(out_5[-c(1:5)], out_5c[-c(1:5)])
  })
