# To be examined in an interactive session

library(manymome)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
dat <- add_na(dat, prop = .10, seed = 4589)
length(complete.cases(dat))
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
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, missing = "fiml")
set.seed(4456)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 5,
                warn = FALSE)

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

suppressWarnings(out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415,
                               R = 5, parallel = FALSE, progress = FALSE))
fit_boot_out <- do_boot(fit, R = 5, seed = 87415, parallel = FALSE, progress = FALSE)
suppressWarnings(out_5b <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, boot_out = fit_boot_out))
suppressWarnings(out_5c <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               boot_ci = TRUE, seed = 87415, R = 5, parallel = FALSE, progress = FALSE))

test_that("cond_indirect_effects: do_boot", {
  expect_identical(unlist(out_5), unlist(out_5b))
  expect_identical(unlist(out_5), unlist(out_5c))
  })
