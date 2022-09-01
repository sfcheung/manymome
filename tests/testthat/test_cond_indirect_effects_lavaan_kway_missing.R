# To be examined in an interactive session

library(manymome)
suppressMessages(suppressMessages(library(lavaan)))

dat <- data_med_mod_b_mod
dat <- add_na(dat, prop = .10, seed = 4589)
length(complete.cases(dat))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit <- sem(mod, dat, fixed.x = FALSE, warn = FALSE, missing = "fiml")
est <- parameterEstimates(fit)

set.seed(4456)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 5,
                warn = FALSE)

out_mm_1 <- mod_levels_list("w1", "w2", fit = fit, merge = TRUE)

# Suppress warnings due to small number of bootstrap samples.
suppressWarnings(out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit))
suppressWarnings(out_2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_x = TRUE))
suppressWarnings(out_3 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_y = TRUE))
suppressWarnings(out_4 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE))

suppressWarnings(out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit_boot,
                               boot_ci = TRUE))
suppressWarnings(out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit_boot,
                               standardized_x = TRUE,
                               boot_ci = TRUE))
suppressWarnings(out_7 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit_boot,
                               standardized_y = TRUE,
                               boot_ci = TRUE))
suppressWarnings(out_8 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m", fit = fit_boot,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, output_type = "list"))
