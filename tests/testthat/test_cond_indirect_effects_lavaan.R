skip("To be examined in an interactive session")

library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
set.seed(4456)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 5,
                warn = FALSE)

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

# Suppress warnings due to small number of bootstrap samples.
suppressWarnings(out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit))
suppressWarnings(out_2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE))
suppressWarnings(out_3 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_y = TRUE))
suppressWarnings(out_4 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE))

suppressWarnings(out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               boot_ci = TRUE))
suppressWarnings(out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               standardized_x = TRUE,
                               boot_ci = TRUE))
suppressWarnings(out_7 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               standardized_y = TRUE,
                               boot_ci = TRUE))
suppressWarnings(out_8 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, output_type = "list"))

# Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

# Suppress warnings due to small number of bootstrap samples.
suppressWarnings(outmo_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit))
suppressWarnings(outmo_2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE))
suppressWarnings(outmo_3 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE))
suppressWarnings(outmo_4 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE))

suppressWarnings(outmo_5 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_boot,
                               boot_ci = TRUE, seed = 87415))
fit_boot_out <- fit2boot_out(fit_boot)
suppressWarnings(outmo_6 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out))
suppressWarnings(outmo_7 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out))
suppressWarnings(outmo_8 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out))

# Monte Carlo

## Mediation

fit_ml <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", warn = FALSE)
fit_mc_out <- do_mc(fit_ml, R = 100, seed = 5155)

# Suppress warnings due to small number of bootstrap samples.
suppressWarnings(out_5_mc <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_ml,
                               mc_ci = TRUE, mc_out = fit_mc_out))
suppressWarnings(out_6_mc <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_ml,
                               standardized_x = TRUE,
                               mc_ci = TRUE, mc_out = fit_mc_out))
suppressWarnings(out_7_mc <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_ml,
                               standardized_y = TRUE,
                               mc_ci = TRUE, mc_out = fit_mc_out))
suppressWarnings(out_8_mc <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_ml,
                               standardized_x = TRUE, standardized_y = TRUE,
                               mc_ci = TRUE, mc_out = fit_mc_out, output_type = "list"))

## Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

# Suppress warnings due to small number of bootstrap samples.

suppressWarnings(outmo_5_mc <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_ml,
                               mc_ci = TRUE, mc_out = fit_mc_out))
suppressWarnings(outmo_6_mc <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_ml,
                               standardized_x = TRUE,
                               mc_ci = TRUE, mc_out = fit_mc_out))
suppressWarnings(outmo_7_mc <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_ml,
                               standardized_y = TRUE,
                               mc_ci = TRUE, mc_out = fit_mc_out))
suppressWarnings(outmo_8_mc <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_ml,
                               standardized_x = TRUE, standardized_y = TRUE,
                               mc_ci = TRUE, mc_out = fit_mc_out))


# ci_type

## Mediation

fit_ml <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", warn = FALSE)
fit_mc_out <- do_mc(fit_ml, R = 100, seed = 5155)

# Suppress warnings due to small number of bootstrap samples.
suppressWarnings(out_5_mc2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_ml,
                               ci_type = "mc", mc_out = fit_mc_out))
suppressWarnings(out_6_boot2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               standardized_x = TRUE,
                               ci_type = "boot"))
suppressWarnings(out_7_mc2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_ml,
                               standardized_y = TRUE,
                               ci_type = "mc", ci_out = fit_mc_out))
suppressWarnings(out_8_boot2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               standardized_x = TRUE, standardized_y = TRUE,
                               ci_type = "boot"))

test_that("cond_indirect_effects: ci_type", {
    expect_equal(out_5_mc$mc_ci, out_5_mc2$mc_ci)
    expect_equal(out_6$boot_ci, out_6_boot2$boot_ci)
    expect_equal(out_7_mc$mc_ci, out_7_mc2$mc_ci)
    expect_equal(out_8$boot_ci, out_8_boot2$boot_ci)
  })

## Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

# Suppress warnings due to small number of bootstrap samples.

suppressWarnings(outmo_5_boot2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_boot,
                               ci_type = "boot"))
suppressWarnings(outmo_6_mc2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_ml,
                               standardized_x = TRUE,
                               ci_type = "mc", ci_out = fit_mc_out))
suppressWarnings(outmo_7_boot2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_boot,
                               standardized_y = TRUE,
                               ci_type = "boot"))
suppressWarnings(outmo_8_mc2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit_ml,
                               standardized_x = TRUE, standardized_y = TRUE,
                               ci_type = "mc", mc_out = fit_mc_out))

test_that("cond_indirect_effects: moderation, ci_type", {
    expect_equal(outmo_5$boot_ci, outmo_5_boot2$boot_ci)
    expect_equal(outmo_6_mc$mc_ci, outmo_6_mc2$mc_ci)
    expect_equal(outmo_7$boot_ci, outmo_7_boot2$boot_ci)
    expect_equal(outmo_8_mc$mc_ci, outmo_8_mc2$mc_ci)
  })



