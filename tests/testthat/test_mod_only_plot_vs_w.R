skip_on_cran()
# The plots should be examined in an interactive session

library(testthat)
library(manymome)

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm1_list <- lm2list(lm_m, lm_y)
lm2_list <- lm2list(lm_m)
lm3_list <- lm2list(lm_y)
lm1fit <- lm2ptable(lm1_list)
lm2fit <- lm2ptable(lm2_list)
lm3fit <- lm2ptable(lm3_list)


suppressMessages(suppressMessages(library(lavaan)))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod1 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
mod2 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
# Covariances not added. Not necessary for this test.
"
mod3 <-
"
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit1 <- sem(mod1, dat, fixed.x = FALSE, warn = FALSE)
fit2 <- sem(mod2, dat, fixed.x = FALSE, warn = FALSE)
fit3 <- sem(mod3, dat, fixed.x = FALSE, warn = FALSE)


cos_1a <- cond_indirect_effects(wlevels = "w1",
                                x = "x", y = "m",
                                fit = lm1_list)
cos_1b <- cond_indirect_effects(wlevels = c("w1", "w2"),
                                x = "m", y = "y",
                                fit = lm1_list)
cos_2 <- cond_indirect_effects(wlevels = "w1",
                               x = "x", y = "m",
                               fit = lm2_list)
cos_3 <- cond_indirect_effects(wlevels = c("w1", "w2"),
                               x = "m", y = "y",
                               fit = lm3_list)

sos_1a <- cond_indirect_effects(wlevels = "w1",
                                x = "x", y = "m",
                                fit = fit1)
sos_1b <- cond_indirect_effects(wlevels = c("w1", "w2"),
                                x = "m", y = "y",
                                fit = fit1)
sos_2 <- cond_indirect_effects(wlevels = "w1",
                               x = "x", y = "m",
                               fit = fit2)
sos_3 <- cond_indirect_effects(wlevels = c("w1", "w2"),
                               x = "m", y = "y",
                               fit = fit3)
test_that("mod only", {
expect_no_error(plot_effect_vs_w(cos_1a, level = .99) + ggplot2::ylim(0, 1))
expect_no_error(plot_effect_vs_w(cos_1a) + ggplot2::ylim(0, 1))
expect_no_error(plot_effect_vs_w(cos_1a, level = .60) + ggplot2::ylim(0, 1))
expect_no_error(plot_effect_vs_w(cos_1a, level = .50) + ggplot2::ylim(0, 1))
})
