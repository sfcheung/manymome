library(testthat)
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
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 6:7]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

# Moderated mediation

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)
out_mm_2 <- mod_levels_list("w1", fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)
out_2 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m1", fit = fit)

out_1b <- cond_indirect_effects(wlevels = list("w4", c("gpgp2", "gpgp3")), x = "x", y = "y", m = "m3", fit = fit)
out_2b <- cond_indirect_effects(wlevels = "w1", x = "x", y = "m1", fit = fit)

test_that("cond_indirect_effects: call mod_levels_list (lm)", {
    expect_equal(unlist(out_1), unlist(out_1b))
    expect_equal(unlist(out_2), unlist(out_2b))
  })

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE, w_method = "percentile")
out_mm_2 <- mod_levels_list("w1", fit = fit, merge = TRUE, w_method = "percentile")

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)
out_2 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m1", fit = fit)

out_1b <- cond_indirect_effects(wlevels = list("w4", c("gpgp2", "gpgp3")), x = "x", y = "y", m = "m3", fit = fit, w_method = "percentile")
out_2b <- cond_indirect_effects(wlevels = "w1", x = "x", y = "m1", fit = fit, w_method = "percentile")

test_that("cond_indirect_effects: call mod_levels_list (lm), percentile", {
    expect_equal(unlist(out_1), unlist(out_1b))
    expect_equal(unlist(out_2), unlist(out_2b))
  })

