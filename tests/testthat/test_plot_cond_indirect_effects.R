# To be examined in an interactive session.

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

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
lm_m3 <- lm(m3 ~ m1 + x*gp, dat)
lm_y <- lm(y ~ m2 + m3 + x*w4, dat)
fit_lm <- lm2list(lm_m3, lm_y)

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"),
                            sd_from_mean = c(-1, 1),
                            fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)

test_that("plot for indirect", {
  expect_error(plot(out_1))
  expect_error(plot(out_1, graph_type = "tumble"))
})


out_mm_1_lm <- mod_levels_list("w4", c("gpgp2", "gpgp3"),
                            sd_from_mean = c(-1, 1),
                            fit = fit_lm, merge = TRUE)


out_1_lm <- cond_indirect_effects(wlevels = out_mm_1_lm, x = "x", y = "y", m = "m3", fit = fit_lm)

test_that("plot for indirect", {
  expect_error(plot(out_1_lm))
  expect_error(plot(out_1_lm, graph_type = "tumble"))
})

out_1_std <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                                   standardized_x = TRUE, standardized_y = TRUE)

test_that("plot for indirect", {
  expect_error(plot(out_1_std))
  expect_error(plot(out_1_std, graph_type = "tumble"))
})

out_1_stdx <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                                   standardized_x = TRUE)

test_that("plot for indirect", {
  expect_error(plot(out_1_stdx))
  expect_error(plot(out_1_stdx, graph_type = "tumble"))
})

out_1_stdy <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                                   standardized_y = TRUE)

test_that("plot for indirect", {
  expect_error(plot(out_1_stdy))
  expect_error(plot(out_1_stdy, graph_type = "tumble"))
})

out_mm_1_lm <- mod_levels_list("w4", c("gpgp2", "gpgp3"),
                            sd_from_mean = c(-1, 1),
                            fit = fit_lm, merge = TRUE)


out_1_lm_std <- cond_indirect_effects(wlevels = out_mm_1_lm, x = "x", y = "y", m = "m3", fit = fit_lm,
                                  standardized_x = TRUE, standardized_y = TRUE)

test_that("plot for indirect", {
  expect_error(plot(out_1_lm_std))
  expect_error(plot(out_1_lm_std, graph_type = "tumble"))
})

out_1_lm_stdx <- cond_indirect_effects(wlevels = out_mm_1_lm, x = "x", y = "y", m = "m3", fit = fit_lm,
                                  standardized_x = TRUE)

test_that("plot for indirect", {
  expect_error(plot(out_1_lm_stdx))
  expect_error(plot(out_1_lm_stdx, graph_type = "tumble"))
})

out_1_lm_stdy <- cond_indirect_effects(wlevels = out_mm_1_lm, x = "x", y = "y", m = "m3", fit = fit_lm,
                                  standardized_y = TRUE)

test_that("plot for indirect", {
  expect_error(plot(out_1_lm_stdy))
  expect_error(plot(out_1_lm_stdy, graph_type = "tumble"))
})


mod_mod <-
"
m1 ~ x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
"
fit_mod <- sem(mod_mod, dat, meanstructure = TRUE, fixed.x = FALSE)
out_mm_mod <- mod_levels_list(c("gpgp2", "gpgp3"),
                              fit = fit_mod, merge = TRUE)
out_mod_mod <- cond_indirect_effects(wlevels = list(c("gpgp2", "gpgp3")),
                                  x = "x", y = "m1",
                                  fit = fit_mod)

#plot(out_mod_mod)

out_mod_mod <- cond_indirect_effects(wlevels = list(c("gpgp2", "gpgp3")),
                                  x = "x", y = "m1",
                                  fit = fit_mod,
                                  standardized_x = TRUE)
#plot(out_mod_mod)
