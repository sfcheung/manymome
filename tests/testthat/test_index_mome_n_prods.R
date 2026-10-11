skip_on_cran()
# This test is long and should be tested locally.
library(testthat)

test_that("index: Fix problem with some models", {

library(manymome)
library(lmhelprs)


set.seed(4321)
n <- 50
x <- rnorm(n)
w <- runif(n, -1.73, 1.73)
z <- runif(n, -1.73, 1.73)
m <- (.2 + .2 * w + .3 * z) * x + rnorm(n, 0, .5)
y <- (.3 + .3 * w + .3 * z) * x + .4 * m + rnorm(n, 0, .5)
dat <- data.frame(x, m, w, z, y)

mod_8 <-
"
m ~ x*w
y ~ x*w + m
"
fit_8 <- many_lm(
  mod_8,
  dat
)

out_ind_8 <- index_of_mome(
  w = "w",
  x = "x",
  m = "m",
  y = "y",
  fit = fit_8
)

mod_w_8 <- mod_levels(
  w = "w",
  fit = fit_8,
  values = c(-1, 0, 1)
)
out_8 <- cond_indirect_effects(
  wlevels = mod_w_8,
  x = "x",
  m = "m",
  y = "y",
  fit = fit_8
)

out_diff_8 <- cond_indirect_diff(
  out_8,
  from = 2,
  to = 1
)

expect_equal(
  coef(out_ind_8),
  coef(out_diff_8)
)

mod_15 <-
"
m ~ x
y ~ x*w + m*w
"
fit_15 <- many_lm(
  mod_15,
  dat
)

out_ind_15 <- index_of_mome(
  w = "w",
  x = "x",
  m = "m",
  y = "y",
  fit = fit_15
)
mod_w_15 <- mod_levels(
  w = "w",
  fit = fit_15,
  values = c(-1, 0, 1)
)
out_15 <- cond_indirect_effects(
  wlevels = mod_w_15,
  x = "x",
  m = "m",
  y = "y",
  fit = fit_15
)

out_diff_15 <- cond_indirect_diff(
  out_15,
  from = 2,
  to = 1
)

expect_equal(
  coef(out_ind_15),
  coef(out_diff_15)
)

mod_a <-
"
m ~ x*w
y ~ x*w + x*z + m*z
"
fit_a <- many_lm(
  mod_a,
  dat
)

out_ind_a <- index_of_momome(
  w = "w",
  z = "z",
  x = "x",
  m = "m",
  y = "y",
  fit = fit_a
)

mod_w_a <- mod_levels(
  w = "w",
  fit = fit_a,
  values = c(-1, 0, 1)
)
mod_z_a <- mod_levels(
  w = "z",
  fit = fit_a,
  values = c(-1, 0, 1)
)
mod_a <- merge_mod_levels(
  mod_w_a,
  mod_z_a
)
out_a <- cond_indirect_effects(
  wlevels = mod_a,
  x = "x",
  m = "m",
  y = "y",
  fit = fit_a
)

est <- coef(out_a)
out_diff_a <- sum(est[c(1, 2, 4, 5)] * c(1, -1, -1, 1))

expect_equal(
  coef(out_ind_a),
  out_diff_a,
  ignore_attr = TRUE
)

mod_b <-
"
m ~ x*w
y ~ x*w + m*w + m*z
"
fit_b <- many_lm(
  mod_b,
  dat
)

expect_error(index_of_momome(
  w = "w",
  z = "z",
  x = "x",
  m = "m",
  y = "y",
  fit = fit_b
))



})