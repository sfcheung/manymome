skip_on_cran()
# This test is long and should be tested locally.
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Test: Simple mediation

test_that("q function: simple moderated mediation", {
suppressWarnings(
out1 <- q_simple_mediation(
  x = "x",
  y = "y",
  m = "m",
  cov = list(y = c("c2", "c1"),
            m = "c2"),
  moderators = list("x -> m" = "w",
                    "x -> y" = "w"),
  data = data_med_mod_ab1,
  R = 50,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_mome(
  m = "m",
  w = "w",
  fit = out1
)

out_index_chk <- index_of_mome(
  y = "y",
  x = "x",
  m = "m",
  w = "w",
  fit = out1$lm_out,
  ci_type = "boot",
  boot_out = attr(out1$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

out_index <- z_index_of_mome(
  m = "m",
  w = "w",
  fit = out1
)

out_index_chk <- z_index_of_mome(
  y = "y",
  x = "x",
  m = "m",
  w = "w",
  fit = out1$lm_out,
  ci_type = "boot",
  boot_out = attr(out1$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})

# Test: Serial mediation

test_that("q function: moderated serial mediation", {
suppressWarnings(
out0 <- q_serial_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = list("x -> m1" = "w1",
                    "x -> y" = "w2"),
  data = data_med_mod_serial,
  R = 50,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_mome(
  m = c("m1", "m2"),
  w = "w1",
  fit = out0
)

out_index_chk <- index_of_mome(
  y = "y",
  x = "x",
  m = c("m1", "m2"),
  w = "w1",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

out_index <- z_index_of_mome(
  m = c("m1", "m2"),
  w = "w1",
  fit = out0
)

out_index_chk <- z_index_of_mome(
  y = "y",
  x = "x",
  m = c("m1", "m2"),
  w = "w1",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})

# Test: Parallel mediation

test_that("q function: moderated parallel mediation", {
suppressWarnings(
out0 <- q_parallel_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = c("x->m2" = "w1",
                  "m2-> y" = "w2",
                  "m1->y" = "w1"),
  data = data_med_mod_parallel,
  R = 50,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_mome(
  m = c("m1"),
  w = "w1",
  fit = out0
)

out_index_chk <- index_of_mome(
  y = "y",
  x = "x",
  m = c("m1"),
  w = "w1",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

expect_error(
index_of_mome(
  m = c("m2"),
  w = "w1",
  fit = out0
)
)

out_index <- z_index_of_mome(
  m = c("m1"),
  w = "w1",
  fit = out0
)

out_index_chk <- z_index_of_mome(
  y = "y",
  x = "x",
  m = c("m1"),
  w = "w1",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})


# Test: Simple mediation

test_that("q function: simple moderated mediation: Two moderators", {
suppressWarnings(
out1 <- q_simple_mediation(
  x = "x",
  y = "y",
  m = "m",
  cov = list(y = c("c2", "c1")),
  moderators = list("x -> m" = "w",
                    "m -> y" = "c1"),
  data = data_med_mod_ab1,
  R = 50,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_momome(
  m = "m",
  w = "w",
  z = "c1",
  fit = out1
)

out_index_chk <- index_of_momome(
  y = "y",
  x = "x",
  m = "m",
  w = "w",
  z = "c1",
  fit = out1$lm_out,
  ci_type = "boot",
  boot_out = attr(out1$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

out_index <- z_index_of_momome(
  m = "m",
  w = "w",
  z = "c1",
  fit = out1
)

out_index_chk <- z_index_of_momome(
  y = "y",
  x = "x",
  m = "m",
  w = "w",
  z = "c1",
  fit = out1$lm_out,
  ci_type = "boot",
  boot_out = attr(out1$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})

# Test: Serial mediation

test_that("q function: moderated serial mediation: Two moderators", {
suppressWarnings(
out0 <- q_serial_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = list("x -> m1" = "w1",
                    "m1 -> m2" = "w2"),
  data = data_med_mod_serial,
  R = 50,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_momome(
  m = c("m1", "m2"),
  w = "w1",
  z = "w2",
  fit = out0
)

out_index_chk <- index_of_momome(
  y = "y",
  x = "x",
  m = c("m1", "m2"),
  w = "w1",
  z = "w2",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

out_index <- z_index_of_momome(
  m = c("m1", "m2"),
  w = "w1",
  z = "w2",
  fit = out0
)

out_index_chk <- z_index_of_momome(
  y = "y",
  x = "x",
  m = c("m1", "m2"),
  w = "w1",
  z = "w2",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})

# Test: Parallel mediation

test_that("q function: moderated parallel mediation: Two moderators", {
suppressWarnings(
out0 <- q_parallel_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = c("x->m2" = "w1",
                  "m2-> y" = "w2",
                  "m1->y" = "w1"),
  data = data_med_mod_parallel,
  R = 50,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_momome(
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0
)

out_index_chk <- index_of_momome(
  y = "y",
  x = "x",
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

expect_error(
index_of_momome(
  m = c("m2"),
  w = c("w1", "w2"),
  fit = out0
)
)

out_index <- z_index_of_momome(
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0
)

out_index_chk <- z_index_of_momome(
  y = "y",
  x = "x",
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0$lm_out,
  ci_type = "boot",
  boot_out = attr(out0$cond_ind_out$ustd[[1]], "boot_out")
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})

test_that("q function: moderated parallel mediation: No bootstrap", {
suppressWarnings(
out0 <- q_parallel_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = c("x->m2" = "w1",
                  "m2-> y" = "w2",
                  "m1->y" = "w1"),
  data = data_med_mod_parallel,
  boot_ci = FALSE,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
)

out_index <- index_of_momome(
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0
)

out_index_chk <- index_of_momome(
  y = "y",
  x = "x",
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0$lm_out,
  boot_ci = FALSE
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)

out_index <- z_index_of_momome(
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0
)

out_index_chk <- z_index_of_momome(
  y = "y",
  x = "x",
  m = c("m2"),
  w = "w1",
  z = "w2",
  fit = out0$lm_out,
  boot_ci = FALSE
)

expect_equal(
  coef(out_index),
  coef(out_index_chk)
)

expect_equal(
  confint(out_index),
  confint(out_index_chk)
)


})
