skip_on_cran()

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

out_jn <- pseudo_johnson_neyman(out1)
out_jn1 <- pseudo_johnson_neyman(out1$cond_ind_out$ustd[[1]])

expect_equal(
  coef(out_jn$cond_effects),
  coef(out_jn1$cond_effects)
)

out_jn <- pseudo_johnson_neyman(out1, which = "stdxy")
out_jn1 <- pseudo_johnson_neyman(out1$cond_ind_out$stdxy[[1]])

expect_equal(
  coef(out_jn$cond_effects),
  coef(out_jn1$cond_effects)
)

expect_no_error(plot_effect_vs_w(out1))

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

out_jn <- pseudo_johnson_neyman(out0, m = c("m1", "m2"))
out_jn1 <- pseudo_johnson_neyman(out0$cond_ind_out$ustd[["x -> m1 -> m2 -> y"]])

expect_equal(
  coef(out_jn$cond_effects),
  coef(out_jn1$cond_effects)
)

out_jn <- pseudo_johnson_neyman(out0, which = "stdx", m = "m1")
out_jn1 <- pseudo_johnson_neyman(out0$cond_ind_out$stdx[["x -> m1 -> y"]])

expect_equal(
  coef(out_jn$cond_effects),
  coef(out_jn1$cond_effects)
)

expect_no_error(plot_effect_vs_w(out0, which = "stdx", m = "m1"))

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

out_jn <- pseudo_johnson_neyman(out0, m = c("m1"))
out_jn1 <- pseudo_johnson_neyman(out0$cond_ind_out$ustd[["x -> m1 -> y"]])

expect_equal(
  coef(out_jn$cond_effects),
  coef(out_jn1$cond_effects)
)

out_jn <- pseudo_johnson_neyman(out0, which = "stdy", m = "m1")
out_jn1 <- pseudo_johnson_neyman(out0$cond_ind_out$stdy[["x -> m1 -> y"]])

expect_equal(
  coef(out_jn$cond_effects),
  coef(out_jn1$cond_effects)
)

expect_no_error(plot_effect_vs_w(out0, which = "stdy", m = "m1"))


})

