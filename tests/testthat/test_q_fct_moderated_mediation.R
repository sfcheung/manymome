skip_on_cran()

library(testthat)
library(manymome)

# Test: Simple mediation

test_that("q function: simple moderated mediation", {
suppressWarnings(
out1 <- q_simple_mediation(x = "x",
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
expect_length(
  intersect(c("x:w", "w:x"),
            names(coef(out1$lm_out$m))),
  1
)
expect_length(
  intersect(c("x:w", "w:x"),
            names(coef(out1$lm_out$y))),
  1
)
})

# Test: Serial mediation

test_that("q function: moderated serial mediation", {
suppressWarnings(
out0 <- q_serial_mediation(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           cov = c("c2", "c1"),
                           moderators = list("x -> m1" = "w1",
                                             "m1 -> m2" = c("w1", "w2"),
                                             "x -> y" = "w2"),
                           data = data_med_mod_serial,
                           R = 50,
                           seed = 1234,
                           parallel = FALSE,
                           progress = !is_testing())
)
expect_length(
  intersect(c("x:w1", "1w:x"),
            names(coef(out0$lm_out$m1))),
  1
)
expect_length(
  intersect(c("m1:w1", "w1:m1"),
            names(coef(out0$lm_out$m2))),
  1
)
expect_length(
  intersect(c("m1:w2", "w2:m1"),
            names(coef(out0$lm_out$m2))),
  1
)
expect_length(
  intersect(c("x:w2", "w2:x"),
            names(coef(out0$lm_out$y))),
  1
)
})

# Test: Parallel mediation

test_that("q function: moderated parallel mediation", {
suppressWarnings(
out0 <- q_parallel_mediation(x = "x",
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
expect_length(
  intersect(c("x:w1", "w1:x"),
            names(coef(out0$lm_out$m2))),
  1
)
expect_length(
  intersect(c("m2:w2", "w2:m2"),
            names(coef(out0$lm_out$y))),
  1
)
expect_length(
  intersect(c("m1:w1", "w1:m1"),
            names(coef(out0$lm_out$y))),
  1
)
})

