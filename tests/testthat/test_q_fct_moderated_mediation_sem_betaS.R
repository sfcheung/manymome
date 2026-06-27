skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple moderated mediation: sem", {
suppressWarnings(
out1 <- q_simple_mediation(
  x = "x",
  y = "y",
  m = "m",
  cov = list(
          y = c("c2", "c1"),
          m = "c2"
        ),
  moderators = list(
          "x -> m" = "w1",
          "m -> y" = "w2"
        ),
  data = data_med_mod_b_mod,
  R = 50,
  seed = 1234,
  fit_method = "sem",
  parallel = FALSE,
  progress = !is_testing())
)

# ==== Check betaS ====

names(out1)
fit <- out1$lm_out
est <- parameterEstimates(fit)
fit_sd <- sqrt(diag(lavInspect(fit, "implied")$cov))
est_xw <- est[(est$op == "~") &
              (grepl(":", est$rhs, fixed = TRUE)), ]
tmp <- strsplit(est_xw$rhs, ":")
est_xw$x <- sapply(tmp, function(x) x[1])
est_xw$w <- sapply(tmp, function(x) x[2])

est_xw$std.prod <- NA_real_

for (i in seq_len(nrow(est_xw))) {
  est_xw[i, "std.prod"] <- est_xw[i, "est"] *
    fit_sd[est_xw[i, "x"]] * fit_sd[est_xw[i, "w"]] /
    fit_sd[est_xw[i, "lhs"]]
}

expect_equal(
  out1$lm_out_lav$m$coefs["x:w1", "std.prod"],
  est_xw[est_xw$lhs == "m", "std.prod"]
)
expect_equal(
  out1$lm_out_lav$y$coefs["m:w2", "std.prod"],
  est_xw[est_xw$lhs == "y", "std.prod"]
)

suppressWarnings(
out2 <- q_simple_mediation(
  x = "x",
  y = "y",
  m = "m",
  cov = list(
          y = c("c2", "c1"),
          m = "c2"
        ),
  moderators = list(
          "x -> m" = "w1",
          "m -> y" = "w2"
        ),
  data = data_med_mod_b_mod,
  R = 50,
  seed = 1234,
  fit_method = "lm",
  parallel = FALSE,
  progress = !is_testing())
)

names(out2)
lm_summary <- summary(out2$lm_out, betaselect = TRUE)

expect_equal(
  out1$lm_out_lav$m$coefs["x:w1", "std.prod"],
  lm_summary$m$coefficients["x:w1", "betaS"],
  tolerance = 1e-2
)
expect_equal(
  out1$lm_out_lav$y$coefs["m:w2", "std.prod"],
  lm_summary$y$coefficients["m:w2", "betaS"],
  tolerance = 1e-2
)

})

skip("WIP")

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

