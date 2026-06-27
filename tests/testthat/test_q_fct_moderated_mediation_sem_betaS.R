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
  est_xw[est_xw$lhs == "m", "std.prod"],
  tolerance = 1e-4
)
expect_equal(
  out1$lm_out_lav$y$coefs["m:w2", "std.prod"],
  est_xw[est_xw$lhs == "y", "std.prod"],
  tolerance = 1e-4
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

expect_output(
  print(out1),
  "product terms formed after"
)

})

# Test: Serial mediation

test_that("q function: moderated serial mediation: sem", {
suppressWarnings(
out0 <- q_serial_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = list("x -> m1" = "w1",
                    "m1 -> m2" = c("w1", "w2"),
                    "x -> y" = "w2"),
  data = data_med_mod_serial,
  R = 50,
  seed = 1234,
  fit_method = "sem",
  parallel = FALSE,
  progress = !is_testing())
)

expect_length(
  intersect(c("x:w1", "w1:x"),
            rownames(out0$lm_out_lav$m1$coefs_lm)),
  1
)
expect_length(
  intersect(c("m1:w1", "w1:m1"),
            rownames(out0$lm_out_lav$m2$coefs_lm)),
  1
)
expect_length(
  intersect(c("m1:w2", "w2:m1"),
            rownames(out0$lm_out_lav$m2$coefs_lm)),
  1
)

suppressWarnings(
out2 <- q_serial_mediation(
  x = "x",
  y = "y",
  m = c("m1", "m2"),
  cov = c("c2", "c1"),
  moderators = list("x -> m1" = "w1",
                    "m1 -> m2" = c("w1", "w2"),
                    "x -> y" = "w2"),
  data = data_med_mod_serial,
  R = 50,
  seed = 1234,
  fit_method = "lm",
  parallel = FALSE,
  progress = !is_testing())
)

lm_summary <- summary(out2$lm_out, betaselect = TRUE)

expect_equal(
  out0$lm_out_lav$m2$coefs["m1:w1", "std.prod"],
  lm_summary$m2$coefficients["m1:w1", "betaS"],
  tolerance = 1e-2
)
expect_equal(
  out0$lm_out_lav$m2$coefs["m1:w2", "std.prod"],
  lm_summary$m2$coefficients["m1:w2", "betaS"],
  tolerance = 1e-2
)
expect_equal(
  out0$lm_out_lav$y$coefs["x:w2", "std.prod"],
  lm_summary$y$coefficients["x:w2", "betaS"],
  tolerance = 1e-2
)

expect_output(
  print(out0),
  "product terms formed after"
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
  fit_method = "sem",
  parallel = FALSE,
  progress = !is_testing()
)
)

# ==== Check betaS ====

fit <- out0$lm_out
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
  out0$lm_out_lav$m2$coefs["x:w1", "std.prod"],
  est_xw[est_xw$lhs == "m2", "std.prod"],
  tolerance = 1e-4
)
expect_equal(
  out0$lm_out_lav$y$coefs["m2:w2", "std.prod"],
  est_xw[(est_xw$lhs == "y") &
         (est_xw$rhs == "m2:w2"), "std.prod"],
  tolerance = 1e-4
)

# Check SDs

# fit_sd
# dat_sd <- apply(
#   data_med_mod_parallel,
#   MARGIN = 2,
#   sd
# )
# fit_sd[names(dat_sd)]
# dat_sd

suppressWarnings(
out2 <- q_parallel_mediation(
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
  fit_method = "lm",
  parallel = FALSE,
  progress = !is_testing()
)
)

lm_summary <- summary(out2$lm_out, betaselect = TRUE)

expect_equal(
  out0$lm_out_lav$m2$coefs["x:w1", "std.prod"],
  lm_summary$m2$coefficients["x:w1", "betaS"],
  tolerance = 1e-2
)
# Difference in standardized product term is due to difference in SDs
expect_equal(
  out0$lm_out_lav$y$coefs["m2:w2", "est"],
  lm_summary$y$coefficients["m2:w2", "Estimate"],
  tolerance = 1e-2
)
expect_equal(
  out0$lm_out_lav$y$coefs["m1:w1", "est"],
  lm_summary$y$coefficients["m1:w1", "Estimate"],
  tolerance = 1e-2
)

expect_output(
  print(out0),
  "product terms formed after"
)

})

