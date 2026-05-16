skip_on_cran()

library(testthat)
library(manymome)

test_that("q function: get_fit", {

out1a <- q_simple_mediation(
  x = "x",
  y = "y",
  m = "m",
  cov = c("c2", "c1"),
  data = data_med,
  R = 200,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing()
)
out1b <- q_simple_mediation(
  x = "x",
  y = "y",
  m = "m",
  cov = c("c2", "c1"),
  data = data_med,
  R = 200,
  seed = 1234,
  fit_method = "sem",
  parallel = FALSE,
  progress = !is_testing()
)

tmp <- get_fit(out1a)
expect_s3_class(
  tmp,
  "lm_list"
)

tmp <- get_fit(out1b)
expect_s4_class(
  tmp,
  "lavaan"
)

})

