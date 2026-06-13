skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

test_that("q function: model:: moderated", {

suppressWarnings(
out1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m1 -> m2 -> y1",
                      "x1 -> m1 -> m3 -> y1"),
            cov = c("c1", "c2"),
            moderators = list(
              "x1 -> m1" = "w1",
              "m1 -> m3" = "w2",
              "m1 -> m3" = "w1",
              "m2 -> y1" = "w2"
            ),
            data = data_mome_demo,
            R = 50,
            seed = 1234,
            ci_type = "boot",
            fit_method = "lm",
            parallel = FALSE,
            progress = !is_testing()
          )
)

expect_length(
  intersect(c("x1:w1", "w1:x1"),
            names(coef(out1$lm_out$m1))),
  1
)
expect_length(
  intersect(c("m1:w1", "w1:m1"),
            names(coef(out1$lm_out$m3))),
  1
)
expect_length(
  intersect(c("m1:w2", "w2:m1"),
            names(coef(out1$lm_out$m3))),
  1
)
expect_length(
  intersect(c("m2:w2", "w2:m2"),
            names(coef(out1$lm_out$y1))),
  1
)

})
