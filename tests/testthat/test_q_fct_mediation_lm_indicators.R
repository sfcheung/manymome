library(testthat)
library(manymome)

# Test: Simple mediation

test_that("q function: mediation with indicators", {

data_sem2 <- data_sem
data_sem2[, c("y", "m", "c2")] <-
    scale_scores(
      list(y = c("-x01", "x02", "x03"),
          m = c("x04", "x05", "x09"),
          c2 = c("x11", "-x13", "x14")),
      data_sem2
    )

out <- q_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("-x01", "x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "-x13", "x14")),
          model = "simple",
          data = data_sem,
          R = 200,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

out_simple <- q_simple_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("-x01", "x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "-x13", "x14")),
          data = data_sem,
          R = 200,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

out_parallel <- q_parallel_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("-x01", "x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "-x13", "x14")),
          data = data_sem,
          R = 200,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

out_serial <- q_serial_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("-x01", "x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "-x13", "x14")),
          data = data_sem,
          R = 200,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

out_user <- q_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          model = c("x10 -> m -> y",
                    "x10 -> y"),
          indicators = list(y = c("-x01", "x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "-x13", "x14")),
          data = data_sem,
          R = 200,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

out_chk <- q_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          model = "simple",
          data = data_sem2,
          R = 200,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

expect_identical(out$ind_total,
                 out_chk$ind_total)
expect_identical(out$ind_out,
                 out_chk$ind_out)

expect_identical(out_simple$ind_total,
                 out_chk$ind_total)
expect_identical(out_simple$ind_out,
                 out_chk$ind_out)

expect_identical(out_parallel$ind_total,
                 out_chk$ind_total)
expect_identical(out_parallel$ind_out,
                 out_chk$ind_out)

expect_identical(out_serial$ind_total,
                 out_chk$ind_total)
expect_identical(out_serial$ind_out,
                 out_chk$ind_out)

expect_identical(out_user$ind_total,
                 out_chk$ind_total)
expect_identical(out_user$ind_out,
                 out_chk$ind_out)

out_rel <- scale_reliability(
  indicators =  out_user$indicators,
  data = data_sem
)

expect_equal(
  out_simple$reliability[names(out_rel$full_output)],
  out_rel$full_output
)

expect_equal(
  out_simple$loadings[names(out_rel$full_output)],
  out_rel$loadings
)


})
