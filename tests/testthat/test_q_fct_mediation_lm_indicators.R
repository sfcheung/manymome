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

})
