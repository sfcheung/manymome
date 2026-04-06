skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

test_that("q function: mediation with indicators: measurement model", {
data_sem$x02r <- -data_sem$x02
data_sem$x10r <- -data_sem$x10
data_sem$x14r <- -data_sem$x14

out1 <- q_mediation(
          x = "f1",
          y = "f4",
          m = "f3",
          indicators = list(f1 = c("x01", "x02", "x03"),
                            f3 = c("x08", "x09", "x10"),
                            f4 = c("x11", "x12", "x13", "x14")),
          model = "simple",
          data = data_sem,
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

# Items incorrectly reversed
out2 <- q_mediation(
          x = "f1",
          y = "f4",
          m = "f3",
          indicators = list(f1 = c("x01", "-x02", "x03"),
                            f3 = c("x08", "x09", "-x10"),
                            f4 = c("x11", "x12", "x13", "-x14")),
          model = "simple",
          data = data_sem,
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

# Items incorrectly recoded
out3 <- q_mediation(
          x = "f1",
          y = "f4",
          m = "f3",
          indicators = list(f1 = c("x01", "x02r", "x03"),
                            f3 = c("x08", "x09", "x10r"),
                            f4 = c("x11", "x12", "x13", "x14r")),
          model = "simple",
          data = data_sem,
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

# Reverse items denoted as such
out4 <- q_mediation(
          x = "f1",
          y = "f4",
          m = "f3",
          indicators = list(f1 = c("x01", "-x02r", "x03"),
                            f3 = c("x08", "x09", "-x10r"),
                            f4 = c("x11", "x12", "x13", "-x14r")),
          model = "simple",
          data = data_sem,
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

expect_false(isTRUE(all.equal(
                as.numeric(out1$reliability),
                as.numeric(out2$reliability)
            )))

chk <- sapply(out2$loadings, function(x) x < 0)
expect_true(any(unlist(chk)))

chk <- lapply(out2$loadings, abs)
expect_equal(out1$loadings,
             chk,
             ignore_attr = TRUE)

expect_false(isTRUE(all.equal(
                as.numeric(out1$reliability),
                as.numeric(out3$reliability)
              )))

chk <- sapply(out3$loadings, function(x) x < 0)
expect_true(any(unlist(chk)))

chk <- lapply(out3$loadings, abs)
expect_equal(out1$loadings,
             chk,
             ignore_attr = TRUE)

expect_equal(as.numeric(out1$reliability),
             as.numeric(out4$reliability))
expect_equal(out1$loadings,
             out4$loadings,
             ignore_attr = TRUE)

expect_equal(coef(out1$ind_out$ustd),
             coef(out2$ind_out$ustd))
expect_equal(coef(out1$ind_out$ustd),
             coef(out3$ind_out$ustd))
expect_equal(coef(out1$ind_out$ustd),
             coef(out4$ind_out$ustd))

})
