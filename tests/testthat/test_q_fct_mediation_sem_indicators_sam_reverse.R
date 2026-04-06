skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

test_that("q function: mediation with indicators: SAM", {

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
          indicator_method = "sam",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())
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
          indicator_method = "sam",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())
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
          indicator_method = "sam",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())
mod <-
'
f1 =~ x01 + x02 + x03
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~  f1
f4 ~  f3
'
fit <- sam(model = mod, data = data_sem)
parameterEstimates(fit, standardized = TRUE)
standardizedSolution(fit)

expect_equal(as.numeric(out1$reliability),
             as.numeric(out2$reliability))

expect_equal(out1$loadings,
             out2$loadings)

expect_equal(as.numeric(out1$reliability),
             as.numeric(out3$reliability))

chk <- lapply(out3$loadings, abs)
expect_equal(out1$loadings,
             chk,
             ignore_attr = TRUE)

chk <- sapply(out3$loadings, function(x) x < 0)

expect_true(any(unlist(chk)))

expect_equal(coef(out1$ind_out$ustd),
             coef(out2$ind_out$ustd))
expect_equal(coef(out1$ind_out$ustd),
             coef(out3$ind_out$ustd))

})
