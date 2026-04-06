library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("q function: mediation with indicators: SAM: Standardized", {

# ==== q function: mediation with indicators: SAM ====

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
          fit_method = "sem",
          indicator_method = "sam",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

mod <-
"
m ~ a*x10 + c2 + x12
y ~ b*m + x10 + c2 + x12
m =~ x04 + x05 + x09
c2 =~ x11 + x13 + x14
y =~ x01 + x02 + x03
ab := a*b
"

fit <- sam(
  mod,
  data = data_sem
)

ind <- indirect_effect(
  x = "x10",
  y = "y",
  m = "m",
  fit = fit)

indstdxy <- indirect_effect(
  x = "x10",
  y = "y",
  m = "m",
  fit = fit,
  standardized_x = TRUE,
  standardized_y = TRUE)

est <- parameterEstimates(fit, standardized = TRUE)
ind_std_est <- est[est$label == "ab", "std.all"]

expect_identical(coef(out$ind_out$stdxy),
                 coef(indstdxy),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

expect_identical(coef(out$ind_out$stdxy),
                 ind_std_est,
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

})
