library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

test_that("q function: mediation with indicators: measurement model: standardized", {

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
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

mod <-
"
m ~ x10 + c2 + x12
y ~ m + x10 + c2 + x12
m =~ x04 + x05 + x09
c2 =~ x11 + x13 + x14
y =~ x01 + x02 + x03
"

fit <- sem(
  mod,
  data = data_sem,
  missing = "fiml"
)

est <- parameterEstimates(fit, standardized = TRUE)

est_chk <- out$lm_out_lav$m$coefs_lm
i <- (est$lhs == "m") & (est$op == "~")
est_i <- est[i, "est"]
names(est_i) <- est$rhs[i]
expect_equal(est_i,
             est_chk[names(est_i), "Estimate"],
             tolerance = 1e-4)

est_chk <- out$lm_out_lav$y$coefs_lm
i <- (est$lhs == "y") & (est$op == "~")
est_i <- est[i, "std.all"]
names(est_i) <- est$rhs[i]
expect_equal(est_i,
             est_chk[names(est_i), "betaS"],
             tolerance = 1e-4)

})
