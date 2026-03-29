library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

test_that("q function: mediation with indicators: measurement model", {

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

out_simple <- q_simple_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("-x01", "x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "-x13", "x14")),
          data = data_sem,
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          # R = 100,
          # seed = 1234,
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
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          # R = 100,
          # seed = 1234,
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
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          # R = 100,
          # seed = 1234,
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
          fit_method = "sem",
          indicator_method = "measurement_model",
          boot_ci = FALSE,
          # R = 100,
          # seed = 1234,
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
  data = data_sem
)

ind <- indirect_effect(
  x = "x10",
  y = "y",
  m = "m",
  fit = fit)

expect_identical(coef(out$ind_out$ustd),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_simple$ind_out$ustd),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_parallel$ind_out$ustd),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_serial$ind_out$ustd),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_user$ind_out$ustd),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

indstdxy <- indirect_effect(
  x = "x10",
  y = "y",
  m = "m",
  fit = fit,
  standardized_x = TRUE,
  standardized_y = TRUE)

expect_identical(coef(out$ind_out$stdxy),
                 coef(indstdxy),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_simple$ind_out$stdxy),
                 coef(indstdxy),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_parallel$ind_out$stdxy),
                 coef(indstdxy),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_serial$ind_out$stdxy),
                 coef(indstdxy),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)
expect_identical(coef(out_user$ind_out$stdxy),
                 coef(indstdxy),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

# Reliability

mod_cfa <-
"
m =~ x04 + x05 + x09
c2 =~ x11 + x13 + x14
y =~ x01 + x02 + x03
"

fit_cfa <- cfa(
  mod_cfa,
  data = data_sem,
  std.lv = TRUE
)

fit_rel <- compRelSEM(
  fit_cfa
)

expect_equal(out_simple$reliability[names(fit_rel)],
             fit_rel)

expect_equal(out_simple$loadings[names(fit_rel)],
             get_loadings(fit_cfa))

})

test_that("q function: mediation with indicators: measurement model: boot_ci", {

skip_on_cran()

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
  data = data_sem
)

ind <- indirect_effect(
  x = "x10",
  y = "y",
  m = "m",
  fit = fit)

expect_identical(coef(out$ind_out$ustd),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

})
