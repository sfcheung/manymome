library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("q function: mediation with indicators: SAM", {

# ==== q function: mediation with indicators: SAM ====

data_sem_rev <- data_sem
data_sem_rev$x02 <- -data_sem_rev$x02
data_sem_rev$x14 <- -data_sem_rev$x14

out <- q_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("x01", "-x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "x13", "-x14"),
                            w = c("x06", "x07", "x08")),
          moderators = c("x10 -> m" = "x12",
                         "m ->y" = "w"),
          model = "simple",
          data = data_sem_rev,
          fit_method = "sem",
          indicator_method = "sam",
          boot_ci = FALSE,
          R = 100,
          seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

out

out_simple <- q_simple_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("x01", "-x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "x13", "-x14"),
                            w = c("x06", "x07", "x08")),
          moderators = c("x10 -> m" = "x12",
                         "m ->y" = "w"),
          data = data_sem_rev,
          fit_method = "sem",
          indicator_method = "sam",
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
          indicators = list(y = c("x01", "-x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "x13", "-x14"),
                            w = c("x06", "x07", "x08")),
          moderators = c("x10 -> m" = "x12",
                         "m ->y" = "w"),
          data = data_sem_rev,
          fit_method = "sem",
          indicator_method = "sam",
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
          indicators = list(y = c("x01", "-x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "x13", "-x14"),
                            w = c("x06", "x07", "x08")),
          moderators = c("x10 -> m" = "x12",
                         "m ->y" = "w"),
          data = data_sem_rev,
          fit_method = "sem",
          indicator_method = "sam",
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
          indicators = list(y = c("x01", "-x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "x13", "-x14"),
                            w = c("x06", "x07", "x08")),
          moderators = c("x10 -> m" = "x12",
                         "m ->y" = "w"),
          data = data_sem_rev,
          fit_method = "sem",
          indicator_method = "sam",
          boot_ci = FALSE,
          # R = 100,
          # seed = 1234,
          parallel = FALSE,
          progress = !is_testing())

mod <-
"
m ~ x10 + c2 + x12 + x10:x12
y ~ m + x10 + c2 + x12 + w + m:w
m =~ x04 + x05 + x09
c2 =~ x11 + x13 + x14
y =~ x01 + x02 + x03
w =~ x06 + x07 + x08

# Covariances added to ensure invariance to linear shifts
m ~~ m:w
w ~~ m:w
x10 ~~ x12
x10 ~~ x10:x12
x12 ~~ x10:x12
c2 ~~ w
c2 ~~ m:w
w ~~ m:w
"

fit <- sam(
  mod,
  data = data_sem
)

ind <- cond_indirect_effects(
  wlevels = c("x12", "w"),
  x = "x10",
  y = "y",
  m = "m",
  fit = fit)

ind_stdxy <- cond_indirect_effects(
  wlevels = c("x12", "w"),
  x = "x10",
  y = "y",
  m = "m",
  fit = fit,
  standardized_x = TRUE,
  standardized_y = TRUE)

expect_identical(
  coef(out$cond_ind_out$ustd[[1]]),
  coef(ind),
  tolerance = 1e-5,
  ignore_attr = TRUE
)
expect_identical(
  coef(out$cond_ind_out$stdxy[[1]]),
  coef(ind_stdxy),
  tolerance = 1e-5,
  ignore_attr = TRUE
)

})

test_that("q function: mediation with indicators: SAM: boot_ci", {

skip("To be examined in an interactive session")

# ==== q function: mediation with indicators: SAM: boot_ci ====

data_sem_rev <- data_sem
data_sem_rev$x02 <- -data_sem_rev$x02
data_sem_rev$x14 <- -data_sem_rev$x14

suppressWarnings(
out <- q_mediation(
          x = "x10",
          y = "y",
          m = "m",
          cov = c("c2", "x12"),
          indicators = list(y = c("x01", "-x02", "x03"),
                            m = c("x04", "x05", "x09"),
                            c2 = c("x11", "x13", "-x14"),
                            w = c("x06", "x07", "x08")),
          moderators = c("x10 -> m" = "x12",
                         "m ->y" = "w"),
          model = "simple",
          data = data_sem_rev,
          fit_method = "sem",
          indicator_method = "sam",
          boot_ci = TRUE,
          R = 5000,
          seed = 2345,
          parallel = TRUE,
          progress = !is_testing())
)

mod <-
"
m ~ x10 + c2 + x12 + x10:x12
y ~ m + x10 + c2 + x12 + w + m:w
m =~ x04 + x05 + x09
c2 =~ x11 + x13 + x14
y =~ x01 + x02 + x03
w =~ x06 + x07 + x08

# Covariances added to ensure invariance to linear shifts
m ~~ m:w
w ~~ m:w
x10 ~~ x12
x10 ~~ x10:x12
x12 ~~ x10:x12
c2 ~~ w
c2 ~~ m:w
w ~~ m:w
"

# Suppress the harmless warning that will
# appear in lavaan 0.7-1
suppressWarnings(
fit <- sam(
  mod,
  data = data_sem,
  se = "bootstrap",
  parallel = "snow",
  bootstrap.args = list(R = 5000),
  iseed = 2345
)
)

suppressWarnings(
ind <- cond_indirect_effects(
  wlevels = c("x12", "w"),
  x = "x10",
  y = "y",
  m = "m",
  fit = fit,
  boot_ci = TRUE)
)

expect_identical(coef(out$cond_ind_out$ustd[[1]]),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

suppressWarnings(
ind <- cond_indirect_effects(
  wlevels = c("x12", "w"),
  x = "x10",
  y = "y",
  m = "m",
  fit = fit,
  standardized_x = TRUE,
  standardized_y = TRUE,
  boot_ci = TRUE)
)

expect_identical(coef(out$cond_ind_out$stdxy[[1]]),
                 coef(ind),
                 tolerance = 1e-5,
                 ignore_attr = TRUE)

})

