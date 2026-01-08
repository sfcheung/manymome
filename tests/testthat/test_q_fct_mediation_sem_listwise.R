skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem: listwise", {
data(data_med)
data_med[1:3, "m"] <- NA
data_med[4:10, "y"] <- NA
out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 40,
                          seed = 1234,
                          fit_method = "sem",
                          missing = "listwise",
                          parallel = FALSE,
                          progress = FALSE)
expect_equal(lavInspect(out$lm_out, "ntotal"), 90)

data_med <- na.omit(data_med)
out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 40,
                          seed = 1234,
                          fit_method = "sem",
                          parallel = FALSE,
                          progress = FALSE)
expect_equal(lavInspect(out$lm_out, "ntotal"), 90)

})

# Test: Serial mediation

test_that("q function: serial mediation: listwise", {
data(data_serial)
data_serial[1:3, "m1"] <- NA
data_serial[4:6, "m2"] <- NA
data_serial[7:8, "y"] <- NA
out0 <- q_serial_mediation(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           cov = c("c2", "c1"),
                           data = data_serial,
                           R = 40,
                           seed = 1234,
                           fit_method = "sem",
                           missing = "listwise",
                           parallel = FALSE,
                           progress = FALSE)
expect_equal(lavInspect(out0$lm_out, "ntotal"), 92)

out0 <- q_serial_mediation(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           cov = c("c2", "c1"),
                           data = na.omit(data_serial),
                           R = 40,
                           seed = 1234,
                           fit_method = "sem",
                           missing = "listwise",
                           parallel = FALSE,
                           progress = FALSE)
expect_equal(lavInspect(out0$lm_out, "ntotal"), 92)

})

# Test: Parallel mediation

test_that("q function: parallel mediation: listwise", {
data(data_parallel)
data_parallel[1:3, "m1"] <- NA
data_parallel[4:6, "m2"] <- NA
data_parallel[7:8, "y"] <- NA
out0 <- q_parallel_mediation(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              cov = c("c2", "c1"),
                              data = data_parallel,
                              R = 40,
                              fit_method = "sem",
                              missing = "listwise",
                              seed = 1234,
                              parallel = FALSE,
                              progress = FALSE)
expect_equal(lavInspect(out0$lm_out, "ntotal"), 92)

out0 <- q_parallel_mediation(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              cov = c("c2", "c1"),
                              data = na.omit(data_parallel),
                              R = 40,
                              fit_method = "sem",
                              seed = 1234,
                              parallel = FALSE,
                              progress = FALSE)
expect_equal(lavInspect(out0$lm_out, "ntotal"), 92)
})

