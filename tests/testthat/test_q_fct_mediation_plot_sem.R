skip("WIP")

skip_on_cran()
skip_if_not_installed("semptools")
skip_if_not_installed("semPlot")

library(testthat)
library(manymome)
library(lavaan)

test_that("q function plot", {

out0 <- q_simple_mediation(
            x = "x",
            y = "y",
            m = "m",
            cov = c("c2", "c1"),
            data = data_med,
            ci_type = "mc",
            R = 200,
            seed = 1234,
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(out0, plot_now = FALSE))
expect_no_error(plot(out0,
     standardized = TRUE, plot_now = FALSE))


outs <- q_serial_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_serial,
            ci_type = "mc",
            R = 100,
            seed = 1234,
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outs, plot_now = FALSE))
expect_no_error(plot(outs,
       rsquares = FALSE, plot_now = FALSE))
expect_no_error(plot(outs,
     standardized = TRUE, plot_now = FALSE))


outp <- q_parallel_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_parallel,
            fit_method = "sem",
            ci_type = "mc",
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outp, plot_now = FALSE))
expect_no_error(plot(outp,
     standardized = TRUE, plot_now = FALSE))

outm1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m2 -> y1",
                      "x1 -> m11 -> m12 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            ci_type = "mc",
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outm1, plot_now = FALSE))
expect_no_error(plot(outm1,
     standardized = TRUE, plot_now = FALSE))



outm2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> y1"),
            cov = c("c1", "c2"),
            fit_method = "sem",
            data = data_med_complicated,
            R = 200,
            ci_type = "mc",
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outm2, plot_now = FALSE))
expect_no_error(plot(outm2,
       standardized = TRUE, plot_now = FALSE))


outm3 <- q_mediation(
            x = "x2",
            y = "c1",
            model = c("x2 -> m2 -> m11 -> y1",
                      "x2 -> m2 -> m12 -> y1",
                      "m2 -> y1",
                      "y1 -> c1"),
            cov = c("c2"),
            fit_method = "sem",
            data = data_med_complicated,
            R = 200,
            ci_type = "mc",
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outm3, plot_now = FALSE))
expect_no_error(plot(outm3,
       standardized = TRUE, plot_now = FALSE))

# lm

out0 <- q_simple_mediation(
            x = "x",
            y = "y",
            m = "m",
            cov = c("c2", "c1"),
            data = data_med,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(out0, plot_now = FALSE))
expect_no_error(plot(out0,
     standardized = TRUE, plot_now = FALSE))


outs <- q_serial_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_serial,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outs, plot_now = FALSE))
expect_no_error(plot(outs,
     standardized = TRUE, plot_now = FALSE))

outp <- q_parallel_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_parallel,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outp, plot_now = FALSE))
expect_no_error(plot(outp,
     standardized = TRUE, plot_now = FALSE))

outm1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m2 -> y1",
                      "x1 -> m11 -> m12 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outm1, plot_now = FALSE))
expect_no_error(plot(outm1,
     standardized = TRUE, plot_now = FALSE))

outm2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outm2, plot_now = FALSE))
expect_no_error(plot(outm2,
     standardized = TRUE, plot_now = FALSE))

outm3 <- q_mediation(
            x = "x2",
            y = "c1",
            model = c("x2 -> m2 -> m11 -> y1",
                      "x2 -> m2 -> m12 -> y1",
                      "m2 -> y1",
                      "y1 -> c1"),
            cov = c("c2"),
            data = data_med_complicated,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
expect_no_error(plot(outm3, plot_now = FALSE))
expect_no_error(plot(outm3,
     standardized = TRUE, plot_now = FALSE))

})

