skip("WIP")

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem", {

plot_q <- function(
                x,
                v_pos = c("middle", "lower", "upper"),
                v_preference = c("upper", "lower"),
                ...
              ) {
  if (!requireNamespace("semptools", quietly = TRUE)) {
    stop("Please install 'semptools' first.")
  }
  if (!requireNamespace("semPlot", quietly = TRUE)) {
    stop("Please install 'semPlot' first.")
  }
  fit <- x$lm_out
  fit_type <- NA
  if (inherits(fit, "lavaan")) {
    fit_type <- "lavaan"
    pm <- semPlot::semPlotModel(fit)
    fit_x <- x$x
    fit_y <- x$y
    fit_m <- x$m
    fit_cov <- eval(x$call$cov)
    fit_ov <- lavaan::lavNames(fit, "ov")
    fit_ov <- setdiff(fit_ov, fit_cov)
    pm <- semptools::keep_nodes(pm, c(fit_ov))
    p <- semPlot::semPaths(
                  pm,
                  whatLabels = "est",
                  intercepts = FALSE,
                  exoCov = FALSE,
                  DoNotPlot = TRUE
                )
    p <- semptools::add_rsq(p,
                            object = fit)
    p <- semptools::auto_layout_mediation(
              p,
              v_pos = v_pos,
              v_preference = v_preference
            )
    p <- semptools::safe_edge_label_position(p)
    p <- semptools::safe_resid_position(p)
    rsq_est <- rsq_to_ptable(x$lm_out_lav)
    p <- semptools::mark_sig(
              p,
              object = fit,
              ests_r2 = rsq_est)
    plot(p)
  } else if (inherits(fit, "lm_list")) {
    stop("Not ready for now")
  }
}

rsq_to_ptable <- function(out_lav) {
  rsq_test <- sapply(
                out_lav,
                \(x) x$rsq_test
              )
  out <- data.frame(lhs = names(rsq_test),
                    op = "r2",
                    pvalue = rsq_test)
  rownames(out) <- names(rsq_test)
  out
}

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
plot_q(out0)

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
plot_q(outs)

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
plot_q(outp)

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
plot_q(outm1)


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
plot_q(outm2)

outm3 <- q_mediation(
            x = "x2",
            y = "y1",
            model = c("x2 -> m2 -> m11 -> y1",
                      "x2 -> m2 -> m12 -> y1"),
            cov = c("c1", "c2"),
            fit_method = "sem",
            data = data_med_complicated,
            R = 200,
            ci_type = "mc",
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm3)

})

