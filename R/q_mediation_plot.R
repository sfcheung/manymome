#' @title Plot Method for the Output
#' of 'q_mediation' Family
#'
#' @description Plot the path model
#' fitted by the family of 'q_mediation'
#' functions.
#'
#' @details
#' This method requires the
#' `semptools` and `semPlot`` packages.
#' They are not installed by default.
#' Install them first before using the
#' plot method.
#'
#' This method draws the path models
#' fitted by [q_mediation()],
#' [q_simple_mediation()],
#' [q_serial_mediation()], and
#' [q_parallel_mediation()], with
#' path coefficients and R-squares.
#'
#' It will try to set positions of the
#' variables automatically, following
#' the left-to-right convention: `x`
#' variables on the left, `y` variables
#' on the right, mediators between them,
#' and arrows (paths) flow from left to
#' right. The figure should usually be
#' usable. If not, it can be further
#' modified by helper functions such as
#' those in `semptools` that can
#' manipulate a `qgraph` object.
#'
#' @return A `qgraph` object generated
#' by [semPlot::semPaths()], which is
#' plotted by default unless `plot_now`
#' is set to `FALSE`. It can be further
#' modified by other functions that
#' work on a `qgraph` object, such as
#' those from `semptools`.
#'
#' @param x The output of
#' [q_mediation()],
#' [q_simple_mediation()],
#' [q_serial_mediation()], and
#' [q_parallel_mediation()].
#' (Named `x`
#' because it is required in the naming
#' of arguments of the `plot` generic
#' function.)
#'
#' @param standardized Logical. If
#' `TRUE`, `betaS` in the printout of
#' [q_mediation()] family will be used
#' in the figure, with only numerical
#' variables standardized. If `FALSE`,
#' the default, then the original
#' (unstandardized) coefficients will
#' be used.
#'
#' @param size_variables  The size of
#' the observed variables, to be passed
#' to `sizeMan` of
#' [semPlot::semPaths()]. Default is
#' `NULL` and the size is determined
#' internally based on the number of
#' variables.
#'
#' @param size_path_labels The size of
#' the edge labels (parameter
#' estimates), to be passed to
#' `edge.label.cex` of
#' [semPlot::semPaths()]. Default is
#' `NULL` and the size is determined
#' internally based on the number of
#' variables.
#'
#' @param nchar_variables The number of
#' characters to be displayed for each
#' variable. To be passed to
#' `nCharNodes` of
#' [semPlot::semPaths()]. Default is
#' `NULL`, equivalent to `0` for
#' `nCharNodes`, to disable abbreviation
#' of the variable names.
#'
#' @param nchar_path_labels The number
#' of characters to be displayed for
#' each label for a path. To be passed
#' to `nCharEdges` of
#' [semPlot::semPaths()]. Default is
#' `NULL`, equivalent to `0` for
#' `nCharEdges`e, to disable
#' abbreviation of the labels.
#'
#' @param rsquares Logical. If `TRUE`,
#' the default, R-squares will be
#' drawn instead of error variances for
#' mediators and outcome (`y`) variables.
#'
#' @param sigs Logical. If `TRUE`, the
#' default, significance test results
#' will be marked by asterisks, based on
#' the same *p*-values for R-squares
#' displayed when printing the output of
#' the [q_mediation()] family.
#'
#' @param margins The margins of the plot.
#' A numeric vector of four values:
#' bottom, left, top, and right. Passed
#' to the `mar` argument of
#' [semPlot::semPaths()].
#'
#' @param v_pos How the mediators are to
#' be positioned vertically. If set to
#' `"middle`, with one `x` variable and
#' one `y` variable, the mediators will
#' tend to be placed around the
#' horizontal line joining `x` and `y`.
#' If set to `"upper"`, they will be
#' placed along or above this line. If
#' set to `"lower"`, they will be placed
#' along or below this line. Note that
#' this only affects the initial positions.
#' The positions will be further adjusted
#' based on the free paths in the model.
#' This argument is to be passed to
#' [semptools::auto_layout_mediation()].
#'
#' @param v_preference The preference in
#' shifting the mediators upward
#' (`"upper"`) or downward (`"lower"`)
#' to avoid blocking or overlapping with
#' any paths in the models. It is used
#' only when`v_pos` is `"middle"`. If
#' `v_pos` is `"lower"`, then
#' `v_preference` will be forced to be
#' `"lower". If `v_pos` is `"upper"`,
#' then `v_preference` will be forced to
#' be `"upper". This argument is to be
#' passed to
#' [semptools::auto_layout_mediation()].
#'
#' @param ... Optional arguments to be
#' passed to [semPlot::semPaths()] to
#' generate the initial plot, before
#' being adjusted by
#' [semptools::auto_layout_mediation()].
#'
#' @param plot_now If `TRUE`, the default,
#' the plot will be plotted when calling
#' this method.
#'
#' @examples
#'
#' # CI disabled in these examples.
#' # Please see the help page of these functions on forming
#' # confidence intervals for the indirect effects.
#'
#' # ===== Simple mediation
#'
#' out <- q_simple_mediation(x = "x",
#'                           y = "y",
#'                           m = "m",
#'                           cov = c("c2", "c1"),
#'                           boot_ci = FALSE,
#'                           data = data_med)
#' plot(out)
#'
#' # ===== Serial mediation
#'
#' out <- q_serial_mediation(x = "x",
#'                           y = "y",
#'                           m = c("m1", "m2"),
#'                           cov = c("c2", "c1"),
#'                           boot_ci = FALSE,
#'                           data = data_serial)
#' plot(out)
#'
#' # ==== Parallel mediation
#'
#' out <- q_parallel_mediation(x = "x",
#'                             y = "y",
#'                             m = c("m1", "m2"),
#'                             cov = c("c2", "c1"),
#'                             boot_ci = FALSE,
#'                             data = data_parallel)
#'
#' plot(out)
#'
#' # ===== A user-specified mediation model
#'
#' out <- q_mediation(x = "x1",
#'                    y = "y1",
#'                    model = c("x1 -> m11 -> m2 -> y1",
#'                              "x1 -> m12 -> m2 -> y1"),
#'                    cov = c("c2", "c1"),
#'                    boot_ci = FALSE,
#'                    data = data_med_complicated)
#' plot(out)
#'
#' @export
plot.q_mediation <- function(
                x,
                standardized = FALSE,
                size_variables = NULL,
                size_path_labels = NULL,
                nchar_variables = NULL,
                nchar_path_labels = NULL,
                rsquares = TRUE,
                sigs = TRUE,
                margins = c(5, 5, 5, 5),
                v_pos = c("middle", "lower", "upper"),
                v_preference = c("upper", "lower"),
                plot_now = TRUE,
                ...
              ) {

  v_pos <- match.arg(v_pos)
  v_preference <- match.arg(v_preference)

  # ==== Check package requirements ====

  if (!requireNamespace("semptools", quietly = TRUE)) {
    warning("Please install 'semptools' first.")
    return(NULL)
  }
  if (!requireNamespace("semPlot", quietly = TRUE)) {
    warning("Please install 'semPlot' first.")
    return(NULL)
  }

  # ==== Check the fit object ====

  fit <- x$lm_out
  fit_type <- NA
  if (inherits(fit, "lavaan")) {
    fit_type <- "lavaan"
  } else {
    fit_type <- "lm"
  }
  est <- switch(
            fit_type,
            lm = lmhelprs::lm_list_to_partable(fit),
            lavaan = lavaan::parameterEstimates(fit)
          )

  # ==== Standardized Solution ====

  if (standardized) {
    if (fit_type == "lm") {
      est <- add_betaselect_lm_list(
                  fit = fit,
                  ptable = est
                )
    } else if (fit_type == "lavaan") {
      est <- add_betaselect_lm_out_lav(
                  lm_out_lav = x$lm_out_lav,
                  ptable = est
                )
    }
  }

  # ==== semPlotModel ====
  if (!standardized) {
    pm <- switch(
              fit_type,
              lm = semPlot::semPlotModel(est),
              lavaan = semPlot::semPlotModel(fit)
            )
  } else {
    se <- FALSE
    if (fit_type == "lavaan") {
      est$ustart <- est$est
      tmp <- !is.na(est$z)
      est$free <- 0
      est$free[tmp] <- seq_along(which(tmp))
      est$plabel <- paste0(".p", seq_len(nrow(est)), ".")
    }
    est$est_org <- est$est
    est$est <- est$est.std
    est$ustart <- est$est
    pm <- switch(
              fit_type,
              lm = semPlot::semPlotModel(est),
              lavaan = semPlot::semPlotModel(est)
            )
  }
  fit_x <- x$x
  fit_y <- x$y
  fit_m <- x$m
  fit_cov <- eval(x$call$cov)
  fit_ov <- switch(
              fit_type,
              lm = lavaan::lavNames(est, "ov"),
              lavaan = lavaan::lavNames(fit, "ov")
            )
  fit_ov <- setdiff(fit_ov, fit_cov)
  pm <- semptools::keep_nodes(pm, c(fit_ov))

  # ==== Graphic Settings ====

  if (is.null(size_variables)) {
    sizeMan <- quick_scale(
                  m = fit_ov,
                  val_max = 10,
                  val_min = 7,
                  m_p_max = 1 + 2,
                  m_p_min = 4 + 2
                )
  } else {
    sizeMan <- size_variables
  }

  if (is.null(size_path_labels)) {
    edge.label.cex <- quick_scale(
                    m = fit_ov,
                    val_max = 1.00,
                    val_min = 0.75,
                    m_p_max = 1 + 2,
                    m_p_min = 4 + 2
                  )
  } else {
    edge.label.cex <- size_path_labels
  }

  # ==== Base Plot ====

  p <- semPlot::semPaths(
                object = pm,
                what = "paths",
                whatLabels = "est",
                intercepts = FALSE,
                residuals = rsquares,
                thresholds = FALSE,
                nCharNodes = ifelse(is.null(nchar_variables), 0, nchar_variables),
                nCharEdges = ifelse(is.null(nchar_path_labels), 0, nchar_path_labels),
                sizeMan = sizeMan,
                edge.label.cex = edge.label.cex,
                mar = margins,
                ...,
                DoNotPlot = TRUE
              )

  # ==== R-squares ====

  if (rsquares) {
    rsq_est <- switch(
                fit_type,
                lm = rsq_to_ptable(fit),
                lavaan = rsq_to_ptable(x$lm_out_lav)
              )
    p <- switch(
            fit_type,
            lm = semptools::add_rsq(
                      p,
                      ests = rsq_est
                    ),
            lavaan = semptools::add_rsq(
                      p,
                      object = fit
                    )
          )
  } else {
    rsq_est <- NULL
  }

  # ==== Layout ====

  p <- semptools::auto_layout_mediation(
            p,
            v_pos = v_pos,
            v_preference = v_preference
          )
  p <- semptools::safe_edge_label_position(p)
  p <- semptools::safe_resid_position(p)

  # ==== Sig. Stars ====

  if (sigs) {
    p <- switch(
          fit_type,
          lm = semptools::mark_sig(
            p,
            ests = est,
            ests_r2 = rsq_est
          ),
          lavaan = semptools::mark_sig(
              p,
              object = fit,
              ests_r2 = rsq_est
            )
        )
  }

  # ==== Plot? ====

  if (plot_now) {
    plot(p)
  } else {
    return(p)
  }
}

