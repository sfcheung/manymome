#' @title Plot Method for the Output
#' of 'q_mediation' Family
#'
#' @description Plot the path model
#' fitted by the family of 'q_mediation'
#' functions.
#'
#' @details
#' This method requires the
#' `semptools` and `semPlot` packages.
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
#' manipulate a `qgraph` object. For
#' example, [semptools::move_node()]
#' can be used to adjust the position
#' of a variable in the plot.
#'
#' The helper function [indirect_on_plot()]
#' adds the indirect effect
#' estimates (as well as confidence
#' intervals and *p*-values, if
#' available) to a plot. The `plot` method
#' will add these effects by default,
#' and so users usually do not need to
#' use this function.
#' However, if the plot needs to be modified
#' before being drawn, this function
#' can be used to add the effects
#' after drawing the modified plot.
#'
#' @return
#' The `plot` method returns a `qgraph`
#' object generated
#' by [semPlot::semPaths()], which is
#' plotted by default unless `plot_now`
#' is set to `FALSE`. It can be further
#' modified by other functions that
#' work on a `qgraph` object, such as
#' those from `semptools`.
#'
#' The function [indirect_on_plot()]
#' returns the object set to
#' `q_mediation_output`
#' invisibly. It is called for its
#' side-effect.
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
#' the observed variables (the
#' "rectangles"), to be passed
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
#' variables in the plot.
#'
#' @param nchar_variables The number of
#' characters to be displayed for each
#' variable. To be passed to
#' `nCharNodes` of
#' [semPlot::semPaths()]. Default is
#' `NULL`, equivalent to `0` for
#' `nCharNodes`, to disable abbreviating
#' the variable names.
#'
#' @param nchar_path_labels The number
#' of characters to be displayed for
#' each label for a path. To be passed
#' to `nCharEdges` of
#' [semPlot::semPaths()]. Default is
#' `NULL`, equivalent to `0` for
#' `nCharEdges`, to disable
#' abbreviating the labels.
#'
#' @param digits The number
#' of digits to be printed after the
#' decimals. To be passed
#' to `nDigits` of
#' [semPlot::semPaths()]. Default is
#' 2.
#'
#' @param rsquares Logical. If `TRUE`,
#' the default, R-squares will be
#' drawn instead of error variances for
#' mediators and outcome variables
#' (the `y` variables).
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
#' `"middle"`, with one `x` variable and
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
#' `"lower"`. If `v_pos` is `"upper"`,
#' then `v_preference` will be forced to
#' be `"upper"`. This argument is to be
#' passed to
#' [semptools::auto_layout_mediation()].
#'
#' @param print_indirect Logical.
#' Whether the indirect effect(s), and
#' total indirect effect if applicable,
#' will be printed on the plot. Default
#' is `TRUE`. Used only if `plot_now`
#' is `TRUE`. Confidence intervals,
#' if stored, will be printed, at
#' the level of confidence used when
#' doing the analysis.
#'
#' @param indirect_standardized If
#' `print_indirect` is `TRUE`, which
#' type of
#' effects are to be printed: `"none"`
#' for the unstandardized (raw) indirect
#' effects, `"stdx"` for the effects with
#' `x` standardized, `"stdy"` for the
#' effects with `y` standardized, and
#' `"stdxy"` for the effects with both
#' `x` and `y` standardized.
#'
#' @param size_indirect The size used
#' when printing the indirect effects.
#' The final size is determined by
#' multiplying the final
#' value `size_path_labels` (determined
#' internally if it is set to `NULL`)
#' by this value. If equal to 1, then
#' the size used in printing the
#' indirect effects should be "close"
#' to the size of numbers on the paths.
#'
#' @param ... For the `plot` method,
#' these are optional arguments to be
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
#' # These examples require the package
#' # semptools (version 0.3.2 or above).
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
#' # Standardized effects
#' plot(out,
#'      standardized = TRUE,
#'      indirect_standardized = "stdxy")
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
#' plot(out,
#'      v_pos = "lower")
#' plot(out,
#'      v_pos = "upper")
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
#' @importFrom graphics mtext par
plot.q_mediation <- function(
                x,
                standardized = FALSE,
                size_variables = NULL,
                size_path_labels = NULL,
                nchar_variables = NULL,
                nchar_path_labels = NULL,
                digits = 2,
                rsquares = TRUE,
                sigs = TRUE,
                margins = c(5, 5, 5, 5),
                v_pos = c("middle", "lower", "upper"),
                v_preference = c("upper", "lower"),
                print_indirect = TRUE,
                indirect_standardized = c("none", "stdx", "stdy", "stdxy"),
                size_indirect = 1,
                plot_now = TRUE,
                ...
              ) {

  v_pos <- match.arg(v_pos)
  v_preference <- match.arg(v_preference)

  indirect_standardized <- match.arg(indirect_standardized)

  # ==== Check package requirements ====

  if (!requireNamespace("semptools", quietly = TRUE)) {
    warning("Please install 'semptools' (0.3.2 or above) first.")
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

  if (print_indirect) {
    k_ind <- length(x$ind_out$ustd)
    if (k_ind > 1) {
      add_total <- TRUE
      mar_add <- k_ind + 1
    } else {
      add_total <- FALSE
      mar_add <- 1
    }
    # mar_add <- mar_add + .5
    # if (margins[1] < mar_add) {
    #   margins[1] <- mar_add
    # }
  }

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
                nDigits = digits,
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
                      digits = digits,
                      ests = rsq_est
                    ),
            lavaan = semptools::add_rsq(
                      p,
                      digits = digits,
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

    if (print_indirect) {

      # ==== Print indirect effects ====

      # No need for these lines because oma is used
      # node_too_close <- node_below(p)

      # if (node_too_close) {
      #   p <- semptools::rescale_layout(
      #                 p,
      #                 y_min = -.9
      #               )
      # }
      # plot(p)

      indirect_on_plot(q_mediation_output = x,
                       digits = digits,
                       size_indirect = size_indirect,
                       indirect_standardized = indirect_standardized,
                       margins = margins,
                       original_plot = p)

      # which_ind <- switch(indirect_standardized,
      #                     none = "ustd",
      #                     stdx = "stdx",
      #                     stdy = "stdy",
      #                     stdxy = "stdxy")

      # tmp <- indirect_list_to_note(x$ind_out[[which_ind]],
      #                              digits = digits)
      # text_indirect_list(tmp,
      #                   cex = edge.label.cex * size_indirect)
      # if (add_total) {
      #   tmp <- total_indirect_to_note(x$ind_total[[which_ind]],
      #                                 digits = digits)
      #   text_total_indirect(tmp,
      #                       line = k_ind + 1,
      #                       cex = edge.label.cex * size_indirect)
      # }
    } else {
      plot(p)
    }
  }
  invisible(p)
}

#' @rdname plot.q_mediation
#'
#' @param q_mediation_output The original
#' object used to generate the plot
#' (the output of the [q_mediation()]
#' family).
#' Indirect effects will be retrieved
#' from this output.
#'
#' @param original_plot The plot
#' generated by the `plot` method. If
#' supplied, a new plot will be
#' generated and then the indirect
#' effects will be printed on this new
#' plot. If `NULL`, the default, the
#' indirect effects will be printed on
#' the existing plot. Space will be
#' added to make room for the
#' indirect effects only if this
#' argument is set. If `original_plot`
#' is not used, make sure there is
#' enough room at the bottom for the
#' indirect effects.
#'
#' @export
indirect_on_plot <- function(
                            q_mediation_output = NULL,
                            digits = 2,
                            size_indirect = 1,
                            indirect_standardized = c("none", "stdx", "stdy", "stdxy"),
                            margins = c(5, 5, 5, 5),
                            original_plot = NULL
                          ) {
  indirect_standardized <- match.arg(indirect_standardized)

  # ==== Retrieve edge.label.cex, if possible ====

  if (!is.null(original_plot)) {
    edge.label.cex <- original_plot$graphAttributes$Edges$label.cex
  } else {
    edge.label.cex <- 1
  }

  # ==== Determine the lines required ====

  k_ind <- length(q_mediation_output$ind_out$ustd)
  if (k_ind > 1) {
    add_total <- TRUE
    mar_add <- k_ind + 1
  } else {
    add_total <- FALSE
    mar_add <- 1
  }
  mar_add <- mar_add + .5

  # ==== Set the outer margin, if necessary ====

  parold <- par(no.readonly = TRUE)

  if (!is.null(original_plot)) {
    on.exit(par(parold))
    use_outer <- TRUE
    par(oma = c(mar_add, 0, 0, 0))
    original_plot$plotOptions$mar[1] <- .5
    plot(original_plot)
  } else {
    use_outer <- FALSE
  }

  # ==== Standardized indirect effects? ====

  which_ind <- switch(indirect_standardized,
                      none = "ustd",
                      stdx = "stdx",
                      stdy = "stdy",
                      stdxy = "stdxy")

  # ==== Print the indirect effect(s) ====

  tmp <- indirect_list_to_note(q_mediation_output$ind_out[[which_ind]],
                                digits = digits)
  text_indirect_list(tmp,
                    cex = edge.label.cex * size_indirect,
                    start_at = 0,
                    outer = use_outer)

  if (add_total) {

    # ==== Print the total indirect effects ====

    tmp <- total_indirect_to_note(q_mediation_output$ind_total[[which_ind]],
                                  digits = digits)
    text_total_indirect(tmp,
                        line = k_ind,
                        cex = edge.label.cex * size_indirect,
                        outer = use_outer)
  }

  # ==== Reset par() ====

  par(parold)

  invisible(q_mediation_output)
}