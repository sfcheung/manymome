#' @title Plot an Effect Against a Moderator
#'
#' @description It plots an effect, direct
#' or indirect,
#' against a moderator, with confidence
#' band if available.
#'
#' @details It receives an output
#' of [cond_indirect_effects()] and
#' plot the effect against the
#' moderator. The effect can be an
#' indirect effect or a direct effect.
#'
#' It uses the levels of the moderator
#' stored in the output of
#' [cond_indirect_effects()]. Therefore,
#' the desired levels of the moderator
#' to be plotted needs to be specified
#' when calling [cond_indirect_effects()],
#' as illustrated in the example.
#'
#' Currently, this function only supports
#' a path with exactly one moderator,
#' and the moderator is a numeric variable.
#'
#' ## Using Original Standard Errors
#'
#' If the following conditions are met, the
#' stored standard errors, if available,
#' will be used to
#' form the confidence intervals:
#'
#' - Confidence intervals have not been
#'  formed (e.g., by bootstrapping or
#'  Monte Carlo).
#'
#' - The path has no mediators.
#'
#' - The model has only one group.
#'
#' - The path is moderated by one or
#'  more moderator.
#'
#' - Both the `x`-variable and the
#'  `y`-variable are not standardized.
#'
#' If the model is fitted by OLS
#' regression (e.g., using [stats::lm()]),
#' then the variance-covariance matrix
#' of the coefficient estimates will be
#' used, and confidence
#' intervals are computed from the *t*
#' statistic.
#'
#' If the model is fitted by structural
#' equation modeling using `lavaan`, then
#' the variance-covariance computed by
#' `lavaan` will be used,
#' and confidence intervals are computed
#' from the *z* statistic.
#'
#' ## Caution
#'
#' If the model is fitted by structural
#' equation modeling and has moderators,
#' the standard errors, *p*-values,
#' and confidence interval computed
#' from the variance-covariance matrices
#' for conditional effects
#' can only be trusted if all covariances
#' involving the product terms are free.
#' If any of them are fixed, for example,
#' fixed to zero, it is possible
#' that the model is not invariant to
#' linear transformation of the variables.
#'
#'
#' @return
#' [plot_effect_vs_w()] returns a
#' [ggplot2] graph. Plotted if
#' not assigned to a name. It can be
#' further modified like a usual
#' [ggplot2] graph.
#'
#' @param object The output of
#' [cond_indirect_effects()].
#'
#' @param w The name of the moderator.
#' Must be present in `object`. If `NULL`,
#' the default,
#' and `object` has only one moderator,
#' then it will be set to that moderator.
#' Because this function currently only
#' supports a path with only one moderator,
#' this argument can be left as `NULL` for now.
#'
#' @param w_label The label of the
#' horizontal axis. If `NULL`,
#' the default, it will be
#' `paste0("Moderator: ", w)`.
#'
#' @param effect_label The label of the
#' vertical axis. If `NULL`, the default,
#' it will be generated from the path.
#'
#' @param add_zero_line Whether a
#' horizontal line at zero will be drawn.
#' Default is `TRUE`.
#'
#' @param always_draw_zero_line If `FALSE`,
#' the default,
#' then the line at zero, if requested
#' will be drawn
#' only if zero is within the range of
#' the plot. If `TRUE`, then the line
#' at zero will always be drawn.
#'
#' @param line_linewidth The width of
#' the line of the effect for each
#' level of the moderator, to be used
#' by [ggplot2::geom_line()]. Default is 1.
#' Always overrides the value of
#' `line_args`.
#'
#' @param line_color The color of the line
#' of the effect for each level of the
#' moderator, to be used
#' by [ggplot2::geom_line()].
#' Default is "blue".
#' Always overrides the value of
#' `line_args`.
#'
#' @param shade_the_band If `TRUE`,
#' the default, a confidence band will
#' be drawn as a region along the line
#' if confidence intervals can be retrieved
#' from `object`.
#'
#' @param draw_the_intervals If `TRUE`,
#' the default, two lines will be drawn
#' for the confidence intervals along
#' the line if they can be retrieved
#' from `object`.
#'
#' @param band_fill_color The color of
#' of the confidence band, to be used
#' by [ggplot2::geom_ribbon()].
#' Default is "lightgrey".
#' Always overrides the value of
#' `band_args`.
#'
#' @param band_alpha A number from
#' 0 to 1 for the level
#' of transparency
#' of the confidence band, to be used
#' by [ggplot2::geom_ribbon()].
#' Default is `.50`.
#' Always overrides the value of
#' `band_args`.
#'
#' @param intervals_color The color of
#' the lines of the confidence intervals,
#' to be used
#' by [ggplot2::geom_line()]. Default is
#' `"black"`.
#' Always overrides the value of
#' `intervals_args`.
#'
#' @param intervals_linetype The line
#' type of
#' the lines of the confidence intervals,
#' to be used
#' by [ggplot2::geom_line()]. Default is
#' `"longdash"`.
#' Always overrides the value of
#' `intervals_args`.
#'
#' @param intervals_linewidth The line
#' width of
#' the lines of the confidence intervals,
#' to be used
#' by [ggplot2::geom_line()]. Default is
#' 1.
#' Always overrides the value of
#' `intervals_args`.
#'
#' @param zero_line_color The color of
#' the line at zero,
#' to be used
#' by [ggplot2::geom_line()]. Default is
#' `"grey"`.
#' Always overrides the value of
#' `zero_line_args`.
#'
#' @param zero_line_linetype The line
#' type of
#' the line at zero,
#' to be used
#' by [ggplot2::geom_line()]. Default is
#' `"solid"`.
#' Always overrides the value of
#' `zero_line_args`.
#'
#' @param zero_line_linewidth The line
#' width of
#' the line at zero,
#' to be used
#' by [ggplot2::geom_line()]. Default is
#' 1.
#' Always overrides the value of
#' `zero_line_args`.
#'
#' @param line_args A named list of
#' additional arguments to be passed
#' to [ggplot2::geom_line()] for the line
#' of the effect against moderator.
#' Default is `list()`.
#'
#' @param band_args A named list of
#' additional arguments to be passed
#' to [ggplot2::geom_ribbon()] for the
#' confidence band.
#' Default is `list()`.
#'
#' @param intervals_args A named list of
#' additional arguments to be passed
#' to [ggplot2::geom_line()] for the lines
#' of confidence intervals.
#' Default is `list()`.
#'
#' @param zero_line_args A named list of
#' additional arguments to be passed
#' to [ggplot2::geom_line()] for the line
#' at zero.
#' Default is `list()`.
#'
#' @param level The level of confidence
#' for the confidence intervals computed
#' from the original standard errors. Used only for
#' paths without mediators and both
#' `x`- and `y`-variables are not
#' standardized.
#'
#' @seealso [cond_indirect_effects()]
#'
#' @examples
#'
#' dat <- data_med_mod_a
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m + x + c1 + c2, dat)
#' fit_lm <- lm2list(lm_m, lm_y)
#' # Set R to a large value in real research.
#' boot_out_lm <- do_boot(fit_lm,
#'                        R = 50,
#'                        seed = 54532,
#'                        parallel = FALSE,
#'                        progress = FALSE)
#'
#' # Compute the conditional indirect effects
#' # from 2 SD below mean to 2 SD above mean of the moderator,
#' # by setting sd_from_mean of cond_indirect_effects().
#' # Set length.out to a larger number for a smooth graph.
#' out_lm <- cond_indirect_effects(wlevels = "w",
#'                                 x = "x",
#'                                 y = "y",
#'                                 m = "m",
#'                                 fit = fit_lm,
#'                                 sd_from_mean = seq(-2, 2, length.out = 10),
#'                                 boot_ci = TRUE,
#'                                 boot_out = boot_out_lm)
#' p <- plot_effect_vs_w(out_lm)
#' p
#' # The output is a ggplot2 graph and so can be further customized
#' library(ggplot2)
#' # Add the line for the mean of w, the moderator
#' p2 <- p + geom_vline(xintercept = mean(dat$w),
#'                      color = "red")
#' p2
#'
#' @export

plot_effect_vs_w <- function(object,
                             w = NULL,
                             w_label = NULL,
                             effect_label = NULL,
                             add_zero_line = TRUE,
                             always_draw_zero_line = FALSE,
                             line_linewidth = 1,
                             line_color = "blue",
                             shade_the_band = TRUE,
                             draw_the_intervals = TRUE,
                             band_fill_color = "lightgrey",
                             band_alpha = .50,
                             intervals_color = "black",
                             intervals_linetype = "longdash",
                             intervals_linewidth = 1,
                             zero_line_color = "grey",
                             zero_line_linewidth = 1,
                             zero_line_linetype = "solid",
                             line_args = list(),
                             band_args = list(),
                             intervals_args = list(),
                             zero_line_args = list(),
                             level = .95) {
    full_output_1 <- attr(object, "full_output")[[1]]
    has_m <- isTRUE(!is.null(full_output_1$m))
    standardized_x <- full_output_1$standardized_x
    standardized_y <- full_output_1$standardized_y
    path_str <- path_name(full_output_1)
    wlevels <- attr(object, "wlevels")
    w_type <- attr(wlevels, "w_type")
    has_wlevels <- cond_indirect_effects_has_wlevels(object)
    has_groups <- cond_indirect_effects_has_groups(object)
    if (ncol(wlevels) > 1) {
        stop("Only an effect with one moderator is supported.")
      }
    if (!is.numeric(wlevels[, 1])) {
        stop("Only numeric moderators are supported.")
      }
    if (is.null(w)) {
        w <- attr(wlevels, "names")
      }
    if (!isTRUE(w %in% colnames(wlevels))) {
        stop(w, " is not a moderator in the object.")
      }
    wvalue <- wlevels[, w]
    ind <- stats::coef(object)
    has_ci <- "CI.lo" %in% colnames(object)
    se_out <- cond_effects_original_se(object,
                                      level = level,
                                      append = FALSE)
    has_original_se <- !is.null(se_out)

    if (!has_ci &&
        !has_m &&
        !has_groups &&
        has_wlevels &&
        !standardized_x &&
        !standardized_y &&
        has_original_se) {
        has_ci <- TRUE
      }

    if (has_ci) {
        ci_lower <- stats::confint(object, level = level)[, 1]
        ci_upper <- stats::confint(object, level = level)[, 2]
      } else {
        ci_lower <- NULL
        ci_upper <- NULL
      }
    datplot <- data.frame(cbind(wvalue, ind, ci_lower, ci_upper))
    datplot <- datplot[order(datplot$wvalue, decreasing = TRUE), ]
    if (is.null(w_label)) {
        w_label <- paste0("Moderator: ", w)
      }
    if (is.null(effect_label)) {
        effect_label <- path_str
      }
    x_standardized <- full_output_1$standardized_x
    y_standardized <- full_output_1$standardized_y
    std_str <- character(0)
    if (x_standardized && y_standardized) {
        std_str <- paste0("(", full_output_1$x, " and ",
                               full_output_1$y, " standardized)")
      }
    if (x_standardized && !y_standardized) {
        std_str <- paste0("(", full_output_1$x, " standardized)")
      }
    if (!x_standardized && y_standardized) {
        std_str <- paste0("(", full_output_1$y, " standardized)")
      }
    effect_label <- paste(effect_label, std_str)
    line_args_final <- utils::modifyList(line_args,
                         list(linewidth = line_linewidth,
                              color = line_color))
    p <- ggplot2::ggplot(data = datplot,
                         ggplot2::aes(x = wvalue,
                                      y = ind)) +
          do.call(ggplot2::geom_line, line_args_final) +
          ggplot2::xlab(w_label) +
          ggplot2::ylab(effect_label)
    if (has_ci) {
        if (shade_the_band) {
            band_args_final <- utils::modifyList(band_args,
                                list(mapping = ggplot2::aes(x = wvalue,
                                                            ymin = ci_lower,
                                                            ymax = ci_upper),
                                     fill = band_fill_color,
                                     alpha = band_alpha))
            p <- p + do.call(ggplot2::geom_ribbon, band_args_final)
          }
        if (draw_the_intervals) {
            intervals_args_final <- utils::modifyList(intervals_args,
                                     list(linetype = intervals_linetype,
                                          linewidth = intervals_linewidth,
                                          color = intervals_color))
            intervals_args_final1 <- utils::modifyList(intervals_args_final,
                                     list(mapping = ggplot2::aes(x = wvalue,
                                                                  y = ci_lower)))
            intervals_args_final2 <- utils::modifyList(intervals_args_final,
                                     list(mapping = ggplot2::aes(x = wvalue,
                                                                  y = ci_upper)))
            p <- p +
                  do.call(ggplot2::geom_line, intervals_args_final1) +
                  do.call(ggplot2::geom_line, intervals_args_final2)
          }
      }
    if (has_ci) {
        tmp <- range(datplot$ci_lower, datplot$ci_upper)
      } else {
        tmp <- range(datplot$ind)
      }
    do_zero_line <- !((tmp[1] > 0) || tmp[2] < 0) || always_draw_zero_line
    if (add_zero_line && do_zero_line) {
        zero_line_args_final <- utils::modifyList(zero_line_args,
                                  list(linetype = zero_line_linetype,
                                       color = zero_line_color,
                                       linewidth = zero_line_linewidth,
                                       yintercept = 0))
        p <- p +
              do.call(ggplot2::geom_hline, zero_line_args_final)
      }
    p
  }

#' @details
#' The function [fill_wlevels()] is
#' a helper to automatically
#' fill in additional levels
#' of the moderators, to plot a graph
#' with smooth confidence band. It
#' accepts the output of [cond_indirect_effects()]
#' or [pseudo_johnson_neyman()], finds
#' the range of the values of the
#' moderator, and returns an output
#' of [cond_indirect_effects()] with
#' the desired number of levels within
#' this range. It is intended to be a
#' helper. If it does not work, users
#' can still get the desired number of
#' levels by setting the values manually
#' when calling [cond_indirect_effects()].
#'
#' @param to_fill The output of
#' [cond_indirect_effects()] or
#' [pseudo_johnson_neyman()], for which
#' additional levels of the moderator
#' will be added.
#'
#' @param cond_out If `to_fill` is
#' the output of [pseudo_johnson_neyman()],
#' the original output of [cond_indirect_effects()]
#' used in the call to [pseudo_johnson_neyman()]
#' need to be supplied through this argument.
#'
#' @param k The desired number of levels
#' of the moderator.
#'
#' @return
#' [fill_wlevels()] returns an updated
#' output of [cond_indirect_effects()]
#' with the desired number of levels of
#' the moderator.
#'
#' @examples
#'
#' # Use fill_wlevels to add moderator levels:
#'
#' dat <- data_med_mod_a
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m + x + c1 + c2, dat)
#' fit_lm <- lm2list(lm_m, lm_y)
#' wlevels <- mod_levels(w = "w",
#'                       sd_from_mean = c(-3, 0, 3),
#'                       fit = fit_lm)
#' wlevels
#' cond_out <- cond_indirect_effects(wlevels = wlevels,
#'                                   x = "x",
#'                                   y = "m",
#'                                   fit = fit_lm)
#' cond_out
#' # Only 3 points
#' p1 <- plot_effect_vs_w(cond_out)
#' p1
#' # Increase the number of levels to 15
#' cond_out_filled <- fill_wlevels(cond_out,
#'                                 k = 15)
#' cond_out_filled
#' p2 <- plot_effect_vs_w(cond_out_filled)
#' p2
#'
#'
#' @rdname plot_effect_vs_w
#' @export

fill_wlevels <- function(to_fill,
                         cond_out = NULL,
                         k = 21) {
    is_jn <- inherits(to_fill, "pseudo_johnson_neyman")
    if (is_jn && is.null(cond_out)) {
        stop("The original cond_indirect_effects output is required",
             "if the 'to_fill' is the output of 'pseudo_johnson_neyman()'.")
      }
    if (is_jn) {
        jn_cond <- to_fill$cond_effects
        wlevels <- attr(jn_cond, "wlevels")
      } else {
        cond_out <- to_fill
        wlevels <- attr(cond_out, "wlevels")
      }
    call0 <- attr(cond_out, "call")
    fit <- attr(cond_out, "fit")
    if (ncol(wlevels) > 1) {
        stop("Only support a path with one moderator.")
      }
    w <- colnames(wlevels)
    wlevels_raw <- attr(wlevels, "wlevels")
    wvalues <- wlevels[, w, drop = TRUE]
    if (length(wvalues) >= k) {
        return(cond_out)
      }
    wvalues_new <- seq(from = min(wvalues),
                       to = max(wvalues),
                       length.out = k)
    wlevels_new <- mod_levels(w = w,
                              fit = fit,
                              values = wvalues_new)
    call1 <- call0
    call1$wlevels <- wlevels_new
    out <- eval(call1,
                envir = parent.frame())
    out
  }