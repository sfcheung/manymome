#' @title Pseudo Johnson-Neyman Probing
#'
#' @description Use the pseudo
#' Johnson-Neyman approach (Hayes, 2022)
#' to find the range of values of a
#' moderator in which the conditional
#' effect is not significant.
#'
#' @details This function uses the
#' pseudo Johnson-Neyman approach
#' proposed by Hayes (2022) to find the
#' values of a moderator at which a
#' conditional effect is
#' "nearly just significant" based on
#' confidence interval. If an effect is
#' moderated, there will be two such
#' points (though one can be very large
#' or small) forming a range.
#' The conditional effect
#' is not significant within this range,
#' and significant outside this range,
#' based on the confidence interval.
#'
#' This function receives the output
#' of [cond_indirect_effects()]
#' and search for, within
#' a specific range, the two values of
#' the moderator at which
#' the conditional effect is "nearly just significant",
#' that is, the confidence interval
#' "nearly touches" zero.
#'
#' Note that numerical method is used
#' to find the points. Therefore,
#' strictly speaking, the effects at
#' the end points are still either
#' significant or not significant, even
#' if the confidence limit is very close
#' to zero.
#'
#' Though numerical method is used,
#' if the test is conducted using the
#' standard error (see below), the result is
#' equivalent to the (true)
#' Johnson-Neyman (1936) probing.
#' The function [johnson_neyman()] is
#' just an alias to [pseudo_johnson_neyman()],
#' with the name consistent with what
#' it does in this special case.
#'
#' ## Supported Methods
#'
#' This function supports models fitted
#' by [lm()], [lavaan::sem()],
#' and [semTools::sem.mi()]. This function
#' also supports both bootstrapping
#' and Monte Carlo confidence intervals.
#' It also supports conditional
#' direct paths (no mediator) and
#' conditional indirect paths (with one
#' or more mediator), with `x` and/or
#' `y` standardized.
#'
#' ## Requirements
#'
#' To be eligible for using this function,
#' one of these conditions must be met:
#'
#' - One form of confidence intervals
#' (e.g, bootstrapping or Monte Carlo)
#' must has been requested (e.g.,
#' setting `boot_ci = TRUE` or
#' `mc_ci = TRUE`) when calling
#' [cond_indirect_effects()].
#'
#' - Tests can be done using stored
#' standard errors: A path with no
#' mediator and both the `x`- and
#' `y`-variables are not standardized.
#'
#' For pre-computed confidence intervals,
#' the confidence level of the confidence
#' intervals adopted when calling
#' [cond_indirect_effects()] will be used
#' by this function.
#'
#' For tests conducted by standard
#' errors, the argument `level` is used
#' to control the level of significance.
#'
#' ## Possible failures
#'
#' Even if a path has only one moderator,
#' it is possible that no solution, or
#' more than one solution, is/are found
#' if the relation between this moderator
#' and the conditional effect is not linear.
#'
#' Solution may also be not found if
#' the conditional effect is significant
#' over a wide range of value of the
#' moderator.
#'
#' It is advised to use [plot_effect_vs_w()]
#' to examine the relation between the
#' effect and the moderator first before
#' calling this function.
#'
#' ## Speed
#'
#' Note that, for conditional indirect
#' effects, the search can be slow
#' because the confidence interval needs
#' to be recomputed for each new value
#' of the moderator.
#'
#' ## Limitations
#'
#' - This function currently only supports
#' a path with only one moderator,
#'
#' - This function does not yet support
#' multigroup models.
#'
#' @return
#' A list of the class `pseudo_johnson_neyman`
#' (with a print method, [print.pseudo_johnson_neyman()]).
#' It has these major elements:
#'
#' - `cond_effects`: An output of
#' [cond_indirect_effects()] for the
#' two levels of the moderator found.
#'
#' - `w_min_valid`: Logical. If `TRUE`,
#'  the conditional effect is just
#'  significant at the lower level of
#'  the moderator found,
#'  and so is significant below this point.
#'  If `FALSE`, then the lower level of
#'  the moderator found is just the
#'  lower bound of the range searched,
#'  that is, `w_lower`.
#'
#' - `w_max_valid`: Logical. If `TRUE`,
#'  the conditional effect is just
#'  significant at the higher level of
#'  the moderator found,
#'  and so is significant above this point.
#'  If `FALSE`, then the higher level of
#'  the moderator found is just the
#'  upper bound of the range searched,
#'  that is, `w_upper`.
#'
#' @param object A
#' `cond_indirect_effects`-class object,
#' which is the output of [cond_indirect_effects()].
#'
#' @param w_lower The smallest value of
#' the moderator when doing the search.
#' If set to `NULL,` the default, it
#' will be 10 standard deviations
#' below mean, which should be small
#' enough.
#'
#' @param w_upper The largest value of
#' the moderator when doing the search.
#' If set to `NULL,` the default, it
#' will be 10 standard deviations
#' above mean, which should be large
#' enough.
#'
#' @param optimize_method The optimization
#' method to be used. Either
#' `"uniroot"` (the default) or
#' `"optimize"`, corresponding to
#' [stats::uniroot()] and
#' [stats::optimize()], respectively.
#'
#' @param extendInt Used by
#' [stats::uniroot()]. If `"no"`, then
#' search will be conducted strictly
#' within `c(w_lower, w_upper)`. Otherwise,
#' the range is extended based on this
#' argument if the solution is not found.
#' Please refer to [stats::uniroot()]
#' for details.
#'
#' @param tol The tolerance level used
#' by both [stats::uniroot()] and
#' [stats::optimize()].
#'
#' @param x The output of
#' [pseudo_johnson_neyman()].
#'
#' @param digits Number of digits to
#' display. Default is 3.
#'
#' @param level The level of confidence
#' of the confidence level. One minus
#' this level is the level of significance.
#' Default is .95, equivalent to a
#' level of significance of .05.
#'
#' @param ... Other arguments. Not used.
#'
#' @references
#' Johnson, P. O., & Neyman, J. (1936). Test of certain linear hypotheses and their application to some educational problems. *Statistical Research Memoirs, 1*, 57--93.
#'
#' Hayes, A. F. (2022). *Introduction to mediation, moderation, and conditional process analysis: A regression-based approach* (Third edition). The Guilford Press.
#'
#'
#' @seealso [cond_indirect_effects()]
#'
#' @examples
#'
#' library(lavaan)
#'
#' dat <- data_med_mod_a
#' dat$wx <- dat$x * dat$w
#' mod <-
#' "
#' m ~ x + w + wx
#' y ~ m + x
#' "
#' fit <- sem(mod, dat)
#'
#' # In real research, R should be 2000 or even 5000
#' # In real research, no need to set parallel and progress to FALSE
#' # Parallel processing is enabled by default and
#' # progress is displayed by default.
#' boot_out <- do_boot(fit,
#'                     R = 40,
#'                     seed = 4314,
#'                     parallel = FALSE,
#'                     progress = FALSE)
#' out <- cond_indirect_effects(x = "x", y = "y", m = "m",
#'                              wlevels = "w",
#'                              fit = fit,
#'                              boot_ci = TRUE,
#'                              boot_out = boot_out)
#'
#' # Visualize the relation first
#' plot_effect_vs_w(out)
#'
#' out_jn <- pseudo_johnson_neyman(out)
#' out_jn
#'
#' # Plot the range
#' plot_effect_vs_w(out_jn$cond_effects)
#'
#' @export
#'

pseudo_johnson_neyman <- function(object = NULL,
                                  w_lower = NULL,
                                  w_upper = NULL,
                                  optimize_method = c("uniroot", "optimize"),
                                  extendInt = c("no", "yes", "downX", "upX"),
                                  tol = .Machine$double.eps^0.25,
                                  level = .95) {
    optimize_method <- match.arg(optimize_method)
    if (inherits(object, "cond_indirect_effects")) {
        if (cond_indirect_effects_has_groups(object)) {
            stop("Multigroup models are not supported.")
          }
        # Do not use update() because it is not reliable in this context
        x <- attr(object, "x", exact = TRUE)
        y <- attr(object, "y", exact = TRUE)
        m <- attr(object, "m", exact = TRUE)
        wlevels0 <- attr(object, "wlevels", exact = TRUE)
        w <- colnames(wlevels0)[1]
        fit <- attr(object, "fit", exact = TRUE)
        full_output_i <- attr(object, "full_output", exact = TRUE)[[1]]
        out_call <- attr(object, "call", exact = TRUE)
        boot_out <- attr(object, "boot_out", exact = TRUE)
        mc_out <- attr(object, "mc_out", exact = TRUE)
        has_boot_out <- !is.null(boot_out)
        has_mc_out <- !is.null(mc_out)
        ci_type <- ifelse(has_boot_out, "boot",
                          ifelse(has_mc_out, "mc",
                                 NA))
        boot_ci <- has_boot_out
        mc_ci <- has_mc_out
        standardized_x <- full_output_i$standardized_x
        standardized_y <- full_output_i$standardized_y
        se_out <- cond_effects_original_se(object)
        has_se <- !is.null(se_out)
        if (ncol(wlevels0) != 1) {
            stop("Support only one moderator.")
          }
        if (!is.numeric(wlevels0[, 1])) {
            stop("Support only numeric moderator.")
          }
        if (!has_boot_out && !has_mc_out && !has_se) {
            stop("Confidence intervals or SEs not in 'object'.")
          }
      } else {
        stop("'object' needs to be an output of cond_indirect_effects().")
      }
    w_tmp <- mod_levels(w = w,
                        fit = fit,
                        w_method = "sd",
                        sd_from_mean = c(-10, 10))
    w_tmp <- range(w_tmp[, w])
    if (is.null(w_lower)) {
        w_lower <- w_tmp[1]
      }
    if (is.null(w_upper)) {
        w_upper <- w_tmp[2]
      }
    wlevel_interval <- c(w_lower, w_upper)
    w_lower_ci <- pseudo_johnson_neyman_one_bound(w0 = w_lower,
                                                  object = object,
                                                  type = "ci",
                                                  level = level)
    w_upper_ci <- pseudo_johnson_neyman_one_bound(w0 = w_upper,
                                                  object = object,
                                                  type = "ci",
                                                  level = level)
    w_increasing <- (w_upper_ci[1, 2] > w_lower_ci[1, 2])
    if (w_increasing) {
        w_lb_0 <- list(w = w_lower, objective = w_lower_ci[1, 2])
        w_ub_0 <- list(w = w_upper, objective = w_upper_ci[1, 1])
      } else {
        w_lb_0 <- list(w = w_upper, objective = w_upper_ci[1, 2])
        w_ub_0 <- list(w = w_lower, objective = w_lower_ci[1, 1])
      }
    if (optimize_method == "uniroot") {
        w_lb <- tryCatch(pseudo_johnson_neyman_uniroot(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "lower",
                            extendInt = extendInt,
                            tol = tol,
                            level = level),
                         error = function(e) e)
        if (inherits(w_lb, "error")) {
            w_lb <- w_lb_0
            names(w_lb) <- c("root", "f.root")
          }
        w_ub <- tryCatch(pseudo_johnson_neyman_uniroot(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "upper",
                            extendInt = extendInt,
                            tol = tol,
                            level = level),
                         error = function(e) e)
        if (inherits(w_ub, "error")) {
            w_ub <- w_ub_0
            names(w_ub) <- c("root", "f.root")
          }
        w_df <- rbind(data.frame(w_lb[c("root", "f.root")]),
                      data.frame(w_ub[c("root", "f.root")]))
      }
    if (optimize_method == "optimize") {
        w_lb <- tryCatch(pseudo_johnson_neyman_optimize(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "lower",
                            tol = tol,
                            level = level),
                         error = function(e) e)
        w_ub <- tryCatch(pseudo_johnson_neyman_optimize(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "upper",
                            tol = tol,
                            level = level),
                         error = function(e) e)
        if (inherits(w_lb, "error") || inherits(w_ub, "error")) {
            stop("The search failed. Try setting optimize_method to 'uniroot'.")
          }
        w_df <- rbind(data.frame(w_lb),
                      data.frame(w_ub))
      }
    colnames(w_df) <- c("w", "fx")
    w_df <- w_df[order(w_df$w, decreasing = TRUE), ]
    w_min <- min(w_df$w)
    w_max <- max(w_df$w)
    w_min_valid <- isTRUE(all.equal(w_df[2, "fx"], 0, tolerance = 1e-5))
    w_max_valid <- isTRUE(all.equal(w_df[1, "fx"], 0, tolerance = 1e-5))
    w_tmp <- mod_levels(w = w,
                        fit = fit,
                        values = c(Low = w_min, High = w_max))
    w_lower <- min(w_min, w_lower)
    w_upper <- max(w_max, w_upper)
    out_cond <- cond_indirect_effects(wlevels = w_tmp,
                                      x = x,
                                      y = y,
                                      m = m,
                                      fit = fit,
                                      boot_ci = boot_ci,
                                      boot_out = boot_out,
                                      mc_ci = mc_ci,
                                      mc_out = mc_out,
                                      standardized_x = standardized_x,
                                      standardized_y = standardized_y,
                                      level = level)
    out <- list(cond_effects = out_cond,
                w_min_valid = w_min_valid,
                w_max_valid = w_max_valid,
                w_range_lb = w_min,
                w_range_ub = w_max,
                w_lower = w_lower,
                w_upper = w_upper,
                level = level)
    class(out) <- c("pseudo_johnson_neyman", class(out))
    out
  }

#' @rdname pseudo_johnson_neyman
#' @export
johnson_neyman <- pseudo_johnson_neyman

#' @noRd

pseudo_johnson_neyman_uniroot <- function(object,
                                          wlevel_interval,
                                          which = c("lower", "upper"),
                                          extendInt = c("no", "yes", "downX", "upX"),
                                          tol = .Machine$double.eps^0.25,
                                          maxiter = 1000,
                                          level = .95) {
    w_b <- stats::uniroot(pseudo_johnson_neyman_one_bound,
                          interval = wlevel_interval,
                          object = object,
                          which = which,
                          type = "limit",
                          level = level,
                          extendInt = extendInt,
                          tol = tol,
                          maxiter = maxiter)
    tmp <- pseudo_johnson_neyman_one_bound(w0 = w_b$root,
                                           object = object,
                                           which = which,
                                           type = "ci",
                                           level = level)
    chk <- switch(which,
                  lower = (tmp[2] < 0) && isTRUE(all.equal(tmp[2], 0)),
                  upper = (tmp[1] > 0) && isTRUE(all.equal(tmp[1], 0)))
    if (chk) {
        adj_tmp <- switch(which,
                          lower = -1 * abs(tmp[2]),
                          upper = abs(tmp[1]))
        w_b <- stats::uniroot(pseudo_johnson_neyman_one_bound,
                               interval = c(w_b$root * (1 - 1e-2),
                                            w_b$root * (1 + 1e-2)),
                               object = object,
                               which = which,
                               type = "limit",
                               adj = adj_tmp,
                               level = level,
                               extendInt = "no",
                               tol = tol,
                               maxiter = maxiter)
      }
    w_b
  }

#' @noRd

pseudo_johnson_neyman_optimize <- function(object,
                                           wlevel_interval,
                                           which = c("lower", "upper"),
                                           tol = .Machine$double.eps^0.25,
                                           maximum = FALSE,
                                           level = .95) {
    w_b <- stats::optimize(pseudo_johnson_neyman_one_bound,
                              interval = wlevel_interval,
                              object = object,
                              which = which,
                              type = "distance",
                              maximum = maximum,
                              tol = tol,
                              level = level)
    tmp <- pseudo_johnson_neyman_one_bound(w0 = w_b$minimum,
                                           object = object,
                                           which = which,
                                           type = "ci",
                                           level = level)
    chk <- switch(which,
                  lower = (tmp[2] < 0) && isTRUE(all.equal(tmp[2], 0)),
                  upper = (tmp[1] > 0) && isTRUE(all.equal(tmp[1], 0)))
    if (chk) {
        adj_tmp <- switch(which,
                     lower = sign(tmp[2]) * sqrt(abs(tmp[2])),
                     upper = sign(tmp[1]) * sqrt(abs(tmp[1])))
        w_b <- stats::optimize(pseudo_johnson_neyman_one_bound,
                                  interval = c(w_b$minimum * (1 - 1e-2),
                                               w_b$minimum * (1 + 1e-2)),
                                  object = object,
                                  which = which,
                                  type = "distance",
                                  adj = adj_tmp,
                                  level = level,
                                  maximum = maximum,
                                  tol = tol)
      }
    w_b
  }




#' @describeIn pseudo_johnson_neyman Print
#' method for output of [pseudo_johnson_neyman()].
#'
#' @export

print.pseudo_johnson_neyman <- function(x, digits = 3, ...) {
    out_cond <- x$cond_effects
    full_output_i <- attr(out_cond, "full_output", exact = TRUE)[[1]]
    w <- names(full_output_i$wvalues)[1]
    # ci_level <- full_output_i$level
    ci_level <- x$level
    sig_level <- 1 - ci_level
    w_range <- c(x$w_lower, x$w_upper)
    w_range_lb <- min(w_range)
    w_range_ub <- max(w_range)
    w_min_valid <- x$w_min_valid
    w_max_valid <- x$w_max_valid
    w_found_lb <- x$w_range_lb
    w_found_ub <- x$w_range_ub
    w_lb_str <- formatC(w_found_lb, digits = digits, format = "f")
    w_ub_str <- formatC(w_found_ub, digits = digits, format = "f")
    w_range_lb_str <- formatC(w_range_lb, digits = digits, format = "f")
    w_range_ub_str <- formatC(w_range_ub, digits = digits, format = "f")
    cat("\n")
    if (!is.null(cond_effects_original_se(out_cond))) {
        cat("== Johnson-Neyman Probing ==\n")
      } else {
        cat("== Pseudo Johnson-Neyman Probing ==\n")
      }
    cat("\n")
    str_tmp <- character(0)
    str_just <- paste0("- On 'Sig': A conditional effect at the bound of the range ",
                       "may be marked as ",
                       "significant or not significant. ",
                       "However, it can be treated as 'just significant' if ",
                       "its confidence interval practically 'touches' zero.")
    if (w_min_valid && w_max_valid) {
        str_tmp <- c(str_tmp,
                   paste0("The conditional effect is ",
                          "not significant when ",
                          w, " is greater than ",
                          w_lb_str, " and less than ", w_ub_str,
                          ", and is significant when ",
                          w, " is outside this range, ",
                          "at ", sig_level, " level of significance."))
      }
    if (!w_min_valid && w_max_valid) {
        str_tmp <- c(str_tmp,
                   paste0("The conditional effect is ",
                          "not significant when ",
                          w, " is greater than ",
                          w_lb_str, " and less than ", w_ub_str,
                          ", and is significant when ",
                          w, " is greater than ", w_ub_str, ", ",
                          "at ", sig_level, " level of significance."))
      }
    if (w_min_valid && !w_max_valid) {
        str_tmp <- c(str_tmp,
                   paste0("The conditional effect is ",
                          "not significant when ",
                          w, " is greater than ",
                          w_lb_str, " and less than ", w_ub_str,
                          ", and is significant when ",
                          w, " is less than ", w_lb_str, ", ",
                          "at ", sig_level, " level of significance."))
      }
    if (!w_min_valid && !w_max_valid) {
        str_tmp <- c(str_tmp,
                   paste0("The conditional effect is ",
                          "not significant when ",
                          w, " is greater than ",
                          w_lb_str, " and less than ", w_ub_str,
                          ", ",
                          "at ", sig_level, " level of significance."))
      }
    if (!w_min_valid || !w_max_valid) {
        str_tmp <- c(str_tmp, "", "-- Note --")
      }
    if (!w_min_valid) {
        str_tmp <- c(str_tmp,
                     paste0("- The lower bound of the range of ",
                            "nonsignificance is below ",
                            "the range being searched (",
                            w_range_lb_str, " to ", w_range_ub_str, "). ",
                            "Set a lower value for 'w_lower' if necessary."))
      }
    if (!w_max_valid) {
        str_tmp <- c(str_tmp,
                     paste0("- The upper bound of the range of ",
                            "nonsignificance is above ",
                            "the range being searched (",
                            w_range_lb_str, " to ", w_range_ub_str, "). ",
                            "Set a higher value for 'w_upper' if necessary."))
      }
    tmp <- as.vector(unlist(stats::confint(out_cond)))
    tmp2 <- sapply(tmp,
              function(x) {
                  isTRUE(all.equal(x, 0, tolerance = 1e-5))
                })
    if (any(tmp2)) {
        str_tmp <- c(str_tmp,
                    str_just)
      }
    str_tmp_final <- strwrap(str_tmp,
                            exdent = 0)
    cat(str_tmp_final, sep = "\n")
    print(out_cond)
    invisible(x)
  }

#' @noRd
# Search for one bound of the range
pseudo_johnson_neyman_one_bound <- function(w0,
                                            object = NULL,
                                            x = NULL,
                                            y = NULL,
                                            m = NULL,
                                            w = NULL,
                                            fit = NULL,
                                            boot_ci = FALSE,
                                            boot_out = NULL,
                                            mc_ci = FALSE,
                                            mc_out = NULL,
                                            standardized_x = FALSE,
                                            standardized_y = FALSE,
                                            which = c("lower", "upper"),
                                            type = c("distance", "limit", "ci", "est", "full_out"),
                                            adj = 0,
                                            level = .95) {
    which <- match.arg(which)
    type <- match.arg(type)
    if (inherits(object, "cond_indirect_effects")) {
        # Do not use update() because it is not reliable in this context
        x <- attr(object, "x", exact = TRUE)
        y <- attr(object, "y", exact = TRUE)
        m <- attr(object, "m", exact = TRUE)
        wlevels0 <- attr(object, "wlevels", exact = TRUE)
        w <- colnames(wlevels0)[1]
        fit <- attr(object, "fit", exact = TRUE)
        full_output_i <- attr(object, "full_output", exact = TRUE)[[1]]
        out_call <- attr(object, "call", exact = TRUE)
        boot_out <- attr(object, "boot_out", exact = TRUE)
        mc_out <- attr(object, "mc_out", exact = TRUE)
        has_boot_out <- !is.null(boot_out)
        has_mc_out <- !is.null(mc_out)
        ci_type <- ifelse(has_boot_out, "boot",
                          ifelse(has_mc_out, "mc",
                                 NA))
        boot_ci <- has_boot_out
        mc_ci <- has_mc_out
        standardized_x <- full_output_i$standardized_x
        standardized_y <- full_output_i$standardized_y
      }
    w_i <- w0
    names(w_i) <- w
    out <- cond_indirect(wvalues = w_i,
                         x = x,
                         y = y,
                         m = m,
                         fit = fit,
                         standardized_x = standardized_x,
                         standardized_y = standardized_y,
                         boot_ci = boot_ci,
                         boot_out = boot_out,
                         mc_ci = mc_ci,
                         mc_out = mc_out,
                         level = level)
    out1 <- switch(which, lower = stats::confint(out, level = level)[1, 2] + adj,
                          upper = stats::confint(out, level = level)[1, 1] - adj)
    return(switch(type,
                  distance = out1^2,
                  limit = out1,
                  ci = stats::confint(out, level = level),
                  est = stats::coef(out),
                  full_out = out))
  }
