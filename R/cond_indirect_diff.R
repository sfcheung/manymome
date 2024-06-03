#' @title Differences In Conditional
#' Indirect Effects
#'
#' @description Compute the difference
#' in conditional indirect effects
#' between two sets of levels of the
#' moderators.
#'
#' @details
#'
#' Ths function takes the output of
#' [cond_indirect_effects()] and
#' computes the difference in
#' conditional indirect effects between
#' any two rows, that is, between levels
#' of the moderator, or two sets of
#' levels of the moderators when the
#' path has more than one moderator.
#'
#' The difference is meaningful when the
#' difference between the two levels or
#' sets of levels are meaningful. For
#' example, if the two levels are the
#' mean of the moderator and one
#' standard deviation above mean of the
#' moderator, then this difference is
#' the change in indirect effect when
#' the moderator increases by one
#' standard deviation.
#'
#' If the two levels are 0 and 1, then
#' this difference is the index of
#' moderated mediation as proposed by
#' Hayes (2015). (This index can also be
#' computed directly by
#' [index_of_mome()], designed
#' specifically for this purpose.)
#'
#' The function can also compute the
#' change in the standardized indirect
#' effect between two levels of a
#' moderator or two sets of levels of
#' the moderators.
#'
#' This function is intended to be a
#' general purpose function that allows
#' users to compute the difference
#' between any two levels or sets of
#' levels that are meaningful in a
#' context.
#'
#' This function itself does not set the
#' levels of comparison. The levels to
#' be compared need to be set when
#' calling [cond_indirect_effects()].
#' This function extracts required
#' information from the output of
#' [cond_indirect_effects()].
#'
#' If bootstrap or Monte Carlo
#' estimates are available
#' in the input or bootstrap
#' or Monte Carlo confidence
#' intervals are requested in calling
#' [cond_indirect_effects()],
#' [cond_indirect_diff()] will also form
#' the bootstrap confidence
#' interval for the difference in
#' conditional indirect effects
#' using the stored estimates.
#'
#' If bootstrap confidence interval is
#' to be formed and both effects used
#' the same type of interval, then that
#' type will be used. Otherwise,
#' percentile confidence interval will
#' be formed.
#'
#' @return A `cond_indirect_diff`-class
#' object. This class has a `print`
#' method
#' ([print.cond_indirect_diff()]), a
#' `coef` method
#' ([coef.cond_indirect_diff()]), and a
#' `confint` method
#' ([confint.cond_indirect_diff()]).
#'
#'
#' @param output A
#' `cond_indirect_effects`-class object:
#' The output of
#' [cond_indirect_effects()].
#'
#' @param from A row number of `output`.
#'
#' @param to A row number of `output`.
#' The change in indirect effects is
#' computed by the change in the
#' level(s) of the moderator(s) from Row
#' `from` to Row `to`.
#'
#' @param level The level of confidence
#' for the confidence
#' interval. Default is .95.
#'
#'
#' @seealso [index_of_mome()] for
#' computing the index of moderated
#' mediation, [index_of_momome()] for
#' computing the index of moderated
#' moderated mediation,
#' [cond_indirect_effects()],
#' [mod_levels()], and
#' [merge_mod_levels()] for preparing
#' the levels to be compared.
#'
#' @references
#' Hayes, A. F. (2015). An index and test of linear moderated mediation.
#' *Multivariate Behavioral Research, 50*(1), 1-22.
#' \doi{10.1080/00273171.2014.962683}
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' dat$xw1 <- dat$x * dat$w1
#' mod <-
#' "
#' m1 ~ a * x  + f * w1 + d * xw1
#' y  ~ b * m1 + cp * x
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Create levels of w1, the moderators
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#'
#' # Conditional effects from x to y when w1 is equal to each of the levels
#' boot_out <- fit2boot_out_do_boot(fit, R = 40, seed = 4314, progress = FALSE)
#' out <- cond_indirect_effects(x = "x", y = "y", m = "m1",
#'                              wlevels = w1levels, fit = fit,
#'                              boot_ci = TRUE, boot_out = boot_out)
#' out
#' out_ind <- cond_indirect_diff(out, from = 2, to = 1)
#' out_ind
#' coef(out_ind)
#' confint(out_ind)
#'
#'
#'
#' @export
#'
#' @describeIn cond_indirect_diff
#' Compute the difference in in
#' conditional indirect effect between
#' two rows in the output of
#' [cond_indirect_effects()].
#'
#' @order 1

cond_indirect_diff <- function(output,
                          from = NULL,
                          to = NULL,
                          level = .95) {
    has_wlevels <- cond_indirect_effects_has_wlevels(output)
    has_groups <- cond_indirect_effects_has_groups(output)
    if (has_wlevels && has_groups) {
        stop("Objects with both wlevels and groups not yet supported")
      }
    if (missing(output)) {
        stop("'output' is missing.")
      }
    if (!inherits(output, "cond_indirect_effects")) {
        stop("'output' is not a 'cond_indirect_effects'-class object.")
      }
    if (is.null(from) || is.null(to)) {
        stop("Either 'from' or 'to' (or both) is/are missing")
      }
    if ((level >= 1) || (level <= .50)) {
        stop("'level' must be greater than .50 and less than 1.")
      }
    output_full <- attr(output, "full_output")
    output_full_from <- output_full[[from]]
    output_full_to <- output_full[[to]]
    boot_i_from <- output_full_from$boot_indirect
    boot_i_to <- output_full_to$boot_indirect
    mc_i_from <- output_full_from$mc_indirect
    mc_i_to <- output_full_to$mc_indirect
    effect_diff <- stats::coef(output_full_to) - stats::coef(output_full_from)
    if (is.null(boot_i_from) || is.null(boot_i_to)) {
        has_boot <- FALSE
        boot_type <- NULL
      } else {
        has_boot <- TRUE
      }
    if (is.null(mc_i_from) || is.null(mc_i_to)) {
        has_mc <- FALSE
      } else {
        has_mc <- TRUE
      }
    if (all(has_mc, has_boot)) stop("Cannot form both Monte Carlo and bootstrapping confidence intervals.")
    if (has_mc) {
        mc_diff <- mc_i_to - mc_i_from
        mc_diff_ci <- boot_ci_internal(t0 = effect_diff,
                                       t = mc_diff,
                                       level = level,
                                       boot_type = "perc")
        mc_diff_se <- stats::sd(mc_diff, na.rm = TRUE)
      } else {
        mc_diff <- NA
        mc_diff_ci <- c(NA, NA)
        mc_diff_se <- NA
      }
    if (has_boot) {
        if (identical(output_full_from$boot_type,
                      output_full_to$boot_type)) {
            boot_type <- output_full_from$boot_type
          } else {
            boot_type <- "perc"
          }
        boot_diff <- boot_i_to - boot_i_from
        boot_diff_ci <- boot_ci_internal(t0 = effect_diff,
                                t = boot_diff,
                                level = level,
                                boot_type = boot_type)
        boot_diff_p <- est2p(boot_diff)
        boot_diff_se <- stats::sd(boot_diff, na.rm = TRUE)
      } else {
        boot_diff <- NA
        boot_diff_ci <- c(NA, NA)
        boot_diff_p <- NA
        boot_diff_se <- NA
      }
    if (has_wlevels && !has_groups) {
        wlevels <- attr(output, "wlevels")
        wlevels_from <- wlevels[from, , drop = FALSE]
        wlevels_to <- wlevels[to, , drop = FALSE]
        final_from <- wlevels_from
        final_to <- wlevels_to
      }
    if (!has_wlevels && has_groups) {
        group_labels <- output$Group
        group_numbers <- output$Group_ID
        group_from <- paste0(group_labels[from], " [",
                             group_numbers[from], "]")
        group_to <- paste0(group_labels[to], " [",
                           group_numbers[to], "]")
        names(group_from) <- "Group"
        names(group_to) <- "Group"
        final_from <- group_from
        final_to <- group_to
      }
    if (has_wlevels && has_groups) {
        # TODO:
        # - Add support for objects with both wlevels and groups.
      }
    out_diff_ci <- c(NA, NA)
    out_diff_se <- NA
    if (has_mc) out_diff_ci <- mc_diff_ci
    if (has_boot) out_diff_ci <- boot_diff_ci
    if (has_mc) out_diff_se <- mc_diff_se
    if (has_boot) out_diff_se <- boot_diff_se
    ci_type <- NA
    if (has_mc) ci_type <- "mc"
    if (has_boot) ci_type <- "boot"
    # TOCHECK (BC): Store boot_type [DONE]
    out <- list(index = effect_diff,
                ci = out_diff_ci,
                pvalue = boot_diff_p,
                se = out_diff_se,
                level = level,
                from = final_from,
                to = final_to,
                output = output[c(to, from), ],
                boot_diff = boot_diff,
                mc_diff = mc_diff,
                has_groups = has_groups,
                has_wlevels = has_wlevels,
                boot_type = boot_type,
                ci_type = ci_type)
    class(out) <- c("cond_indirect_diff", class(out))
    out
  }

#' @title Print the Output of
#' 'cond_indirect_diff'
#'
#' @description Print the output of
#' [cond_indirect_diff()].
#'
#' @details The `print` method of the
#' `cond_indirect_diff`-class object.
#'
#' If bootstrapping confidence interval
#' was requested, this method has the
#' option to print a
#' *p*-value computed by the
#' method presented in Asparouhov and Muthén (2021).
#' Note that this *p*-value is asymmetric
#' bootstrap *p*-value based on the
#' distribution of the bootstrap estimates.
#' It is not computed based on the
#' distribution under the null hypothesis.
#'
#' For a *p*-value of *a*, it means that
#' a 100(1 - *a*)% bootstrapping confidence
#' interval
#' will have one of its limits equal to
#' 0. A confidence interval
#' with a higher confidence level will
#' include zero, while a confidence
#' interval with a lower confidence level
#' will exclude zero.
#'
#' @return It returns `x` invisibly.
#' Called for its side effect.
#'
#' @param x The output of
#' [cond_indirect_diff()].
#'
#' @param digits The number of decimal
#' places in the printout.
#'
#' @param pvalue Logical. If `TRUE`,
#' asymmetric *p*-value based on
#' bootstrapping will be printed if
#' available. Default is `FALSE.`
#'
#' @param pvalue_digits Number of decimal
#' places to display for the *p*-value.
#' Default is 3.
#'
#' @param se Logical. If `TRUE` and
#' confidence intervals are available,
#' the standard errors of the estimates
#' are also printed. They are simply the
#' standard deviations of the bootstrap
#' estimates or Monte Carlo simulated
#' values, depending on the method used
#' to form the confidence intervals.
#'
#' @param ... Optional arguments.
#' Ignored.
#'
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' @seealso [cond_indirect_diff()]
#'
#' @export

print.cond_indirect_diff <- function(x,
                                     digits = 3,
                                     pvalue = FALSE,
                                     pvalue_digits = 3,
                                     se = FALSE,
                                     ...) {
    full_output_attr <- attr(x$output, "full_output")[[1]]
    has_groups <- is.numeric(full_output_attr$group_number)
    has_wlevels <- !is.null(full_output_attr$wvalues)
    print(x$output, digits = digits, annotation = FALSE, ...)
    x_type <- x$type
    if (!is.null(x_type)) {
        tmp <- switch(x_type,
            index_of_mome = "\n== Index of Moderated Mediation ==",
            index_of_momome = "\n== Index of Moderated Moderated Mediation =="
          )
      } else {
        tmp <- "\n== Difference in Conditional Indirect Effect =="
      }
    cat(tmp)
    cat("\n")
    xto0 <- sapply(x$to, function(xx) {
                    ifelse(is.numeric(xx),
                           formatC(xx, digits = digits, format = "f"),
                           xx)
                  })
    xfrom0 <- sapply(x$from, function(xx) {
                    ifelse(is.numeric(xx),
                           formatC(xx, digits = digits, format = "f"),
                           xx)
                  })
    tofrom <- rbind(xto0, xfrom0)
    rownames(tofrom) <- paste0(c("To:   ", "From: "),
                               c(rownames(x$to), rownames(x$from)))
    if (!is.null(x_type)) {
        if (x_type == "index_of_mome") {
            cat("\n")
            cat("Levels compared: Row 1 - Row 2")
            cat("\n")
          }
        if (x_type == "index_of_momome") {
            cat("\n")
            cat("Levels compared:\n")
            cat("(Row 1 - Row 2) - (Row 3 - Row 4)")
            cat("\n")
          }
      } else {
        cat("\nLevels: \n")
        print(tofrom, quote = FALSE)
        if (!has_wlevels && has_groups) {
            tmp <- paste0("Levels compared: ",
                          xto0,
                          " - ",
                          xfrom0)
          } else {
            tmp <- "Levels compared: Row 1 - Row 2"
          }
        cat("\n", tmp, "\n", sep = "")
      }
    index_df <- data.frame(x = full_output_attr$x,
                           y = full_output_attr$y,
                           Change = formatC(x$index, digits = digits, format = "f"))
    has_ci <- !all(is.na(x$ci))
    ci_type <- x$ci_type
    if (has_ci) {
        index_df$CI.lo <- formatC(x$ci[1], digits = digits, format = "f")
        index_df$CI.hi <- formatC(x$ci[2], digits = digits, format = "f")
        if (!identical(NA, x$boot_diff) && !is.na(x$pvalue) &&
            pvalue) {
            index_df$pvalue <- formatC(x$pvalue,
                                       digits = pvalue_digits,
                                       format = "f")
          }
        if ((!identical(NA, x$boot_diff) || !identical(NA, x$mc_diff)) &&
            !is.na(x$se) &&
            se) {
            index_df$SE <- formatC(x$se,
                                   digits = digits,
                                   format = "f")
          }
      }
    if (!is.null(x_type)) {
        rownames(index_df) <- "Index"
        colnames(index_df)[colnames(index_df) %in% "Change"] <- "Index"
      } else {
        rownames(index_df) <- "Change"
      }
    if (!is.null(x_type)) {
        tmp <- "\n"
      } else {
        tmp <- "\nChange in Indirect Effect:\n\n"
      }
    cat(tmp)
    print(index_df, nd = digits)
    cat("\n ")
    if (has_ci) {
        boot_type <- x$boot_type
        tmp <- switch(ci_type,
                      mc = "Monte Carlo",
                      boot = switch(boot_type,
                                    perc = "percentile",
                                    bc = "bias-corrected"))
        cat(strwrap(paste0("- [CI.lo, CI.hi]: ",
                          x$level * 100,
                          "% ", tmp, " confidence interval."), exdent = 3),
                          sep = "\n")
        if (!identical(NA, x$boot_diff) && !is.na(x$pvalue) &&
            pvalue) {
            cat(" - P-value is asymmetric bootstrap p-value.\n", sep = "")
          }
        if ((!identical(NA, x$boot_diff) || !identical(NA, x$mc_diff)) &&
            !is.na(x$se) &&
            se) {
            cat(" - [SE]: Standard error.\n", sep = "")
          }
      }
    if (full_output_attr$standardized_x) {
        cat(" - ", full_output_attr$x, " standardized.\n", sep = "")
      }
    if (full_output_attr$standardized_y) {
        cat(" - ", full_output_attr$y, " standardized.\n", sep = "")
      }
    cat("\n")
    invisible(x)
  }

#' @title Print the Output of
#' 'cond_indirect_diff()'
#'
#' @description Extract the change in
#' conditional indirect effect.
#'
#' @details The `coef` method of the
#' `cond_indirect_diff`-class object.
#'
#' @return Scalar: The change of
#' conditional indirect effect in
#' `object`.
#'
#' @param object The output of
#' [cond_indirect_diff()].
#'
#' @param ... Optional arguments.
#' Ignored.
#'
#' @seealso [cond_indirect_diff()]
#'
#' @export

coef.cond_indirect_diff <- function(object, ...) {
    full_output_attr <- attr(object$output, "full_output")[[1]]
    out <- object$index
    names(out) <- paste0(full_output_attr$y,
                         "~",
                         paste0(rev(full_output_attr$m), collapse = "~"),
                         "~",
                         full_output_attr$x)
    out
  }


#' @title Confidence Interval of the
#' Output of 'cond_indirect_diff()'
#'
#' @description Extract the confidence
#' interval the output of
#' [cond_indirect_diff()].
#'
#' @details The `confint` method of the
#' `cond_indirect_diff`-class object.
#'
#' The type of confidence intervals
#' depends on the call used to
#' create the object. This function
#' merely extracts the stored
#' confidence intervals.
#'
#' @return A one-row-two-column data
#' frame of the confidence limits. If
#' confidence interval is not available,
#' the limits are `NA`s.
#'
#' @param object The output of
#' [cond_indirect_diff()].
#'
#' @param parm Ignored.
#'
#' @param level The level of confidence
#' for the confidence
#' interval. Default is .95. Must match
#' the level of the stored confidence
#' interval.
#'
#' @param ... Optional arguments.
#' Ignored.
#'
#' @export

confint.cond_indirect_diff<- function(object, parm, level = .95, ...) {
    if (object$level != level) {
        stop("Requested level does not match stored level.")
      }
    full_output_attr <- attr(object$output, "full_output")[[1]]
    out <- data.frame(as.list(object$ci), check.names = FALSE)
    if (all(is.na(out))) out <- data.frame(ci.lower = NA, ci.upper = NA)
    rownames(out) <- paste0(full_output_attr$y,
                         "~",
                         paste0(rev(full_output_attr$m), collapse = "~"),
                         "~",
                         full_output_attr$x)
    out
  }
