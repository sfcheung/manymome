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
#' If bootstrap estimates are available
#' in the input or bootstrap confidence
#' intervals are requested in calling
#' [cond_indirect_effects()],
#' [cond_indirect_diff()] will also form
#' the percentile bootstrap confidence
#' interval for the difference in
#' conditional indirect effects.
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
#' for the bootstrap confidence
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
    effect_diff <- stats::coef(output_full_to) - stats::coef(output_full_from)
    if (is.null(boot_i_from) || is.null(boot_i_to)) {
        has_boot <- FALSE
      } else {
        has_boot <- TRUE
      }
    if (has_boot) {
        boot_diff <- boot_i_to - boot_i_from
        # levels0 <- c((1 - level) / 2, 1 - (1 - level) / 2)
        # boot_diff_ci <- stats::quantile(boot_diff, probs = levels0)
        boot_tmp <- list(t0 = effect_diff,
                         t = matrix(boot_diff, ncol = 1),
                         R = length(boot_diff))
        boot_diff_ci <- boot::boot.ci(boot_tmp,
                            type = "perc",
                            conf = level)$percent[4:5]
        names(boot_diff_ci) <- paste0(formatC(c(100 * (1 - level) / 2,
                                      100 * (1 - (1 - level) / 2)), 2,
                                      format = "f"), "%")
      } else {
        boot_diff <- NA
        boot_diff_ci <- c(NA, NA)
      }
    wlevels <- attr(output, "wlevels")
    wlevels_from <- wlevels[from, , drop = FALSE]
    wlevels_to <- wlevels[to, , drop = FALSE]
    out <- list(index = effect_diff,
                ci = boot_diff_ci,
                level = level,
                from = wlevels_from,
                to = wlevels_to,
                output = output[c(to, from), ],
                boot_diff = boot_diff)
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
#' @return It returns `x` invisibly.
#' Called for its side effect.
#'
#' @param x The output of
#' [cond_indirect_diff()].
#'
#' @param digits The number of decimal
#' places in the printout.
#'
#' @param ... Optional arguments.
#' Ignored.
#'
#' @seealso [cond_indirect_diff()]
#'
#' @export

print.cond_indirect_diff <- function(x, digits = 3, ...) {
    full_output_attr <- attr(x$output, "full_output")[[1]]
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
      }
    index_df <- data.frame(x = full_output_attr$x,
                           y = full_output_attr$y,
                           Change = formatC(x$index, digits = digits, format = "f"))
    has_ci <- !all(is.na(x$ci))
    if (has_ci) {
        index_df$CI.lo <- formatC(x$ci[1], digits = digits, format = "f")
        index_df$CI.hi <- formatC(x$ci[2], digits = digits, format = "f")
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
        cat(strwrap(paste0("- [CI.lo, CI.hi]: ",
                          x$level * 100,
                          "% percentile confidence interval."), exdent = 3),
                          sep = "\n")
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
#' for the bootstrap confidence
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
