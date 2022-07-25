#' @title Generalized Index of Moderated-Mediation
#'
#' @description Compute a generalized version of the index of
#'   moderated-mediation proposed by Hayes (2015).
#'
#' @details
#'
#' Ths function computes a generalized version of the index of
#' moderated-mediation
#' proposed by Hayes (2015).
#'
#' The variant proposed by Hayes is the change in the indirect effect
#' along a moderated pathway when a moderator increases by one unit.
#'
#' The generalized version is the change in the indirect effect
#' along a moderated pathway when a moderator increases by an amount
#' meaningful in a context.
#'
#' For example, the function can compute the change in the indirect effect
#' when a moderator increases by one standard deviation. We call this
#' variant the *standardized index of moderated-mediation*.
#'
#' The function can also compute the change in the standardized
#' indirect effect when a moderator increases by one unit. We call this
#' variant the *index of standardized moderated-mediation*.
#'
#' The change in the standardized
#' indirect effect when a moderator increases by one standardized deviation
#' is called the *standardized index of standardized moderated-mediation*.
#'
#' This function itself does not set the levels of comparison. The levels
#' to be compared need to be set when calling [cond_indirect_effects()].
#' This function extracts required information from the output of
#' [cond_indirect_effects()].
#'
#' @return
#' It returns an `index_of_mome`-class object. This class
#' has a `print` method ([print.index_of_mome()]), a `coef` method
#' ([coef.index_of_mome()]), and a `confint` method ([confint.index_of_mome()]).
#'
#'
#' @param output A `cond_indirect_effects`-class object: The output
#'               of [cond_indirect_effects()].
#' @param from A row number of `output`.
#' @param to A row number of `output`. The change in indirect effects
#'           is computed by the change in the level(s) of the moderator(s)
#'           from Row `from` to Row `to`.
#' @param level The level of confidence for the bootstrap confidence
#'    interval. Default is .95.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Create levels of w1, the moderators
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#'
#' # Conditional effects from x to y when w1 is equal to each of the levels
#' boot_out <- fit2boot_out_do_boot(fit, R = 100, seed = 4314)
#' out <- cond_indirect_effects(x = "x", y = "y", m = "m1",
#'                       wlevels = w1levels, fit = fit,
#'                       boot_ci = TRUE, boot_out = boot_out)
#' out
#' out_ind <- index_of_mome(out, from = 2, to = 3)
#' out_ind
#' coef(out_ind)
#' confint(out_ind)
#'
#'
#'
#' @export
#' @describeIn index_of_mome Compute the generalized index of
#'                           moderated-mediation.
#' @order 1

index_of_mome <- function(output,
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
    boot_i_from <- output_full_from$boot_i
    boot_i_to <- output_full_to$boot_i
    if (is.null(boot_i_from) || is.null(boot_i_to)) {
        stop("Bootstrap estimates not found. Was bootstrapping requested?")
      }
    boot_diff <- boot_i_to - boot_i_from
    levels0 <- c((1 - level) / 2, 1 - (1 - level) / 2)
    boot_diff_ci <- stats::quantile(boot_diff, probs = levels0)
    effect_diff <- stats::coef(output_full_to) - stats::coef(output_full_from)
    wlevels <- attr(output, "wlevels")
    wlevels_from <- wlevels[from, , drop = FALSE]
    wlevels_to <- wlevels[to, , drop = FALSE]
    out <- list(index = effect_diff,
                ci = boot_diff_ci,
                level = level,
                from = wlevels_from,
                to = wlevels_to,
                output = output[c(to, from), ])
    class(out) <- c("index_of_mome", class(out))
    out
  }

#' @title Print the Output of 'index_of_mome'
#'
#' @description Print the output of [index_of_mome()].
#'
#' @details The `print` method of the `index_of_mome`-class object.
#'
#' @return Return `x` invisibly. Called for its side effect.
#'
#' @param x The output of [index_of_mome()].
#' @param digits The number of decimal places in the printout.
#' @param ... Optional arguments. Ignored.
#' @seealso [index_of_mome()]
#' @export

print.index_of_mome <- function(x, digits = 3, ...) {
    full_output_attr <- attr(x$output, "full_output")[[1]]
    print(x$output, digits = digits, ...)
    cat("\n== Index of Moderated-Mediation ==")
    cat("\n")
    tofrom <- rbind(x$to, x$from)
    rownames(tofrom) <- paste0(c("To: ", "From: "),
                               rownames(tofrom))
    cat("\nLevels: \n")
    print(tofrom)
    index_df <- data.frame(x = full_output_attr$x,
                           y = full_output_attr$y,
                           Change = formatC(x$index, digits = digits, format = "f"),
                           CI.lo = formatC(x$ci[1], digits = digits, format = "f"),
                           CI.hi = formatC(x$ci[2], digits = digits, format = "f"))
    rownames(index_df) <- "Index"
    cat("\nChange in Indirect Effect:\n")
    print(index_df, nd = digits)
    cat("\n ")
    cat(strwrap(paste0("- [CI.lo, CI.hi]: ",
                       x$level,
                       "% percentile confidence interval."), exdent = 3),
                       sep = "\n")
    if (full_output_attr$standardized_x) {
        cat(" - ", full_output_attr$x, " standardized.\n", sep = "")
      }
    if (full_output_attr$standardized_y) {
        cat(" - ", full_output_attr$y, " standardized.\n", sep = "")
      }
    cat("\n")
    invisible(x)
  }

#' @title Print the Output of 'index_of_mome'
#'
#' @description Extract the Index of Moderated-Mediation.
#'
#' @details The `coef` method of the `index_of_mome`-class object.
#'
#' @return Scalar: The index of moderated-mediation in `object`.
#'
#' @param object The output of [index_of_mome()].
#' @param ... Optional arguments. Ignored.
#' @seealso [index_of_mome()]
#' @export

coef.index_of_mome <- function(object, ...) {
    full_output_attr <- attr(object$output, "full_output")[[1]]
    out <- object$index
    names(out) <- paste0(full_output_attr$y,
                         "~",
                         paste0(rev(full_output_attr$m), collapse = "~"),
                         "~",
                         full_output_attr$x)
    out
  }


#' @title Confidence Interval of the Output of 'index_of_mome'
#'
#' @description Extract the confidence interval the output of [index_of_mome()].
#'
#' @details The `confint` method of the `index_of_mome`-class object.
#'
#' @return A one-row-two-column data frame of the confidence limits.
#'
#' @param object The output of [index_of_mome()].
#' @param parm Ignored.
#' @param level The level of confidence for the bootstrap confidence
#'    interval. Default is .95. Must match the level of the stored
#'    confidence interval.
#' @param ... Optional arguments. Ignored.
#' @export

confint.index_of_mome<- function(object, parm, level = .95, ...) {
    if (object$level != level) {
        stop("Requested level does not match stored level.")
      }
    full_output_attr <- attr(object$output, "full_output")[[1]]
    out <- data.frame(as.list(object$ci), check.names = FALSE)
    rownames(out) <- paste0(full_output_attr$y,
                         "~",
                         paste0(rev(full_output_attr$m), collapse = "~"),
                         "~",
                         full_output_attr$x)
    out
  }
