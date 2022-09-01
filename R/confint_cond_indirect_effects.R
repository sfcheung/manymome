#' @title Confidence Intervals of
#' Indirect Effects or Conditional
#' Indirect Effects
#'
#' @description Return the confidence
#' intervals of the conditional indirect
#' effects or conditional effects in the
#' output of [cond_indirect_effects()].
#'
#' @details It extracts and returns the
#' columns for confidence intervals, if
#' available.
#'
#' @param object The output of
#' [cond_indirect_effects()].
#'
#' @param parm Ignored. Always returns
#' the bootstrap confidence interval of
#' the effects for all levels stored.
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval. Ignored for now
#' and will use the level of the stored
#' intervals.
#'
#' @param ...  Additional arguments.
#' Ignored by the function.
#'
#' @return A data frame with two
#' columns, one for each confidence
#' limit of the confidence intervals.
#' The number of rows is equal to the
#' number of rows of `object`.
#'
#' @seealso [cond_indirect_effects()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ x  + w1 + x:w1
#' m2 ~ m1
#' y  ~ m2 + x + w4 + m2:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for cond_indirect():
#'
#' # Create levels of w1 and w4
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#' w4levels <- mod_levels("w4", fit = fit)
#' w4levels
#' w1w4levels <- merge_mod_levels(w1levels, w4levels)
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' # R should be at least 2000 or 5000 in real research.
#' out1 <- suppressWarnings(cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = w1levels, fit = fit,
#'                       boot_ci = TRUE, R = 20, seed = 54151,
#'                       parallel = FALSE))
#' confint(out1)
#'
#'
#' @export

confint.cond_indirect_effects <- function(object, parm, level = .95, ...) {
    # TODO: Get CIs for other levels
    # if (isTRUE(!is.null(object$boot_ci))) {
    #     boot_out <- list(t0 = object$indirect,
    #                      t = matrix(object$boot_indirect, ncol = 1),
    #                      R = length(object$boot_indirect))
    #     out0 <- boot::boot.ci(boot_out,
    #                         type = "perc",
    #                         conf = level)$percent[4:5]
    #   } else {
    #     warning("Bootstrapping interval not in the object.")
    #     out0 <- c(NA, NA)
    #   }
    out0 <- as.data.frame(object)
    full_output <- attr(object, "full_output")
    if (is.null(full_output[[1]]$boot_ci)) {
          warning("Bootstrapping intervals not in the object.")
          out0 <- data.frame(x1 = rep(NA, nrow(object)),
                             x2 = rep(NA, nrow(object)))
      } else {
        out <- out0[, c("CI.lo", "CI.hi")]
      }
    # Borrowed from stats::confint()
    probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
    cnames <- paste(format(100 * probs,
                           trim = TRUE,
                           scientific = FALSE,
                           digits = 2), "%")
    wlevels <- attr(object, "wlevels")
    colnames(out) <- cnames
    rownames(out) <- rownames(wlevels)
    out
  }
