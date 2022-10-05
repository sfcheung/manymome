#' @title Confidence Intervals of
#' Indirect Effects in an 'indirect_list'
#' Object
#'
#' @description Return the  bootstrap
#' confidence intervals of the indirect
#' effects
#' stored in the output of
#' [many_indirect_effects()].
#'
#' @details It extracts and returns the
#' stored bootstrap confidence interval
#' if available.
#'
#' @param object The output of
#' [many_indirect_effects()].
#'
#' @param parm Ignored for now.
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval.
#'
#' @param ...  Additional arguments.
#' Ignored by the function.
#'
#' @return A two-column data frame.
#' The columns are the limits of
#' the confidence intervals.
#'
#' @seealso [many_indirect_effects()]
#'
#' @examples
#'
#' dat <- modmed_x1m3w4y1
#' # TODO
#'
#'
#' @export

confint.indirect_list <- function(object, parm = NULL, level = .95, ...) {
    p <- length(object)
    if (isTRUE(!is.null(object[[1]]$boot_ci))) {
        confint0 <- lapply(object, stats::confint,
                           parm = parm,
                           level = level, ...)
        confint0 <- do.call(rbind, confint0)
        rownames(confint0) <- names(object)
      } else {
        warning("Bootstrapping interval not in the object.")
        confint0 <- array(data = rep(NA, p * 2),
                          dim = c(p, 2),
                          dimnames = list(names(object), c("CI.lo", "CI.hi")))
      }
    confint0
  }
