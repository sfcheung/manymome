#' @title Get the intercept in 'lavaan'
#' fit object
#'
#' @param x Character. The variable for
#' which the intercept will be
#' extracted.
#'
#' @param fit The fit object. Currently
#' only supports a
#' [lavaan::lavaan-class] object.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [semTools::runMI()] or
#' its wrapper, such as [semTools::sem.mi()].
#'
#' @param est The output of
#' [lavaan::parameterEstimates()]. If
#' `NULL`, the default, it will be
#' generated from `fit`. If supplied,
#' `fit` will ge ignored.
#'
#'
#' @examples
#' \donttest{
#' }
#' @noRd
#'
#'

get_intercept <- function(x,
                          fit,
                          est = NULL) {
    if (!missing(fit)) {
         fit <- auto_lm2list(fit)
      }
    if (is.null(est)) {
      est <- lav_est(fit, se = FALSE, ci = FALSE)
    }
    i <- (est$lhs == x) &
         (est$op == "~1")
    if (isTRUE(any(i))) {
        return(est[i, "est"])
      } else {
        return(NA)
      }
  }
