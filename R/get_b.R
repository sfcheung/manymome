#' @title Get the Path Coefficient from 'x' to 'y'
#'
#' @param x Character. The predictor.
#' @param y Character. The outcome.
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`, the
#'            default, it will be generated from `fit`. If supplied,
#'            `fit` will ge ignored.
#'
#'
#' @examples
#' \donttest{
#' }
#'
#' @noRd

get_b <- function(x,
                  y,
                  fit,
                  est = NULL) {
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    i <- (est$lhs == y) &
         (est$op == "~") &
         (est$rhs == x)
    if (isTRUE(any(i))) {
        return(est[i, "est"])
      } else {
        return(NA)
      }
  }
