#' @title Get the Path Coefficient from
#' 'x' to 'y'
#'
#' @param x Character. The predictor.
#'
#' @param y Character. The outcome.
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
#'
#' @noRd

get_b <- function(x,
                  y,
                  fit,
                  est = NULL) {
    if (is.null(est)) {
      est <- lav_est(fit, se = FALSE, ci = FALSE)
    }
    i <- (est$lhs == y) &
         (est$op == "~") &
         (est$rhs == x)
    if (isTRUE(any(i))) {
        out <- est[i, "est"]
        if (length(out) > 1) {
            # Multigroup model
            if (!is.null(est$group)) {
                names(out) <- est$group[i]
              } else {
                names(out) <- seq_along(out)
              }
          }
        return(out)
      } else {
        return(NA)
      }
  }
