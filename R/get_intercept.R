#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param x Character. The variable for which the intercept will be extracted.
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`, the
#'            default, it will be generated from `fit`. If supplied,
#'            `fit` will ge ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    i <- (est$lhs == x) &
         (est$op == "~1")
    if (isTRUE(any(i))) {
        return(est[i, "est"])
      } else {
        return(NA)
      }
  }
