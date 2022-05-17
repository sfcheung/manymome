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
#' @param x Character. The name of predictor at the start of the pathway.
#' @param y Character. The name of the outcome variable at
#'          the end of the pathway.
#' @param m A vector of the variable names of the
#'          moderators. The pathway goes from the first
#'          mediator successively to the last mediator. If
#'          `NULL`, the default, the pathway goes from `x`
#'          to `y`.
#' @param xvalue Scalar. The value of `y` predicted by this
#'                value of `x` will be computed.
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#' @export
#'
#'

predict_i <- function(x,
                      y,
                      m = NULL,
                      xvalue,
                      fit) {
    if (is.null(m)) {
        fit0 <- refit_1_i(x = x, xvalue = xvalue, fit = fit)
        return(get_intercept(y, fit0))
      }
    fit0 <- refit_x_i(x = x, ys = m, xvalue = xvalue, fit = fit)
    out <- get_intercept(y, fit0)
    out
  }
