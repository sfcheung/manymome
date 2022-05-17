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
#' @param ys A vector of the variable names of the
#'          mediators. The pathway goes from the first
#'          mediator successively to the last mediator.
#' @param xvalue Scalar. The value of `y` predicted by this
#'                value of `x` will be computed.
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param wvalues A numeric vector of named elements. The names are the variable
#'                names of the moderators, and the values are the values to
#'                which the moderators will be set to. Default is `NULL`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#' @export
#'
#'

refit_x_i <- function(x,
                      ys,
                      xvalue,
                      fit,
                      wvalues = NULL) {
    fit0 <- refit_1_i(x, xvalue, fit, wvalues)
    for (xi in ys) {
        fit0 <- refit_1_i(xi, get_intercept(xi, fit0), fit, wvalues)
      }
    fit0
  }
