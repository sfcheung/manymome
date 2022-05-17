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
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param x Character. The name of predictor at the start of the pathway.
#' @param y Character. The name of the outcome variable at
#'          the end of the pathway.
#' @param m A vector of the variable names of the
#'          moderators. The pathway goes from the first
#'          mediator successively to the last mediator. If
#'          `NULL`, the default, the pathway goes from `x`
#'          to `y`.
#' @param wvalues A numeric vector of named elements. The names are the variable
#'                names of the moderators, and the values are the values to
#'                which the moderators will be set to. Default is `NULL`.
#' @param data A data frame or on object that can be coerced to a data frame.
#'             If supplied, it will be used in updating the `fit` object.
#'             If `NULL`, the default, the data stored in `fit` will be used.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#' @export
#'
#'

cond_effect_i <- function(fit,
                          x,
                          y,
                          m = NULL,
                          wvalues = NULL,
                          data = NULL) {
    if (is.null(data)) {
        dat0 <- lav_data_used(fit, drop_colon = TRUE)
      } else {
        dat0 <- data
      }
    if (!is.null(wvalues)) {
        dat0[, names(wvalues)] <- sweep(dat0[, names(wvalues), drop = FALSE],
                                       MARGIN = 2,
                                       STATS = wvalues)
        fit0 <- lavaan::update(fit,
                               data = dat0,
                               se = "none",
                               baseline = FALSE,
                               h1 = FALSE)
      } else {
        fit0 <- fit
      }
    indirect <- indirect(x = x, y = y, m = m, fit = fit0)
    out <- list(indirect = indirect[1],
                bs = indirect[-1],
                fit = fit0)
    out
  }
