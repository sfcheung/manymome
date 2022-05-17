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

refit_1_i <- function(x,
                      xvalue,
                      fit,
                      wvalues = NULL) {
    dat0 <- lav_data_used(fit, drop_colon = TRUE)
    if (!is.null(wvalues)) {
        dat0[, names(wvalues)] <- sweep(dat0[, names(wvalues), drop = FALSE],
                                       MARGIN = 2,
                                       STATS = wvalues)
        # fit1 <- lavaan::update(fit,
        #                        data = dat0,
        #                        se = "none",
        #                        baseline = FALSE,
        #                        h1 = FALSE)
      } else {
        # fit1 <- fit
      }
    dat0[, x] <- dat0[, x] - xvalue
    fit2 <- lavaan::update(fit,
                           data = dat0,
                           se = "none",
                           baseline = FALSE,
                           h1 = FALSE)
    fit2
  }
