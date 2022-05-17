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
#' @param xvalues A vector of two numbers. The values of `y`
#'                predicted by these two values of `x` will
#'                be computed. Default is `c(-1, 1)`.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#' @export
#'
#'

cond_effect <- function(fit,
                        x,
                        y,
                        m = NULL,
                        wvalues = NULL,
                        xvalues = c(-1, 1)) {
    dat0 <- lav_data_used(fit, drop_colon = TRUE)
    if (!is.null(wvalues)) {
        dat0[, names(wvalues)] <- sweep(dat0[, names(wvalues), drop = FALSE],
                                       MARGIN = 2,
                                       STATS = wvalues)
        # fit0 <- lavaan::update(fit,
        #                        data = dat0,
        #                        se = "none",
        #                        baseline = FALSE,
        #                        h1 = FALSE)
      } else {
        # fit0 <- fit
      }
    fit1 <- refit_x_i(x = x, ys = m, xvalue = xvalues[1],
                      fit = fit, wvalues = wvalues)
    fit2 <- refit_x_i(x = x, ys = m, xvalue = xvalues[2],
                      fit = fit, wvalues = wvalues)
    eff <- (get_intercept(y, fit2) - get_intercept(y, fit1)) /
              (xvalues[2] - xvalues[1])
    out <- list(effect = eff,
                fit1 = fit1,
                fit2 = fit2)
  }
