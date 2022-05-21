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
#'          mediators. The pathway goes from the first
#'          mediator successively to the last mediator. If
#'          `NULL`, the default, the pathway goes from `x`
#'          to `y`.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`, the
#'            default, it will be generated from `fit`. If supplied,
#'            `fit` will ge ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + c4 * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' check_path(x = "x", y = "m3", m = c("m1", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m1", "m2"), fit = fit)
#'
#' @export
#'
#'

check_path <- function(x,
                       y,
                       m = NULL,
                       fit = NULL,
                       est = NULL) {
    if (is.null(est)) {
      fit_type <- cond_indirect_check_fit(fit)
      est <- switch(fit_type,
              lm = lm2ptable(fit)$est,
              lavaan = lavaan::parameterEstimates(fit))
    }
    if (is.null(m)) {
        return(any(((est$lhs == y) & (est$op == "~") & (est$rhs == x))))
      }
    y0 <- y
    for (xi in c(rev(m), x)) {
        if (any(((est$lhs == y0) & (est$op == "~") & (est$rhs == xi)))) {
            y0 <- xi
            next
          } else {
            return(FALSE)
          }
      }
    return(TRUE)
  }