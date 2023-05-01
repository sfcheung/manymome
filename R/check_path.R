#' @title Check a Path Exists in a Model
#'
#' @description It checks whether a
#' path, usually an indirect path,
#' exists in a model.
#'
#' @details It checks whether the path
#' defined by a predictor (`x`), an
#' outcome (`y`), and optionally a
#' sequence of mediators (`m`), exists
#' in a model. It can check models in a
#' [lavaan::lavaan-class] object or a
#' list of outputs of [lm()].
#'
#' For example, in the ql
#' in [lavaan] syntax
#'
#' ```
#' m1 ~ x
#' m2 ~ m1
#' m3 ~ x
#' y ~ m2 + m3
#' ```
#'
#' This path is valid: `x = "x", y = "y", m = c("m1", "m2")`
#'
#' This path is invalid: `x = "x", y = "y", m = c("m2")`
#'
#' This path is also invalid: `x = "x", y = "y", m = c("m1", "m2")`
#'
#' @return A logical vector of length
#' one. `TRUE` if the path is valid,
#' `FALSE` if the path is invalid.
#'
#' @param fit The fit object. Currently
#' only supports a
#' [lavaan::lavaan-class] object or a
#' list of outputs of [lm()].
#'
#' @param x Character. The name of
#' predictor at the start of the path.
#'
#' @param y Character. The name of the
#' outcome variable at the end of the
#' path.
#'
#' @param m A vector of the variable
#' names of the mediators. The path goes
#' from the first mediator successively
#' to the last mediator. If `NULL`, the
#' default, the path goes from `x` to
#' `y`.
#'
#' @param est The output of
#' [lavaan::parameterEstimates()]. If
#' `NULL`, the default, it will be
#' generated from `fit`. If supplied,
#' `fit` will ge ignored.
#'
#'
#' @examples
#'
#' library(lavaan)
#' data(data_serial_parallel)
#' dat <- data_serial_parallel
#' mod <-
#' "
#' m11 ~ x + c1 + c2
#' m12 ~ m11 + x + c1 + c2
#' m2 ~ x + c1 + c2
#' y ~ m12 + m2 + m11 + x + c1 + c2
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE)
#'
#' # The following paths are valid
#' check_path(x = "x", y = "y", m = c("m11", "m12"), fit = fit)
#' check_path(x = "x", y = "y", m = "m2", fit = fit)
#' # The following paths are invalid
#' check_path(x = "x", y = "y", m = c("m11", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m12", "m11"), fit = fit)
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
              lavaan = lav_est(fit, se = FALSE, ci = FALSE),
              lavaan.mi = lav_est(fit, se = FALSE, ci = FALSE))
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
