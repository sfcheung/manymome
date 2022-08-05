#' @title Estimates of Conditional Indirect Effects or Conditional Effects
#'
#' @description Return the estimates of the conditional indirect
#'   effects or conditional effects for all levels in the output of
#'   [cond_indirect_effects()].
#'
#' @details It just extracts and returns the column `ind` or `std`
#'  in the output of [cond_indirect_effects()].
#'
#' @return
#'  A numeric vector: The estimates of the conditional effects
#'  or conditional indirect effects.
#'
#' @param object The output of [cond_indirect_effects()].
#' @param ...  Optional arguments. Ignored by the function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ x  + w1 + x:w1
#' m2 ~ m1
#' y  ~ m2 + x + w4 + m2:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for cond_indirect():
#'
#' # Create levels of w1 and w4
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#' w4levels <- mod_levels("w4", fit = fit)
#' w4levels
#' w1w4levels <- merge_mod_levels(w1levels, w4levels)
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' out1 <- cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = w1levels, fit = fit)
#' coef(out1)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the levels
#' out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
#'                       wlevels = w1w4levels, fit = fit)
#' coef(out2)
#'
#' @export

coef.cond_indirect_effects <- function(object, ...) {
    wlevels <- attr(object, "wlevels")
    if ("std" %in% colnames(object)) {
        out <- object$std
      } else {
        out <- object$ind
      }
    if (!is.null(wlevels)) {
        names(out) <- rownames(wlevels)
      }
    out
  }