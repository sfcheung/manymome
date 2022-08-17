#' @title Estimates of Conditional Indirect Effects or Conditional Effects
#'
#' @description Return the estimates of the conditional indirect
#'   effects or conditional effects for all levels in the output of
#'   [cond_indirect_effects()].
#'
#' @details It extracts and returns the column `ind` or `std`
#'  in the output of [cond_indirect_effects()].
#'
#' @return
#'  A numeric vector: The estimates of the conditional effects
#'  or conditional indirect effects.
#'
#' @param object The output of [cond_indirect_effects()].
#' @param ...  Optional arguments. Ignored by the function.
#'
#'
#' @seealso [cond_indirect_effects()]
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
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' out1 <- cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = c("w1"), fit = fit)
#' out1
#' coef(out1)
#'
#' # Conditional indirect effects from x1 through m1 and m2 to y,
#' out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
#'                       wlevels = c("w1", "w4"), fit = fit)
#' out2
#' coef(out2)
#'
#' # Standardized conditional indirect effects from x1 through m1 and m2 to y,
#' out2std <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
#'                       wlevels = c("w1", "w4"), fit = fit,
#'                       standardized_x = TRUE, standardized_y = TRUE)
#' out2std
#' coef(out2std)
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
