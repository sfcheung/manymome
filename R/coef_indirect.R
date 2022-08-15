#' @title Extract the Indirect Effect or Conditional Indirect Effect
#'
#' @description Return the estimate of the indirect effect
#'              in the output of [indirect_effect()] or
#'              or the conditional indirect
#'              in the output of [cond_indirect()].
#'
#' @details It extracts and returns the element `indirect`.
#'           in an object.
#'
#' If standardized effect is requested when calling
#' [indirect_effect()] or [cond_indirect()], the
#' effect returned is also standardized.
#'
#' @return
#'  A scalar: The estimate of the indirect effect or
#'            conditional indirect effect.
#'
#' @param object The output of [indirect_effect()] or
#'              [cond_indirect()].
#' @param ...  Optional arguments. Ignored by the function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [indirect_effect()] and [cond_indirect()].
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ x + w1 + x:w1
#' m2 ~ x
#' y  ~ m1 + m2 + x
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for indirect_effect():
#'
#' # Inidrect effect from x through m2 to y
#' out1 <- indirect_effect(x = "x", y = "y", m = "m2", fit = fit)
#' out1
#' coef(out1)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is 1 SD above mean
#' hi_w1 <- mean(dat$w1) + sd(dat$w1)
#' out2 <- cond_indirect(x = "x", y = "y", m = "m1",
#'                       wvalues = c(w1 = hi_w1), fit = fit)
#' out2
#' coef(out2)
#'
#' @export

coef.indirect <- function(object, ...) {
    out <- object$indirect
    names(out) <- paste0(object$y, "~", object$x)
    out
  }
