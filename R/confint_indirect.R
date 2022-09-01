#' @title Confidence Interval of
#' Indirect Effect or Conditional
#' Indirect Effect
#'
#' @description Return the  bootstrap
#' confidence interval of the indirect
#' effect or conditional indirect effect
#' stored in the output of
#' [indirect_effect()] or
#' [cond_indirect()].
#'
#' @details It extracts and returns the
#' stored bootstrap confidence interval
#' if available.
#'
#' @param object The output of
#' [indirect_effect()] or
#' [cond_indirect()].
#'
#' @param parm Ignored because the
#' stored object always has only one
#' parameter.
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval.
#'
#' @param ...  Additional arguments.
#' Ignored by the function.
#'
#' @return A numeric vector of
#' two elements, the limits of
#' the confidence interval.
#'
#' @seealso [indirect_effect()] and
#' [cond_indirect()]
#'
#' @examples
#'
#' dat <- modmed_x1m3w4y1
#'
#' # Indirect Effect
#'
#' library(lavaan)
#' mod1 <-
#' "
#' m1 ~ x
#' m2 ~ m1
#' y  ~ m2 + x
#' "
#' fit <- sem(mod1, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' # R should be at least 2000 or 5000 in real research.
#' out1 <- indirect_effect(x = "x", y = "y",
#'                         m = c("m1", "m2"),
#'                         fit = fit,
#'                         boot_ci = TRUE, R = 45, seed = 54151,
#'                         parallel = FALSE)
#' out1
#' confint(out1)
#'
#' # Conditional indirect effect
#'
#' # Create levels of w1 and w4
#' w1_hi <- mean(dat$w1) + sd(dat$w1)
#' w4_lo <- mean(dat$w4) - sd(dat$w4)
#'
#' mod2 <-
#' "
#' m1 ~ x + w1 + x:w1
#' m2 ~ m1
#' y  ~ m2 + x + w4 + m2:w4
#' "
#' fit2 <- sem(mod2, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' # R should be at least 2000 or 5000 in real research.
#' out2 <- cond_indirect(x = "x", y = "y",
#'                       m = c("m1", "m2"),
#'                       wvalues = c(w1 = w1_hi, w4 = w4_lo),
#'                       fit = fit,
#'                       boot_ci = TRUE, R = 45, seed = 54151,
#'                       parallel = FALSE)
#' out2
#' confint(out2)
#'
#' @export


confint.indirect <- function(object, parm, level = .95, ...) {
    if (isTRUE(!is.null(object$boot_ci))) {
        boot_out <- list(t0 = object$indirect,
                         t = matrix(object$boot_indirect, ncol = 1),
                         R = length(object$boot_indirect))
        out0 <- boot::boot.ci(boot_out,
                            type = "perc",
                            conf = level)$percent[4:5]
      } else {
        warning("Bootstrapping interval not in the object.")
        out0 <- c(NA, NA)
      }
    # Borrowed from stats::confint()
    probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
    cnames <- paste(format(100 * probs,
                           trim = TRUE,
                           scientific = FALSE,
                           digits = 2), "%")
    rnames <- paste0(object$y, "~", object$x)
    out <- array(data = out0,
                 dim = c(1, 2),
                 dimnames = list(rnames, cnames))
    out
  }
