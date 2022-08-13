#' @title Confidence Interval of Indirect Effect or Conditional Indirect Effect
#'
#' @description Return the  bootstrap confidence
#'              interval of the indirect effect
#'              or conditional indirect effect
#'              stored in the output of [indirect_effect()] or
#'              [cond_indirect()].
#'
#' @details It extracts and returns the stored bootstrap
#'           confidence interval.
#'
#' @param object The output of [indirect_effect()] or
#'              [cond_indirect()].
#' @param parm Ignored because the stored object
#'             always has only one parameter.
#' @param level The level of confidence, default is .95, returning the
#'               95% confidence interval.
#' @param ...  Additional arguments. Ignored by the function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [indirect_effect()] and [cond_indirect()]
#'
#' @examples
#'
#' # TODO: Update the examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
#' m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
#' m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
#' y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
#'
#' indirect_1 <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
#'                        wvalues = wvalues)
#' indirect_2 <- (est[est$label == "a1", "est"] +
#'                 wvalues["w1"] * est[est$label == "d1", "est"]) *
#'               (est[est$label == "a2", "est"] +
#'                 wvalues["w2"] * est[est$label == "d2", "est"]) *
#'               (est[est$label == "a3", "est"] +
#'                 wvalues["w3"] * est[est$label == "d3", "est"]) *
#'               (est[est$label == "a4", "est"] +
#'                 wvalues["w4"] * est[est$label == "d4", "est"])
#' indirect_1$indirect
#' indirect_2
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
