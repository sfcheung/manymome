#' @title Print the a 'indirect' Class Object
#'
#' @description Print the content of the output of [indirect()]
#'              or [cond_indirect()].
#'
#' @return
#'  `x` is returned invisibly.
#'
#' @param object The output of the output of [lm2list()].
#' @param x An object of class "summary_lm_list".
#' @param digits The number of significant digits in printing numerical results.
#' @param ...  Other arguments. Not used.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
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
#' indirect_1 <- indirect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
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

summary.lm_list <- function(object, ...) {
    out <- lapply(object, summary)
    class(out) <- c("summary_lm_list", class(out))
    out
  }

#' @describeIn summary.lm_list Print method for output of summary for lm_list.
#' @export

print.summary_lm_list <- function(x, digits = 3, ...) {
    for (xi in x) {
        cat("\n\nModel:\n")
        print(xi$call$formula)
        stats::printCoefmat(xi$coefficients, digits = digits, ...)
        rsq0 <- formatC(xi$r.squared, digits = digits, format = "f")
        adjrsq0 <- formatC(xi$adj.r.squared, digits = digits, format = "f")
        f0 <- paste0("F(", round(xi$fstatistic["numdf"]),
                     ", ", xi$fstatistic["dendf"],
                     ") = ",
                     formatC(xi$fstatistic["value"], digits = digits, format = "f"))
        p1 <- stats::pf(xi$fstatistic["value"],
                        xi$fstatistic["numdf"],
                        xi$fstatistic["dendf"], lower.tail = FALSE)
        p0 <- ifelse(p1 < .001,
                     "p < .001",
                     paste0("p = ", formatC(p1, digits = digits, format = "f")))
        fstr <- paste0(f0, ", ", p0)
        tmp <- paste0("R-square = ", rsq0,
                      ". Adjusted R-square = ", adjrsq0,
                      ". ", fstr)
        cat(tmp)
      }
    cat("\n")
    invisible(x)
  }