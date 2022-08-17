#' @title Summary of an `lm_list`-Class Object
#'
#' @description The summary of content of the output of [lm2list()].
#'
#' @return
#'  `x` is returned invisibly.
#'
#' @param object The output of [lm2list()].
#' @param x An object of class `summary_lm_list`.
#' @param digits The number of significant digits in printing
#' numerical results.
#' @param ...  Other arguments. Not used.
#'
#'
#'
#' @examples
#'
#' data(data_serial_parallel)
#' lm_m11 <- lm(m11 ~ x + c1 + c2, data_serial_parallel)
#' lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, data_serial_parallel)
#' lm_m2 <- lm(m2 ~ x + c1 + c2, data_serial_parallel)
#' lm_y <- lm(y ~ m11 + m12 + m2 + x + c1 + c2, data_serial_parallel)
#' # Join them to form a lm_list-class object
#' lm_serial_parallel <- lm2list(lm_m11, lm_m12, lm_m2, lm_y)
#' lm_serial_parallel
#' summary(lm_serial_parallel)
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