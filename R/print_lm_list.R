#' @title Print a `lm_list`-Class Object
#'
#' @description Print the content of the output of [lm2list()].
#'
#' @return
#'  `x` is returned invisibly.
#'
#' @param x The output of the output of [lm2list()].
#' @param ...  Other arguments. Not used.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#'
#' @export

print.lm_list <- function(x, ...) {
    cat("\nThe models:\n")
    lapply(x, function(y) print(y$call$formula))
    cat("\n")
    invisible(x)
  }
