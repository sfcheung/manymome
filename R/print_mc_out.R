#' @title Print a `mc_out`-Class
#' Object
#'
#' @description Print the content of the
#' output of [do_mc()] or related
#' functions.
#'
#' @return `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x The output of [do_mc()],
#' or any `mc_out`-class object
#' returned
#' by similar functions.
#'
#' @param ...  Other arguments. Not
#' used.
#'
#'
#'
#' @examples
#'
#' library(lavaan)
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' mod <-
#' "
#' m ~ x + w + x:w + c1 + c2
#' y ~ m + w + m:w + x + c1 + c2
#' "
#' fit <- sem(mod, dat)
#' # In real research, R should be 5000 or even 10000
#' mc_out <- do_mc(fit, R = 100, seed = 1234)
#'
#' # Print the output of do_boot()
#' mc_out
#'
#' @export

print.mc_out <- function(x, ...) {
    nmc <- length(x)
    cat("\n")
    cat("== A 'mc_out' class object ==\n")
    cat("\n")
    cat("Number of Monte Carlo replications:", nmc, "\n")
    varnames <- colnames(x[[1]]$implied_stats$cov)
    if (!is.null(varnames)) {
        cat("Column names in data:\n")
        print(varnames, quote = FALSE)
        cat("(Note: May contain derived terms such as product terms.)\n")
      }
    cat("\n")
    invisible(x)
  }
