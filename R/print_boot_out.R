#' @title Print a `boot_out`-Class
#' Object
#'
#' @description Print the content of the
#' output of [do_boot()] or related
#' functions.
#'
#' @return `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x The output of [do_boot()],
#' or any `boot_out`-class object
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
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' # In real research, R should be 2000 or even 5000
#' # In real research, no need to set parallel to FALSE
#' # In real research, no need to set progress to FALSE
#' # Progress is displayed by default.
#' lm_boot_out <- do_boot(lm_out, R = 100,
#'                        seed = 1234,
#'                        progress = FALSE,
#'                        parallel = FALSE)
#' # Print the output of do_boot()
#' lm_boot_out
#'
#' library(lavaan)
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' dat$"x:w" <- dat$x * dat$w
#' dat$"m:w" <- dat$m * dat$w
#' mod <-
#' "
#' m ~ x + w + x:w + c1 + c2
#' y ~ m + w + m:w + x + c1 + c2
#' "
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' # In real research, R should be 2000 or even 5000
#' # In real research, no need to set progress to FALSE
#' # In real research, no need to set parallel to FALSE
#' # Progress is displayed by default.
#' fit_boot_out <- do_boot(fit = fit,
#'                         R = 40,
#'                         seed = 1234,
#'                         parallel = FALSE,
#'                         progress = FALSE)
#' # Print the output of do_boot()
#' fit_boot_out
#'
#' @export

print.boot_out <- function(x, ...) {
    nboot <- length(x)
    cat("\n")
    cat("== A 'boot_out' class object ==\n")
    cat("\n")
    cat("Number of bootstrap samples:", nboot, "\n")
    if (!is.null(x[[1]]$data)) {
        n <- nrow(x[[1]]$data)
        cat("Number of cases in each bootstrap sample:", n, "\n")
        cat("(Missing data, if any, is ignored in the count.)\n")
      }
    # varnames <- colnames(x[[1]]$data)
    varnames <- colnames(x[[1]]$implied_stats$cov)
    if (!is.null(varnames)) {
        cat("Column names in data:\n")
        print(varnames, quote = FALSE)
        cat("(Note: May contain derived terms such as product terms.)\n")
      }
    cat("\n")
    invisible(x)
  }
