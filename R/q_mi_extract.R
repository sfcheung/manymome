#' Extract Multiple Imputation Results from Quick Mediation
#'
#' @description Retrieve the retained mice object or MI computation and
#' diagnostic information from a quick mediation analysis.
#' @param x A result from a `q_()` mediation function with `missing = "mi"`.
#' @details `get_imputation()` returns the retained `mids` object. It returns
#' `NULL` if only a list of completed data frames was supplied. It does not
#' create new imputations. Inspect this object using mice diagnostic tools.
#'
#' `get_mi_info()` returns source, requested and usable imputation counts,
#' omissions and reasons, interval method, draw count, confidence level,
#' effect/covariance pooling conventions, execution information and
#' imputation diagnostics. Current MC covariance extraction uses lavaan.mi
#' with `scale.W = TRUE` (scaled within-imputation covariance).
#' Diagnostics include logged events, methods, predictor matrix, iteration
#' count and whether auxiliary variables predict imputed model variables.
#' Logged events need interpretation; they do not automatically indicate
#' failed imputation or adequate convergence. Predictor usage reflects the
#' retained predictor matrix and active blocks, not the scientific validity
#' of the imputation model. Formula-based prediction can differ from this
#' matrix; usage indicators are `NA` when active formula-based blocks exist.
#' Consult the retained mice object if formulas were customized.
#' Diagnostics have `available = FALSE` for completed-data lists.
#'
#' MI computation is sequential. `parallel` and `ncores` in quick mediation
#' calls do not control imputation, fitting or Monte Carlo generation.
#' @return `get_imputation()`: a `mids` object or `NULL`.
#' `get_mi_info()`: a named list of computation and diagnostic information.
#' @examples
#' if (requireNamespace("lavaan.mi", quietly = TRUE) &&
#'     requireNamespace("mice", quietly = TRUE)) {
#'   set.seed(42)
#'   x <- rnorm(80)
#'   m <- .5*x + rnorm(80)
#'   y <- .2*x + .4*m + rnorm(80)
#'   d <- data.frame(x, m, y)
#'   d$m[1:10] <- NA
#'   out <- q_simple_mediation(x="x", m="m", y="y", data=d,
#'            missing="mi", mice_args=list(m=2, maxit=2),
#'            mi_seed=123, boot_ci=FALSE, mc_ci=FALSE, progress=FALSE)
#'   get_mi_info(out)$diagnostics$logged_events
#'   get_imputation(out)$method
#' }
#' @export
get_imputation <- function(x) {
  q_mi_check_result(x)
  x$imputation
}

#' @rdname get_imputation
#' @export
get_mi_info <- function(x) {
  q_mi_check_result(x)
  x$mi_info
}

#' @noRd
q_mi_check_result <- function(x) {
  if (!inherits(x, "q_mediation") || !identical(x$missing, "mi")) {
    stop("x must be an MI result from a q_() mediation function.")
  }
  invisible(x)
}
