#' @title Monte Carlo Estimates for
#' 'indirect_effects' and
#' 'cond_indirect_effects'
#'
#' @description Generate Monte Carlo
#' estimates to be used by
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()],
#'
#' @details It uses the parameter
#' estimates and their variance-covariance
#' matrix to generate Monte Carlo
#' estimates of the parameter estimates
#' in a model fitted by
#' [lavaan::sem()]. The stored estimates
#' can then be used by
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()] to form
#' Monte Carlo confidence intervals.
#'
#' This approach removes the need to
#' repeat Monte Carlo simulation in
#' each call to
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()]. It also ensures
#' that the same set of Monte Carlo
#' estimates is used in all subsequent
#' analysis.
#'
#'
#' @return A `mc_out`-class object
#' that can be used for the `mc_out`
#' argument of
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()] for forming
#' Monte Carlo confidence intervals. The
#' object is a list with the number of
#' elements equal to the number of
#' Monte Carlo replications. Each element
#' is a
#' list of the parameter estimates and
#' sample variances and covariances of
#' the variables in each Monte Carlo
#' replication.
#'
#' @param fit The output of
#' [lavaan::sem()]. The output of
#' [stats::lm()] is not supported.
#'
#' @param R The number of replications.
#' Default is 100.
#'
#' @param seed The seed for the
#' generating Monte Carlo estimates.
#' Default is `NULL` and seed is not set.
#'
#' @param parallel Not used. Kept
#' for compatibility with [do_boot()].
#'
#' @param ncores Not used. Kept
#' for compatibility with [do_boot()].
#'
#' @param make_cluster_args Not used. Kept
#' for compatibility with [do_boot()].
#'
#' @param progress Logical. Display
#' progress or not. Default is `TRUE`.
#'
#' @seealso [fit2mc_out()], which
#' implements the Monte Carlo simulation.
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
#' wlevels <- mod_levels(w = "w", fit = fit)
#' wlevels
#' out <- cond_indirect_effects(wlevels = wlevels,
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = fit,
#'                              mc_ci = TRUE,
#'                              mc_out = mc_out)
#' out
#'
#' @describeIn do_mc A general purpose function for
#' creating Monte Carlo estimates to be reused
#' by other functions. It returns a
#' `mc_out`-class object.
#'
#' @order 1
#'
#' @export
#'
#'

do_mc <- function(fit,
                    R = 100,
                    seed = NULL,
                    parallel = TRUE,
                    ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                    make_cluster_args = list(),
                    progress = TRUE) {
    fit_type <- cond_indirect_check_fit(fit)
    if (fit_type == "lavaan" || fit_type == "lavaan.mi") {
        fit0 <- gen_mc_est(fit = fit,
                          seed = seed,
                          R = R)
        out <- fit2mc_out(fit0, progress = progress)
      }
    if (fit_type == "lm") {
        stop("Monte Carlo method does not support lm outputs.")
      }
    return(out)
  }

#' @describeIn do_mc Generate Monte Carlo
#' estimates and store them in the `external`
#' slot: `external$manymome$mc`. For advanced
#' users.
#'
#' @order 2
#'
#' @export

gen_mc_est <- function(fit,
                       R = 100,
                       seed = NULL) {
    # fit_vcov <- tryCatch(lavaan::lavInspect(fit, "vcov"),
    #                         error = function(e) e)
    fit_vcov <- tryCatch(get_vcov(fit),
                         error = function(e) e)
      if (inherits(fit_vcov, "error")) {
          stop("Monte Carlo method cannot be used. VCOV of estimates not available.")
        }
    if (!is.null(seed)) set.seed(seed)
    est <- lavaan::coef(fit)
    mc_est <- MASS::mvrnorm(n = R,
                            mu = est,
                            Sigma = fit_vcov)
    attr(mc_est, "R") <- R
    attr(mc_est, "est") <- est
    attr(mc_est, "vcov") <- fit_vcov
    fit@external$manymome$mc <- mc_est
    fit
  }