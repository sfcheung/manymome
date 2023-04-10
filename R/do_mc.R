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
#' @param progress Not used. Kept
#' for compatibility with [do_boot()].
#'
#' @seealso [fit2mc_out()], which
#' implements the Monte Carlo simulation.
#'
#' @examples
#'
#' # TO PROCESS
#'
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' # In real research, R should be 2000 or even 5000
#' # In real research, no need to set parallel and progress to FALSE
#' # Parallel processing is enabled by default and
#' # progress is displayed by default.
#' lm_boot_out <- do_boot(lm_out, R = 50, seed = 1234,
#'                        parallel = FALSE,
#'                        progress = FALSE)
#' wlevels <- mod_levels(w = "w", fit = lm_out)
#' wlevels
#' out <- cond_indirect_effects(wlevels = wlevels,
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = lm_out,
#'                              boot_ci = TRUE,
#'                              boot_out = lm_boot_out)
#' out
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
    if (fit_type == "lavaan") {
        fit0 <- gen_mc_est(fit = fit,
                          seed = seed,
                          R = R)
        out <- fit2mc_out(fit0)
      }
    if (fit_type == "lm") {
        stop("Monte Carlo method does not support lm outputs.")
      }
    return(out)
  }

#' @noRd

gen_mc_est <- function(fit,
                       R = 100,
                       seed = NULL) {
    fit_vcov <- tryCatch(lavaan::lavInspect(fit, "vcov"),
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