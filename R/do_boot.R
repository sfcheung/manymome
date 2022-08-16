#' @title Bootstrap Estimates for 'indirect_effects' and 'cond_indirect_effects'
#'
#' @description Generate bootstrap estimates to be used
#'  by [cond_indirect_effects()], [indirect_effect()],
#'  and [cond_indirect()],
#'
#' @details
#' It does nonparametric bootstrapping to generate bootstrap
#' estimates of the parameter estimates in a model fitted
#' either by [lavaan::sem()] or by a sequence of calls to
#' [lm()]. The stored estimates can then be used by
#' [cond_indirect_effects()], [indirect_effect()], and
#' [cond_indirect()] to form bootstrapping confidence
#' intervals.
#'
#' This approach removes the need to repeat bootstrapping in each call
#' to [cond_indirect_effects()], [indirect_effect()], and
#' [cond_indirect()]. It also ensures that the same set of bootstrap
#' samples is used in all subsequent analysis.
#'
#' It determines the type of the fit object automatically and then
#' calls [lm2boot_out()], [fit2boot_out()], or
#' [fit2boot_out_do_boot()].
#'
#' @return
#' A `boot_out`-class object that can be used for the `boot_out`
#' argument of [cond_indirect_effects()], [indirect_effect()], and
#' [cond_indirect()] for forming bootstrap confidence intervals.
#'
#' @param fit Either (a) a list of `lm` class objects, or
#'  the output of [lm2list()] (i.e., an `lm_list`-class
#'  object), or (b) the output of [lavaan::sem()].
#' @param R The number of bootstrap samples. Default is 100.
#' @param seed The seed for the bootstrapping.
#'             Default is `NULL` and seed is not set.
#' @param parallel Logical. Whether parallel processing will be used.
#'                 Default is `TRUE`. If `fit` is a list of
#'                 [lm()] outputs, parallel processing will not be used.
#' @param ncores Integer. The number of CPU cores to use when
#'               `parallel` is `TRUE`. Default is the number of
#'               non-logical cores minus one (one minimum). Will raise
#'               an error if greater than the number of cores detected
#'               by [parallel::detectCores()]. If `ncores` is set, it
#'               will override `make_cluster_args`.
#' @param make_cluster_args A named list of additional arguments to be
#'               passed to [parallel::makeCluster()]. For advanced
#'               users. See [parallel::makeCluster()] for details.
#'               Default is `list()`, no additional arguments.
#' @param progress Logical. Display progress or not. Default is `TRUE`.

#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm2boot_out()], [fit2boot_out()], and
#'  [fit2boot_out_do_boot()], which implemetns the
#'  bootstrapping.
#'
#' @examples
#' \dontrun{
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' # In real research, R should be 2000 or even 5000
#' lm_boot_out <- do_boot(lm_out, R = 100, seed = 1234)
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
#' }
#'
#' @export
#'
#'

do_boot <- function(fit,
                    R = 100,
                    seed = NULL,
                    parallel = TRUE,
                    ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                    make_cluster_args = list(),
                    progress = TRUE) {
    fit_type <- cond_indirect_check_fit(fit)
    if (fit_type == "lavaan") {
        fit_boot <- tryCatch(lavaan::lavInspect(fit, "boot"),
                             error = function(e) e)
        if (inherits(fit_boot, "error")) {
            has_boot <- FALSE
          } else {
            has_boot <- TRUE
          }
        if (has_boot) {
            out <- fit2boot_out(fit)
          } else {
            out <- fit2boot_out_do_boot(fit = fit,
                                        R = R,
                                        seed = seed,
                                        parallel = parallel,
                                        ncores = ncores,
                                        make_cluster_args = make_cluster_args,
                                        progress = progress)
          }
      }
    if (fit_type == "lm") {
        out <- lm2boot_out(outputs = fit,
                           R = R,
                           seed = seed)
      }
    return(out)
  }
