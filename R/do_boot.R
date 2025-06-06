#' @title Bootstrap Estimates for
#' 'indirect_effects' and
#' 'cond_indirect_effects'
#'
#' @description Generate bootstrap
#' estimates to be used by
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()],
#'
#' @details It does nonparametric
#' bootstrapping to generate bootstrap
#' estimates of the parameter estimates
#' in a model fitted either by
#' [lavaan::sem()] or by a sequence of
#' calls to [lm()]. The stored estimates
#' can then be used by
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()] to form
#' bootstrapping confidence intervals.
#'
#' This approach removes the need to
#' repeat bootstrapping in each call to
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()]. It also ensures
#' that the same set of bootstrap
#' samples is used in all subsequent
#' analysis.
#'
#' It determines the type of the fit
#' object automatically and then calls
#' [lm2boot_out()], [fit2boot_out()], or
#' [fit2boot_out_do_boot()].
#'
#' ## Multigroup Models
#'
#' Since Version 0.1.14.2, support for
#' multigroup models has been added for models
#' fitted by `lavaan`. The implementation
#' of bootstrapping is identical to
#' that used by `lavaan`, with resampling
#' done within each group.
#'
#' @return A `boot_out`-class object
#' that can be used for the `boot_out`
#' argument of
#' [cond_indirect_effects()],
#' [indirect_effect()], and
#' [cond_indirect()] for forming
#' bootstrap confidence intervals. The
#' object is a list with the number of
#' elements equal to the number of
#' bootstrap samples. Each element is a
#' list of the parameter estimates and
#' sample variances and covariances of
#' the variables in each bootstrap
#' sample.
#'
#' @param fit It can be (a) a list of `lm`
#' class objects, or the output of
#' [lm2list()] (i.e., an `lm_list`-class
#' object), or (b) the output of
#' [lavaan::sem()].
#' If it is a single model fitted by
#' [lm()], it will be automatically converted
#' to a list by [lm2list()].
#'
#' @param R The number of bootstrap
#' samples. Default is 100.
#'
#' @param seed The seed for the
#' bootstrapping. Default is `NULL` and
#' seed is not set.
#'
#' @param compute_implied_stats If
#' `TRUE`, default, implied statistics
#' will be computed for each bootstrap
#' sample. Letting users to disable this
#' is an experimental features to let
#' the process run faster.
#'
#' @param parallel Logical. Whether
#' parallel processing will be used.
#' Default is `TRUE`.
#'
#' @param ncores Integer. The number of
#' CPU cores to use when `parallel` is
#' `TRUE`. Default is the number of
#' non-logical cores minus one (one
#' minimum). Will raise an error if
#' greater than the number of cores
#' detected by
#' [parallel::detectCores()]. If
#' `ncores` is set, it will override
#' `make_cluster_args`.
#'
#' @param make_cluster_args A named list
#' of additional arguments to be passed
#' to [parallel::makeCluster()]. For
#' advanced users. See
#' [parallel::makeCluster()] for
#' details. Default is `list()`, no
#' additional arguments.
#'
#' @param progress Logical. Display
#' progress or not. Default is `TRUE`.
#'
#' @seealso [lm2boot_out()],
#' [fit2boot_out()], and
#' [fit2boot_out_do_boot()], which
#' implements the bootstrapping.
#'
#' @examples
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

do_boot <- function(fit,
                    R = 100,
                    seed = NULL,
                    parallel = TRUE,
                    ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                    make_cluster_args = list(),
                    progress = TRUE,
                    compute_implied_stats = TRUE) {
    if (!missing(fit)) {
         fit <- auto_lm2list(fit)
      }
    fit_type <- cond_indirect_check_fit(fit)
    if (fit_type == "lavaan.mi") {
        stop("Bootstrapping does not support multiple imputation.")
      }
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
                                        compute_implied_stats = compute_implied_stats,
                                        parallel = parallel,
                                        ncores = ncores,
                                        make_cluster_args = make_cluster_args,
                                        progress = progress)
          }
      }
    if (fit_type == "lm") {
        if (parallel) {
            out <- lm2boot_out_parallel(outputs = fit,
                                        R = R,
                                        seed = seed,
                                        compute_implied_stats = compute_implied_stats,
                                        parallel = parallel,
                                        ncores = ncores,
                                        make_cluster_args = make_cluster_args,
                                        progress = progress)
          } else {
            out <- lm2boot_out(outputs = fit,
                              R = R,
                              seed = seed,
                              compute_implied_stats = compute_implied_stats,
                              progress = progress)
          }
      }
    return(out)
  }
