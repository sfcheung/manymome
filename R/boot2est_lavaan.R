#' @title Bootstrap Estimates for a `lavaan` Output
#'
#' @description Generate bootstrap estimates from
#'  the output of [lavaan::sem()].
#'
#' @details
#' This function is for advanced users.
#' [do_boot()] is a function users should
#' try first because [do_boot()] has a general
#' interface for input-specific functions
#' like this one.
#'
#' If bootstrapping confidence intervals was requested
#' when calling [lavaan::sem()] by setting `se = "boot"`,
#' [fit2boot_out()] can be used to extract the stored
#' bootstrap estimates so that they can be reused by
#' [indirect_effect()], [cond_indirect_effects()] and
#' related functions
#' to form bootstrapping confidence intervals for
#' effects such as indirect effects and conditional indirect effects.
#'
#' If bootstrapping confidence was not requested
#' when fitting the model by [lavaan::sem()],
#' [fit2boot_out_do_boot()] can be used to generate nonparametric
#' bootstrap estimates from the output of [lavaan::sem()]
#' and store them for use by
#' [indirect_effect()], [cond_indirect_effects()], and related functions.
#'
#' This approach removes the need to repeat bootstrapping in
#' each call to [indirect_effect()], [cond_indirect_effects()],
#' and related functions.
#' It also ensures that the same set of bootstrap samples
#' is used in all subsequent analyses.
#'
#' @return A `boot_out`-class object that can be used for the
#' `boot_out` argument of [indirect_effect()],
#' [cond_indirect_effects()], and related
#' functions for forming bootstrapping confidence
#' intervals.
#' The object is a list with the number of
#' elements equal to the number of bootstrap samples.
#' Each element is a list of the parameter estimates
#' and sample variances and covariances of the variables
#' in each bootstrap sample.
#'
#' @param fit The fit object. This function
#'            only supports a
#'            [lavaan::lavaan-class] object.
#'
#' @seealso [do_boot()], the general purpose
#'          function that users should try first before
#'          using this function.
#' @examples
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
#'
#' # Bootstrapping not requested in calling lavaan::sem()
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE)
#' fit_boot_out <- fit2boot_out_do_boot(fit = fit,
#'                                      R = 40,
#'                                      seed = 1234)
#' out <- cond_indirect_effects(wlevels = "w",
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = fit,
#'                              boot_ci = TRUE,
#'                              boot_out = fit_boot_out)
#' out
#'
#' @export
#' @describeIn fit2boot_out Process stored bootstrap estimates for functions
#'                          such as [cond_indirect_effects()].
#' @order 1
#'

fit2boot_out <- function(fit) {
    # if (length(lavaan::lavNames(fit, "lv")) != 0) {
    #     stop(paste0("fit2boot_out() does not support a model with latent variables.",
    #                 "\nPlease use fit2boot_out_do_boot()."))
    #   }
    boot_est <- boot2est(fit)
    boot_implied <- boot2implied(fit)
    out <- mapply(function(x, y) list(est = x,
                                      implied_stats = y),
                  x = boot_est,
                  y = boot_implied,
                  SIMPLIFY = FALSE)
    names(out) <- names(boot_est)
    class(out) <- "boot_out"
    out
  }

#' @param R The number of bootstrap samples. Default is 100.
#' @param seed The seed for the random resampling. Default is `NULL`.
#' @param parallel Logical. Whether parallel processing will be used.
#'                 Default is `NULL`.
#' @param ncores Integer. The number of CPU cores to use when
#'               `parallel` is `TRUE`. Default is the number of
#'               non-logical cores minus one (one minimum). Will raise
#'               an error if greater than the number of cores detected
#'               by [parallel::detectCores()]. If `ncores` is set, it
#'               will override `make_cluster_args`.
#' @param make_cluster_args A named list of additional arguments to be passed
#'                          to [parallel::makeCluster()]. For advanced users.
#'                          See [parallel::makeCluster()] for details.
#'                          Default is `list()`.
#' @param progress Logical. Display progress or not. Default is `TRUE`.
#'
#' @export
#' @describeIn fit2boot_out Do bootstrapping and store information to be used
#'                          by [cond_indirect_effects()] and related functions.
#'                          Support parallel processing.
#' @order 2
#'
fit2boot_out_do_boot <- function(fit,
                                 R = 100,
                                 seed = NULL,
                                 parallel = FALSE,
                                 ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                                 make_cluster_args = list(),
                                 progress = TRUE) {
    environment(gen_boot_i) <- parent.frame()
    boot_i <- gen_boot_i(fit)
    dat_org <- lav_data_used(fit)
    n <- nrow(dat_org)
    boot_test <- suppressWarnings(boot_i(dat_org))
    if (!isTRUE(all.equal(unclass(lavaan::coef(fit)),
                          lavaan::coef(boot_test)[names(lavaan::coef(fit))]))) {
        stop(paste("Something is wrong.",
                    "This function cannot reproduce the results.",
                    "Please fit the model with se = 'boot'"))
      }
    ft <- lavaan::lavInspect(boot_test, "timing")$total
    requireNamespace("parallel", quietly = TRUE)
    if (!is.null(seed)) set.seed(seed)
    ids <- replicate(R, sample.int(n, replace = TRUE), simplify = FALSE)
    if (parallel) {
        if (is.numeric(ncores)) {
            ncores0 <- parallel::detectCores()
            if (ncores == ncores0) {
                warning(paste0("'ncores' >= The number of detected cores (",
                               ncores0,"). The computer may not be responsive",
                               " when bootstrapping is running."),
                        immediate. = TRUE)
                utils::flush.console()
              }
            if (ncores > ncores0) {
                ncores <- max(ncores0 - 1, 1L)
                # stop(paste0("'ncores' cannot be greater than",
                #             " the detected number of cores (", ncores0,")."))
              }
          } else {
            ncores <- 1L
          }
      } else {
        ncores <- 1L
      }
    if (ncores > 1L) {
        make_cluster_args <- utils::modifyList(make_cluster_args,
                                        list(spec = ncores))
        tmp <- tryCatch({cl <- do.call(parallel::makeCluster,
                                      make_cluster_args)},
                        error = function(e) e)
        has_cl <- !inherits(tmp, "error")
      } else {
        has_cl <- FALSE
      }
    if (has_cl) {
        texp <-  1.2 * R * ft[[1]] / length(cl)
        message(paste0(length(cl), " processes started to run bootstrapping."))
        message(paste0("The expected CPU time is about ",
                        round(texp, 2),
                        " second(s)."))
        utils::flush.console()
        pkgs <- .packages()
        pkgs <- rev(pkgs)
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {
                        sapply(pkgs,
                        function(x) library(x, character.only = TRUE))
                      })
        # No need. ids pregenerated.
        # parallel::clusterSetRNGStream(cl, seed)
        if (progress) {
            op_old <- pbapply::pboptions(type = "timer")
            tmp <- tryCatch({rt <- system.time(out <- suppressWarnings(
                              pbapply::pblapply(ids, boot_i,
                                                d = dat_org,
                                                cl = cl)))},
                              error = function(e) e)
            pbapply::pboptions(op_old)
          } else {
            tmp <- tryCatch({rt <- system.time(out <- suppressWarnings(
                              parallel::parLapplyLB(cl, ids, boot_i,
                                                    d = dat_org)))},
                              error = function(e) e)
          }
        if (inherits(tmp, "error")) {
            try(parallel::stopCluster(cl), silent = TRUE)
            stop("Running in parallel failed. Please set 'parallel' to FALSE.")
          }
        parallel::stopCluster(cl)
      } else {
        # texp <-  R * ft[[1]]
        # message(paste0("The expected CPU time is ",
        #                 round(texp, 2),
        #                 " second(s).\n",
        #                 "Could be faster if 'parallel' set to TRUE."))
        # utils::flush.console()
        if (progress) {
            rt <- system.time(out <- suppressWarnings(pbapply::pblapply(ids, boot_i,
                                                            d = dat_org)))
          } else {
            rt <- system.time(out <- suppressWarnings(lapply(ids, boot_i,
                                                            d = dat_org)))
          }
      }
    i_ok <- sapply(out, function(x) x$ok)
    out <- out[i_ok]
    n_ok <- sum(i_ok)
    if ((n_ok > 0) && (n_ok < R)) {
        n_failed <- R - n_ok
        message(paste0("Note: Estimation failed in ",
                       n_failed,
                       " bootstrap sample(s).",
                       " They were removed.",
                       " Effective number of bootstrap samples is ",
                       n_ok,
                       "."))
        utils::flush.console()
      } else if (n_ok == 0) {
        stop("Estimation failed in all bootstrap samples.")
      }
    class(out) <- "boot_out"
    out
  }

# Convert stored estimates to a list of parameter estimates tables.
# This is preferred because it is what users usually see.
#' @noRd

boot2est <- function(fit) {
    opt <- lavaan::lavInspect(fit, "options")
    if (opt$se != "bootstrap") {
        stop("'se' not set to 'bootstrap' when fitting the model.")
      }
    boot_est0 <- lavaan::lavInspect(fit, "boot")
    ptable <- lavaan::parameterTable(fit)
    p_free <- ptable$free > 0
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- lapply(boot_est, set_est_i,
                        fit = fit,
                        p_free = p_free)
    out_all
  }

# Get the implied statistics from stored parameter estimates
#' @noRd

boot2implied <- function(fit) {
    opt <- lavaan::lavInspect(fit, "options")
    if (opt$se != "bootstrap") {
        stop("'se' not set to 'bootstrap' when fitting the model.")
      }
    if (opt$fixed.x) {
        stop("'fixed.x' set to TRUE is not supported.")
      }
    boot_est0 <- lavaan::lavInspect(fit, "boot")
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- lapply(boot_est, get_implied_i,
                        fit = fit)
    out_all
  }

# Convert set the estimates in a parameter estimates tables.
#' @noRd

set_est_i <- function(est0, fit, p_free) {
    fit@ParTable$est[p_free] <- unname(est0)
    est0 <- lavaan::parameterEstimates(fit,
                                       se = FALSE,
                                       zstat = FALSE,
                                       pvalue = FALSE,
                                       ci = FALSE,
                                       rsquare = TRUE,
                                       remove.eq = FALSE,
                                       remove.ineq = FALSE,
                                       remove.def = FALSE,
                                       remove.nonfree = FALSE,
                                       remove.step1 = FALSE)
    est0
  }

# Get the implied statistics from a set of estimates
#' @noRd

get_implied_i <- function(est0, fit) {
    has_lv <- length(lavaan::lavNames(fit, "lv")) != 0
    if (has_lv) {
        p_free <- fit@ParTable$free > 0
        fit@ParTable$est[p_free] <- unname(est0)
        fit@Model@GLIST <- lavaan::lav_model_set_parameters(fit@Model,
                                                            est0)@GLIST
        implied_cov_all <- lavaan::lavInspect(fit, "cov.all")
        mod0 <- lavaan::lav_model_set_parameters(fit@Model, est0)
        implied_mean_ov <- lavaan::lavInspect(fit, "mean.ov")
        implied_mean_ov[] <- lavaan::lav_model_implied(mod0,
                                             GLIST = NULL,
                                             delta = TRUE)$mean[[1]][, 1]
        implied_mean_lv <- lavaan::lavInspect(fit, "mean.lv")
        implied_mean_lv[] <- NA
        implied <- list(cov = list(implied_cov_all),
                        mean = list(c(implied_mean_ov,
                                      implied_mean_lv)),
                        mean_lv = list(implied_mean_lv))
      } else {
        mod0 <- lavaan::lav_model_set_parameters(fit@Model, est0)
        implied <- lavaan::lav_model_implied(mod0,
                                             GLIST = NULL,
                                             delta = TRUE)
      }
    out <- lav_implied_all(fit)
    out_names <- names(out)
    implied_names <- names(implied)
    out1 <- out
    for (x in out_names) {
        if (x %in% implied_names) {
          out1[[x]][] <- implied[[x]][[1]]
        } else {
          out1[[x]][] <- NA
        }
      }
    if (has_lv) {
        out1$mean_lv <- implied$mean_lv[[1]]
      }
    out1
  }

# Create the function for bootstrapping.
# Return the parameter estimates and implied statistics.
#' @noRd

gen_boot_i <- function(fit) {
  fit_org <- eval(fit)
  # data_full <- lavaan::lavInspect(fit_org, "data")
  function(d, i = NULL) {
      if (is.null(i)) {
          return(lavaan::update(fit_org,
                                data = d,
                                se = "none",
                                baseline = FALSE))
        } else {
          out <- tryCatch(lavaan::update(fit_org,
                                data = d[i, ],
                                se = "none",
                                baseline = FALSE),
                          error = function(e) e,
                          warning = function(e) e)
          if (inherits(out, "error") || inherits(out, "warning")) {
              out1 <- list(est = NA,
                           implied_stats = NA,
                           ok = FALSE)
            } else {
              chk <- tryCatch(lavaan::lavTech(out, what = "post.check"),
                              warning = function(w) w)
              if (!isTRUE(chk) ||
                  !lavaan::lavTech(out, what = "converged")) {
                  out1 <- list(est = NA,
                              implied_stats = NA,
                              ok = FALSE)
                }
              implied <- list(cov = lavaan::lavInspect(out, "cov.all"),
                              mean = lavaan::lavInspect(out, "mean.ov"),
                              mean_lv = lavaan::lavInspect(out, "mean.lv"))
              out1 <- list(est = lavaan::parameterEstimates(
                                  out,
                                  se = FALSE,
                                  zstat = FALSE,
                                  pvalue = FALSE,
                                  ci = FALSE,
                                  rsquare = TRUE,
                                  remove.eq = FALSE,
                                  remove.ineq = FALSE,
                                  remove.def = FALSE,
                                  remove.nonfree = FALSE,
                                  remove.step1 = FALSE),
                            implied_stats = implied,
                            ok = TRUE)
            }
        }
      return(out1)
    }
  }
