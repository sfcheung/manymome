#' @title Bootstrapping Estimates for a `lavaan` Output
#'
#' @description Generate bootstrapping estimates from
#'  the output of [lavaan::sem()].
#'
#' @details
#' This function is for advanced users. 
#' [do_boot()] is a function users should
#' try first because it has a general
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
#' when fitting the model by [lavaan::semm()],
#' [fit2boot_out_do_boot()] can be used to generate nonparametric
#' bootstrap estimates from the output of [lavaan::sem()]
#' and store them for use by
#' [indirect_effect()], [cond_indirect_effects()], and related functions.
#'
#' This approach removes the need to repeat bootstrapping in
#' each call to [indirect_effect()], [cond_indirect_effects()], and related functions.
#' It also ensures that the same set of bootstrap samples
#' is used in all subsequent analyses.
#'
#' @return A `boot_out`-class object that can be used for the
#' `boot_out` argument of [indirect_effect()],
#' [cond_indirect_effects()], and related
#' functions for forming bootstrapping confidence
#' intervals.
#'
#' @param fit The fit object. This function
#'            only supports a
#'            [lavaan::lavaan-class] object.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [do_boot()], the general purpose
#'          function that users should try first.
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
#' # Bootstrapping confidence interval requested in lavaan::sem()
#' # bootstrap should be set to 2000 or even 5000 in real study
#' set.seed(1234)
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE,
#'            se = "boot", bootstrap = 10)
#' fit_boot_out <- fit2boot_out(fit)
#' wlevels <- mod_levels(w = "w", fit = fit)
#' out <- cond_indirect_effects(wlevels = wlevels,
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = fit,
#'                              boot_ci = TRUE,
#'                              boot_out = fit_boot_out)
#' out
#' # Bootstrapping not requested in calling lavaan::sem()
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE)
#' fit_boot_out <- fit2boot_out_do_boot(fit = fit,
#'                                      R = 10,
#'                                      seed = 1234)
#' wlevels <- mod_levels(w = "w", fit = fit)
#' out <- cond_indirect_effects(wlevels = wlevels,
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
    if (length(lavaan::lavNames(fit, "lv")) != 0) {
        stop(paste0("fit2boot_out() does not support a model with latent variables.",
                    "\nPlease use fit2boot_out_do_boot()."))
      }
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
#' @param ncores Integer. The number of CPU cores to use when `parallel` is `TRUE`.
#'               Default is `NULL`, and the number of cores determined by
#'               `getOption("cl.cores", 2)`. Will raise an error if greater than
#'               the number of cores detected by [parallel::detectCores()].
#'               If `ncores` is set, it will override `make_cluster_args`.
#' @param make_cluster_args A named list of additional arguments to be passed
#'                          to [parallel::makeCluster()]. For advanced users.
#'                          See [parallel::makeCluster()] for details.
#'                          Default is `list(spec = getOption("cl.cores", 2))`.
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
                                 ncores = NULL,
                                 make_cluster_args =
                                    list(spec = getOption("cl.cores", 2)),
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
    set.seed(seed)
    ids <- replicate(R, sample.int(n, replace = TRUE), simplify = FALSE)
    if (parallel & requireNamespace("parallel", quietly = TRUE)) {
        pkgs <- .packages()
        pkgs <- rev(pkgs)
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
                stop(paste0("'ncores' cannot be greater than",
                            " the detected number of cores (", ncores0,")."))
              }
            make_cluster_args <- utils::modifyList(make_cluster_args,
                                            list(spec = ncores))
          }
        cl <- do.call(parallel::makeCluster, make_cluster_args)
        texp <-  R * ft[[1]] / length(cl)
        message(paste0(length(cl), " processes started to run bootstrapping."))
        message(paste0("The expected CPU time is about ",
                        round(texp, 2),
                        " second(s)."))
        utils::flush.console()
        parallel::clusterExport(cl, "pkgs", envir = environment())
        parallel::clusterEvalQ(cl, {
                        sapply(pkgs,
                        function(x) library(x, character.only = TRUE))
                      })
        parallel::clusterSetRNGStream(cl, seed)
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
        texp <-  R * ft[[1]]
        message(paste0("The expected CPU time is ",
                        round(texp, 2),
                        " second(s).\n",
                        "Could be faster if 'parallel' set to TRUE."))
        utils::flush.console()
        rt <- system.time(out <- suppressWarnings(lapply(ids, boot_i,
                                                         d = dat_org)))
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

get_implied_i <- function(est0, fit) {
    # fit@ParTable$est[p_free] <- unname(est0)
    # fit@Model@GLIST <- lavaan::lav_model_set_parameters(fit@Model,
    #                                                     est0)@GLIST
    # implied <- lavaan::lavInspect(fit, "implied")
    # implied
    mod0 <- lavaan::lav_model_set_parameters(fit@Model, est0)
    implied <- lavaan::lav_model_implied(mod0,
                                         GLIST = NULL,
                                         delta = TRUE)
    out <- lavaan::lavInspect(fit, "implied")
    out <- lav_implied_all(fit)
    out_names <- names(out)
    out1 <- out
    for (x in out_names) {
        out1[[x]][] <- implied[[x]][[1]]
      }
    out1
  }

gen_boot_i <- function(fit) {
  fit_org <- eval(fit)
  data_full <- lavaan::lavInspect(fit_org, "data")
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
