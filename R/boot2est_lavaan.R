#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + c4 * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' check_path(x = "x", y = "m3", m = c("m1", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m1", "m2"), fit = fit)
#'
#' @export
#' @describeIn fit2boot_out Process stored bootstrap estimates for functions
#'                          such as [cond_indirect_effects()].
#' @order 1
#'

fit2boot_out <- function(fit) {
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

#' @param R Number of bootstrap samples. Default is 100.
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
#'
#' @export
#' @describeIn fit2boot_out Do bootstrapping and store information to be used
#'                          by [cond_indirect_effects()] and related functions.
#'                          Support parallel processing and can be faster than
#'                          setting `se` to `"boot"` in [lavaan::sem()].
#' @order 2
#'
fit2boot_out_do_boot <- function(fit,
                                 R = 100,
                                 seed = NULL,
                                 parallel = FALSE,
                                 ncores = NULL,
                                 make_cluster_args =
                                    list(spec = getOption("cl.cores", 2))) {
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
            make_cluster_args <- modifyList(make_cluster_args,
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
        tmp <- tryCatch({rt <- system.time(out <- suppressWarnings(
                          parallel::parLapplyLB(cl, ids, boot_i,
                                                d = dat_org)))},
                          error = function(e) e)
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
    n_ok <- sapply(out, function(x) x$ok)
    out <- out[n_ok]
    if (isTRUE(!all(n_ok))) {
        n_failed <- R - length(n_ok)
        message(paste("Note: ", n, " bootstrap sample(s) failed.",
                      "They will be removed"))
        utils::flush.console()
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
                                baseline = FALSE,
                                h1 = FALSE))
        } else {
          out <- tryCatch(lavaan::update(fit_org,
                                data = d[i, ],
                                se = "none",
                                baseline = FALSE,,
                                h1 = FALSE),
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
