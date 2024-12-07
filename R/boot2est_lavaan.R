#' @title Bootstrap Estimates for a
#' `lavaan` Output
#'
#' @description Generate bootstrap
#' estimates from the output of
#' [lavaan::sem()].
#'
#' @details This function is for
#' advanced users. [do_boot()] is a
#' function users should try first
#' because [do_boot()] has a general
#' interface for input-specific
#' functions like this one.
#'
#' If bootstrapping confidence intervals
#' was requested when calling
#' [lavaan::sem()] by setting `se =
#' "boot"`, [fit2boot_out()] can be used
#' to extract the stored bootstrap
#' estimates so that they can be reused
#' by [indirect_effect()],
#' [cond_indirect_effects()] and related
#' functions to form bootstrapping
#' confidence intervals for effects such
#' as indirect effects and conditional
#' indirect effects.
#'
#' If bootstrapping confidence was not
#' requested when fitting the model by
#' [lavaan::sem()],
#' [fit2boot_out_do_boot()] can be used
#' to generate nonparametric bootstrap
#' estimates from the output of
#' [lavaan::sem()] and store them for
#' use by [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions.
#'
#' This approach removes the need to
#' repeat bootstrapping in each call to
#' [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions. It also ensures
#' that the same set of bootstrap
#' samples is used in all subsequent
#' analyses.
#'
#' @return A `boot_out`-class object
#' that can be used for the `boot_out`
#' argument of [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions for forming
#' bootstrapping confidence intervals.
#'
#' The object is a list with the number
#' of elements equal to the number of
#' bootstrap samples. Each element is a
#' list of the parameter estimates and
#' sample variances and covariances of
#' the variables in each bootstrap
#' sample.
#'
#' @param fit The fit object. This
#' function only supports a
#' [lavaan::lavaan-class] object.
#'
#' @seealso [do_boot()], the general
#' purpose function that users should
#' try first before using this function.
#'
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
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' fit_boot_out <- fit2boot_out_do_boot(fit = fit,
#'                                      R = 40,
#'                                      seed = 1234,
#'                                      progress = FALSE)
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
#'
#' @describeIn fit2boot_out Process
#' stored bootstrap estimates for
#' functions such as
#' [cond_indirect_effects()].
#'
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

#' @param R The number of bootstrap
#' samples. Default is 100.
#'
#' @param seed The seed for the random
#' resampling. Default is `NULL`.
#'
#' @param parallel Logical. Whether
#' parallel processing will be used.
#' Default is `NULL`.
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
#' details. Default is `list()`.
#'
#' @param progress Logical. Display
#' progress or not. Default is `TRUE`.
#'
#' @param internal A list of arguments
#' to be used internally for debugging.
#' Default is `list()`.
#'
#' @export
#'
#' @describeIn fit2boot_out Do
#' bootstrapping and store information
#' to be used by
#' [cond_indirect_effects()] and related
#' functions. Support parallel
#' processing.
#'
#' @order 2
#'
fit2boot_out_do_boot <- function(fit,
                                 R = 100,
                                 seed = NULL,
                                 parallel = FALSE,
                                 ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                                 make_cluster_args = list(),
                                 progress = TRUE,
                                 internal = list()) {
    if (identical(internal$gen_boot, "update")) {
        environment(gen_boot_i_update) <- parent.frame()
        boot_i <- gen_boot_i_update(fit)
      } else {
        # environment(gen_boot_i_lavaan) <- parent.frame()
        boot_i <- gen_boot_i_lavaan(fit)
      }
    dat_org <- lav_data_used(fit,
                             drop_list_single_group = TRUE)
    ngp <- lavaan::lavTech(fit, "ngroups")
    if (ngp == 1) {
        n <- nrow(dat_org)
      } else {
        n <- sapply(dat_org, nrow)
      }
    boot_test <- suppressWarnings(boot_i(dat_org,
                                         start = lavaan::parameterTable(fit)$start))
    # Increase the tolerance for mutliple group model
    if (!isTRUE(all.equal(unclass(lavaan::coef(fit)),
                          lavaan::coef(boot_test)[names(lavaan::coef(fit))],
                          tolerance = sqrt(.Machine$double.eps * 1e08)))) {
        if (isFALSE(identical(internal$gen_boot, "update"))) {
            # Try again
            environment(gen_boot_i_lavaan) <- parent.frame()
            boot_i <- gen_boot_i_lavaan(fit)
            boot_test <- suppressWarnings(boot_i(dat_org,
                                                start = lavaan::parameterTable(fit)$start))
            if (!isTRUE(all.equal(unclass(lavaan::coef(fit)),
                          lavaan::coef(boot_test)[names(lavaan::coef(fit))],
                          tolerance = sqrt(.Machine$double.eps * 1e08)))) {
                stop(paste("Something is wrong with gen_boot_i_lavaan().",
                            "This function cannot reproduce the results.",
                            "Please fit the model with se = 'boot'"))
              }
          } else {
            stop(paste("Something is wrong with gen_boot_i_update().",
                        "This function cannot reproduce the results.",
                        "Please fit the model with se = 'boot'"))
          }
      }
    ft <- lavaan::lavInspect(boot_test, "timing")$total
    requireNamespace("parallel", quietly = TRUE)
    if (!is.null(seed)) set.seed(seed)
    if (ngp == 1) {
        ids <- replicate(R, sample.int(n, replace = TRUE), simplify = FALSE)
      } else {
        ids <- replicate(R, sapply(n, sample.int, replace = TRUE, simplify = FALSE),
                         simplify = FALSE)
      }
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
        if (has_cl) {
            on.exit(try(parallel::stopCluster(cl), silent = TRUE))
          }
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
         try(parallel::stopCluster(cl), silent = TRUE)
      } else {
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

# Convert stored estimates to a list of
# parameter estimates tables. This is
# preferred because it is what users
# usually see.
#' @noRd

boot2est <- function(fit) {
    opt <- lavaan::lavInspect(fit, "options")
    if (opt$se != "bootstrap") {
        stop("'se' not set to 'bootstrap' when fitting the model.")
      }
    boot_est0 <- lavaan::lavInspect(fit, "boot")
    tmp <- attr(boot_est0, "error.idx")
    if (length(tmp) > 0) {
        boot_est0 <- boot_est0[-tmp, ]
      }
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
    # if (opt$fixed.x) {
    #     stop("'fixed.x' set to TRUE is not supported.")
    #   }
    boot_est0 <- lavaan::lavInspect(fit, "boot")
    tmp <- attr(boot_est0, "error.idx")
    if (length(tmp) > 0) {
        boot_est0 <- boot_est0[-tmp, ]
      }
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- lapply(boot_est, get_implied_i,
                        fit = fit)
    out_all
  }

# Convert set the estimates in a parameter estimates tables.
#' @noRd

set_est_i <- function(est0, fit, p_free, est_df = NULL) {
    type <- NA
    if (inherits(fit, "lavaan")) {
        type <- "lavaan"
      }
    if (inherits(fit, "lavaan.mi")) {
        type <- "lavaan.mi"
      }
    if (isTRUE(is.na(type))) {
        stop("Object is not of a supported type.")
      }
    out <- switch(type,
                  lavaan = set_est_i_lavaan(est0 = est0,
                                            fit = fit,
                                            p_free = p_free,
                                            est_df = est_df),
                  lavaan.mi = set_est_i_lavaan_mi(est0 = est0,
                                                  fit = fit,
                                                  p_free = p_free,
                                                  est_df = est_df))
    out
  }


#' @noRd

set_est_i_lavaan <- function(est0, fit, p_free, est_df = NULL) {
    fit@ParTable$est[p_free] <- unname(est0)
    ptable <- as.data.frame(fit@ParTable)
    if (!is.null(est_df)) {
        est_df$est <- NULL
        if ("group" %in% colnames(est_df)) {
            est0 <- merge(est_df, ptable[, c("lhs", "op", "rhs", "block", "group", "est")],
                          sort = FALSE)
          } else {
            est0 <- merge(est_df, ptable[, c("lhs", "op", "rhs", "est")],
                          sort = FALSE)
          }
        class(est0) <- class(est_df)
        return(est0)
      } else {
        est0 <- lavaan::parameterEstimates(fit,
                                          se = FALSE,
                                          zstat = FALSE,
                                          pvalue = FALSE,
                                          ci = FALSE,
                                          rsquare = FALSE,
                                          remove.eq = FALSE,
                                          remove.ineq = FALSE,
                                          remove.def = FALSE,
                                          remove.nonfree = FALSE,
                                          remove.step1 = FALSE)
        return(est0)
      }
  }

#' @noRd

set_est_i_lavaan_mi <- function(est0, fit, p_free, est_df = NULL) {
    fit@ParTable$est[p_free] <- unname(est0)
    est0 <- lav_est(fit,
                    se = FALSE,
                    ci = FALSE,
                    est_df = est_df)
    est0
  }

# Get the implied statistics from a set of estimates
#' @noRd


get_implied_i <- function(est0, fit, fit_tmp = NULL) {
    type <- NA
    if (inherits(fit, "lavaan")) {
        type <- "lavaan"
      }
    if (inherits(fit, "lavaan.mi")) {
        type <- "lavaan.mi"
      }
    if (isTRUE(is.na(type))) {
        stop("Fit is not of a supported type.")
      }
    out <- switch(type,
                  lavaan = get_implied_i_lavaan(est0 = est0,
                                                fit = fit,
                                                fit_tmp = fit_tmp),
                  lavaan.mi = get_implied_i_lavaan_mi(est0 = est0,
                                                      fit = fit,
                                                      fit_tmp = fit_tmp))
    out
  }

#' @noRd

get_implied_i_lavaan <- function(est0, fit, fit_tmp = NULL) {
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
    ngroups <- lavaan::lavTech(fit, "ngroups")
    out_names <- names(out)
    implied_names <- names(implied)
    out1 <- out
    # For multigroup models, use the format of lavaan::lav_model_implied()
    # - Estimates than groups.
    for (x in out_names) {
        if (x %in% implied_names) {
          if (!is.null(implied[[x]][[1]])) {
              if (ngroups > 1) {
                  for (j in seq_len(ngroups)) {
                      if (is.null(dim(out1[[x]][[j]]))) {
                          tmp <- names(implied[[x]][[j]])
                          out1[[x]][[j]][tmp] <- implied[[x]][[j]]
                        } else {
                          out1[[x]][[j]][] <- implied[[x]][[j]]
                        }
                    }
                } else {
                  if (is.null(dim(implied[[x]][[1]])) &&
                      !is.null(names(implied[[x]][[1]]))) {
                      tmp <- names(implied[[x]][[1]])
                      out1[[x]][tmp] <- implied[[x]][[1]]
                    } else {
                      out1[[x]][] <- implied[[x]][[1]]
                    }
                }
            } else {
              if (ngroups > 1) {
                  for (j in seq_len(ngroups)) {
                      out1[[x]][[j]][] <- NA
                    }
                } else {
                  out1[[x]][] <- NA
                }
            }
        } else {
          if (ngroups > 1) {
              for (j in seq_len(ngroups)) {
                  out1[[x]][[j]][] <- NA
                }
            } else {
              out1[[x]][] <- NA
            }
        }
      }
    if (has_lv) {
        if (ngroups > 1) {
            for (j in seq_len(ngroups)) {
                out1[["mean_lv"]][[j]][] <- NA
              }
          } else {
            out1$mean_lv <- implied$mean_lv[[1]]
          }
      }
    out1
  }

#' @noRd

get_implied_i_lavaan_mi <- function(est0, fit, fit_tmp = NULL) {
    if (is.null(fit_tmp)) {
        fit_tmp <- methods::new("lavaan",
                      version = as.character(utils::packageVersion("lavaan")))
        fit_tmp@Model <- fit@Model
        fit_tmp@Data <- fit@Data
        fit_tmp@ParTable <- fit@ParTableList[[1]]
        fit_tmp@pta <- fit@pta
        fit_tmp@Options <- fit@Options
      }
    has_lv <- length(lavaan::lavNames(fit, "lv")) != 0
    if (has_lv) {
        p_free <- fit_tmp@ParTable$free > 0
        fit_tmp@ParTable$est[p_free] <- unname(est0)
        fit_tmp@Model@GLIST <- lavaan::lav_model_set_parameters(fit_tmp@Model,
                                                                est0)@GLIST
        implied_cov_all <- lavaan::lavInspect(fit_tmp, "cov.all")
        mod0 <- lavaan::lav_model_set_parameters(fit_tmp@Model, est0)
        # implied_mean_ov <- lavaan::lavInspect(fit_tmp, "mean.ov")
        implied_mean_ov <- lavaan::lav_model_implied(mod0,
                                             GLIST = NULL,
                                             delta = TRUE)$mean[[1]][, 1]
        if (is.numeric(implied_mean_ov)) {
            names(implied_mean_ov) <- lavaan::lavNames(fit_tmp, "ov")
            class(implied_mean_ov) <- c("lavaan.vector", class(implied_mean_ov))
          }
        # SF: `lavaan` raises an error in some cases for unknown reasons
        implied_mean_lv <- tryCatch(lavaan::lavInspect(fit_tmp, "mean.lv"),
                                    error = function(e) e)
        if (inherits(implied_mean_lv, "error")) {
            tmp <- lavaan::lavNames(fit_tmp, "lv")
            implied_mean_lv <- rep(NA, length(tmp))
            names(implied_mean_lv) <- tmp
          }
        implied_mean_lv[] <- NA
        implied <- list(cov = list(implied_cov_all),
                        mean = list(c(implied_mean_ov,
                                      implied_mean_lv)),
                        mean_lv = list(implied_mean_lv))
      } else {
        mod0 <- lavaan::lav_model_set_parameters(fit_tmp@Model, est0)
        implied <- lavaan::lav_model_implied(mod0,
                                             GLIST = NULL,
                                             delta = TRUE)
      }
    tmpnames1 <- c(lavaan::lavNames(fit_tmp, "ov"),
                   lavaan::lavNames(fit_tmp, "lv"))
    tmpnames2 <- lavaan::lavNames(fit_tmp, "lv")
    out <- list(cov = lav_implied_all(fit_tmp)$cov,
                mean = stats::setNames(rep(as.numeric(NA), length(tmpnames1)),
                                tmpnames1))
    if (has_lv) {
        tmp <- stats::setNames(rep(as.numeric(NA), length(tmpnames2)),
                        tmpnames2)
        class(tmp) <- c("lavaan.vector", class(tmp))
        out$mean_lv <- tmp
      }
    out_names <- names(out)
    implied_names <- names(implied)
    out1 <- out
    for (x in out_names) {
        if (x %in% implied_names) {
          if (!is.null(implied[[x]][[1]])) {
              if (is.null(dim(implied[[x]][[1]])) &&
                      !is.null(names(implied[[x]][[1]]))) {
                  tmp <- names(implied[[x]][[1]])
                  out1[[x]][tmp] <- implied[[x]][[1]]
                } else {
                  out1[[x]][] <- implied[[x]][[1]]
                }
            } else {
              out1[[x]][] <- NA
            }
        } else {
          out1[[x]][] <- NA
        }
      }
    out1
  }


# Create the function for bootstrapping.
# Return the parameter estimates and implied statistics.
# A new version using lavaan().
# Modelled after lavaan::lav_bootstrap_internal().
#' @noRd

gen_boot_i_lavaan <- function(fit) {
  fit_org <- eval(fit)
  # data_full <- lavaan::lavInspect(fit_org, "data")
  fit_data <- fit_org@Data
  fit_model <- fit_org@Model
  fit_sampstats <- fit_org@SampleStats
  fit_opts <- fit_org@Options
  fit_pt <- fit_org@ParTable

  fit_opts$verbose <- FALSE
  fit_opts$se <- "none"
  fit_opts$baseline <- FALSE
  fit_opts$h1 <- FALSE
  fit_opts$loglik <- FALSE
  fit_opts$test <- "none"

  X_old <- fit_data@X

  function(d, i = NULL, start = NULL) {
      force(fit_data)
      force(fit_model)
      force(fit_sampstats)
      force(fit_opts)
      force(fit_pt)
      force(fit)
      fit_pt1 <- fit_pt
      if (!is.null(start)) {
          fit_pt1$start <- start
        }
      if (is.null(i)) {
          return(lavaan::lavaan(slotData = fit_data,
                                slotModel = fit_model,
                                slotSampleStats = fit_sampstats,
                                slotOptions = fit_opts,
                                slotParTable = fit_pt1))
        } else {
          # 2024-03-29: Added support for multigroup models
          if (!is.list(i)) {
              b_i <- list(i)
            } else {
              b_i <- i
            }
          X_new <- X_old
          for (j in seq_along(X_new)) {
              X_new[[j]] <- X_new[[j]][b_i[[j]], , drop = FALSE]
            }
          fit_data_new <- lavaan::lav_data_update(
                                    lavdata = fit_data,
                                    newX = X_new,
                                    BOOT.idx = b_i,
                                    lavoptions = fit_opts)
          fit_sampstats_new <- tryCatch(lavaan::lav_samplestats_from_data(
                                         lavdata = fit_data_new,
                                         lavoptions = fit_opts),
                                        error = function(e) e,
                                        warning = function(e) e)
          if (inherits(fit_sampstats_new, "error") ||
              inherits(fit_sampstats_new, "warning")) {
              out1 <- list(est = NA,
                           implied_stats = NA,
                           ok = FALSE)
              return(out1)
            }

          # Use the approach in lavaan::lav_bootstrap_internal
          if (fit_model@fixed.x &&
              length(lavaan::lavNames(fit, "ov.x")) > 0) {
              fit_model_i <- NULL
            } else {
              fit_model_i <- fit_model
            }

          out <- tryCatch(lavaan::lavaan(
                                    slotData = fit_data,
                                    slotModel = fit_model_i,
                                    slotSampleStats = fit_sampstats_new,
                                    slotOptions = fit_opts,
                                    slotParTable = fit_pt1),
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
              # If ngroups > 1,
              #   cov, mean, and mean_lv are lists.
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


# Create the function for bootstrapping.
# Return the parameter estimates and implied statistics.
#' @noRd

gen_boot_i_update <- function(fit) {
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
