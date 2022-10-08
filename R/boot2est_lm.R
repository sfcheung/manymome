#' @title Bootstrap Estimates for `lm`
#' Outputs
#'
#' @description Generate bootstrap
#' estimates for models in a list of
#' 'lm' outputs.
#'
#' @details This function is for
#' advanced users. [do_boot()] is a
#' function users should try first
#' because [do_boot()] has a general
#' interface for input-specific
#' functions like this one.
#'
#' It does nonparametric bootstrapping
#' to generate bootstrap estimates of
#' the regression coefficients in the
#' regression models of a list of [lm()]
#' outputs, or an `lm_list`-class object
#' created by [lm2list()]. The stored
#' estimates can be used by
#' [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions in forming
#' bootstrapping confidence intervals
#' for effects such as indirect effect
#' and conditional indirect effects.
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
#' The object is a list with the number
#' of elements equal to the number of
#' bootstrap samples. Each element is a
#' list of the parameter estimates and
#' sample variances and covariances of
#' the variables in each bootstrap
#' sample.
#'
#' @param outputs A list of `lm` class
#' objects, or the output of [lm2list()]
#' (i.e., an `lm_list`-class object).
#'
#' @param R The number of bootstrap
#' samples. Default is 100.
#'
#' @param seed The seed for the
#' bootstrapping. Default is `NULL` and
#' seed is not set.
#'
#' @param progress Whether progress will
#' be displayed. Default is `TRUE`.
#'
#'
#' @seealso [do_boot()], the general
#' purpose function that users should
#' try first before using this function.
#'
#' @examples
#'
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' # In real research, R should be 2000 or even 5000
#' # In real research, no need to set progress to FALSE
#' # Progress is displayed by default.
#' lm_boot_out <- lm2boot_out(lm_out, R = 100, seed = 1234,
#'                            progress = FALSE)
#' out <- cond_indirect_effects(wlevels = "w",
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = lm_out,
#'                              boot_ci = TRUE,
#'                              boot_out = lm_boot_out)
#' out
#'
#' @describeIn lm2boot_out Generate
#' bootstrap estimates using one process
#' (serial, without parallelization).
#'
#' @order 1
#'
#' @export
#'
#'

lm2boot_out <- function(outputs, R = 100, seed = NULL,
                        progress = TRUE) {
    out_type <- cond_indirect_check_fit(outputs)
    if (out_type != "lm") {
        stop("'outputs' must be a list of 'lm()' outputs.")
      }
    dat <- merge_model_frame(outputs)
    n <- nrow(dat)
    if (!is.null(seed)) set.seed(seed)
    if (progress) {
        out0 <- pbapply::pbreplicate(R, lm_boot2est_i(d = dat,
                                          i = sample.int(n, replace = TRUE),
                                          outputs = outputs), simplify = FALSE)
      } else {
        out0 <- replicate(R, lm_boot2est_i(d = dat,
                                          i = sample.int(n, replace = TRUE),
                                          outputs = outputs), simplify = FALSE)
      }
    class(out0) <- "boot_out"
    out0
  }

# Generate the function for bootstrapping.
# Return a parameter estimates tables.
#' @noRd

lm_boot2est_i <- function(d, i = NULL, outputs) {
    if (!is.null(i)) {
        d_i <- d[i, ]
      } else {
        d_i <- d
      }
    out_i <- lapply(outputs, stats::update, data = d_i)
    lm2ptable(out_i)
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
#' @describeIn lm2boot_out Generate
#' bootstrap estimates using parallel
#' processing.
#'
#' @order 2
#'
#' @export

lm2boot_out_parallel <- function(outputs,
                                 R = 100,
                                 seed = NULL,
                                 parallel = FALSE,
                                 ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                                 make_cluster_args = list(),
                                 progress = TRUE) {
    out_type <- cond_indirect_check_fit(outputs)
    if (out_type != "lm") {
        stop("'outputs' must be a list of 'lm()' outputs.")
      }
    dat_org <- merge_model_frame(outputs)
    n <- nrow(dat_org)
    ft <- system.time(lapply(outputs, stats::update, data = dat_org))[3]
    if (isTRUE(ft == 0)) ft <- .00001
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
        # message(paste0("The expected CPU time is about ",
        #                 round(texp, 2),
        #                 " second(s)."))
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
                              pbapply::pblapply(ids,
                                                lm_boot2est_i,
                                                d = dat_org,
                                                outputs = outputs,
                                                cl = cl)))},
                              error = function(e) e)
            pbapply::pboptions(op_old)
          } else {
            tmp <- tryCatch({rt <- system.time(out <- suppressWarnings(
                              parallel::parLapplyLB(cl,
                                                    ids,
                                                    lm_boot2est_i,
                                                    d = dat_org,
                                                    outputs = outputs)))},
                              error = function(e) e)
          }
        if (inherits(tmp, "error")) {
            try(parallel::stopCluster(cl), silent = TRUE)
            stop("Running in parallel failed. Please set 'parallel' to FALSE.")
          }
        parallel::stopCluster(cl)
      } else {
        if (progress) {
            rt <- system.time(out <- suppressWarnings(pbapply::pblapply(ids,
                                                            lm_boot2est_i,
                                                            d = dat_org,
                                                            outputs = outputs)))
          } else {
            rt <- system.time(out <- suppressWarnings(lapply(ids,
                                                            lm_boot2est_i,
                                                            d = dat_org,
                                                            outputs = outputs)))
          }
      }
    class(out) <- "boot_out"
    out
  }