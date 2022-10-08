#' @title Conditional, Indirect, and
#' Conditional Indirect Effects
#'
#' @description Compute the conditional
#' effects, indirect effects, or
#' conditional indirect effects in a
#' structural model fitted by [lm()] or
#' [lavaan::sem()].
#'
#' @details
#'
#' For a model with a mediation path
#' moderated by one or more moderators,
#' [cond_indirect_effects()] can be used
#' to compute the conditional indirect
#' effect from one variable to another
#' variable, at one or more set of
#' selected value(s) of the
#' moderator(s).
#'
#' If only the effect for one set of
#' value(s) of the moderator(s) is
#' needed, [cond_indirect()] can be
#' used.
#'
#' If only the mediator(s) is/are
#' specified (`m`) and no values of
#' moderator(s) are specified, then the
#' indirect effect from one variable
#' (`x`) to another variable (`y`) is
#' computed. A convenient wrapper
#' [indirect_effect()] can be used to
#' compute the indirect effect.
#'
#' If only the value(s) of moderator(s)
#' is/are specified (`wvalues` or
#' `wlevels`) and no mediators (`m`) are
#' specified when calling
#' [cond_indirect_effects()] or
#' [cond_indirect()], then the
#' conditional direct effects from one
#' variable to another are computed.
#'
#' All three functions support using
#' nonparametric bootstrapping to form
#' percentile confidence intervals.
#' Bootstrapping only needs to be done
#' once. These are the possible ways to
#' form bootstrapping:
#'
#' 1. Do bootstrapping in the first call
#' to one of these functions, by setting
#' `boot_ci` to `TRUE` and `R` to the
#' number of bootstrap samples, `level`
#' to the level of confidence (default
#' .95 or 95%), and `seed` to reproduce
#' the results (`parallel` and `ncores`
#' are optional). This will take some
#' time to run. The output will have all
#' bootstrap estimates stored. This
#' output, whether it is from
#' [indirect_effect()],
#' [cond_indirect_effects()], or
#' [cond_indirect()], can be reused by
#' any of these three functions by
#' setting `boot_out` to this output.
#' They will form the confidence
#' intervals using the stored bootstrap
#' estimates.
#'
#' 2. Do bootstrapping using
#' [do_boot()]. The output can be used
#' in the `boot_out` argument of
#' [indirect_effect()],
#' [cond_indirect_effects()] and
#' [cond_indirect()].
#'
#' 3. If [lavaan::sem()] is used to fit
#' a model and `se = "boot"` is used,
#' [do_boot()] can extract them to
#' generate a `boot_out`-class object
#' that again can be used in the
#' `boot_out` argument.
#'
#' If `boot_out` is set, arguments such
#' as `R`, `seed`, and `parallel` will
#' be ignored.
#'
#' @return [indirect_effect()] and
#' [cond_indirect()] return an
#' `indirect`-class object.
#'
#' [cond_indirect_effects()] returns a
#' `cond_indirect_effects`-class object.
#'
#' These two classes of objects have
#' their own print methods for printing
#' the results (see [print.indirect()]
#' and [print.cond_indirect_effects()]).
#' They also have a `coef` method for
#' extracting the estimates
#' ([coef.indirect()] and
#' [coef.cond_indirect_effects()]) and a
#' `confint` method for extracting the
#' confidence intervals
#' ([confint.indirect()] and
#' [confint.cond_indirect_effects()]).
#' Addition and subtraction can also be
#' conducted on `indirect`-class object
#' to estimate and test a function of
#' effects (see [math_indirect])
#'
#' @param x Character. The name of the
#' predictor at the start of the path.
#'
#' @param y Character. The name of the
#' outcome variable at the end of the
#' path.
#'
#' @param m A vector of the variable
#' names of the mediator(s). The path
#' goes from the first mediator
#' successively to the last mediator. If
#' `NULL`, the default, the path goes
#' from `x` to `y`.
#'
#' @param fit The fit object. Can be a
#' [lavaan::lavaan-class] object or a
#' list of [lm()] outputs.
#'
#' @param est The output of
#' [lavaan::parameterEstimates()]. If
#' `NULL`, the default, it will be
#' generated from `fit`. If supplied,
#' `fit` will be ignored.
#'
#' @param implied_stats Implied means,
#' variances, and covariances of
#' observed variables, of the form of
#' the output of [lavaan::lavInspect()]
#' with `what` set to `"implied"`. The
#' standard deviations are extracted
#' from this object for standardization.
#' Default is `NULL`, and implied
#' statistics will be computed from
#' `fit` if required.
#'
#' @param wvalues A numeric vector of
#' named elements. The names are the
#' variable names of the moderators, and
#' the values are the values to which
#' the moderators will be set to.
#' Default is `NULL`.
#'
#' @param standardized_x Logical.
#' Whether `x` will be standardized.
#' Default is `FALSE`.
#'
#' @param standardized_y Logical.
#' Whether `y` will be standardized.
#' Default is `FALSE`.
#'
#' @param boot_ci Logical. Whether
#' bootstrap confidence interval will be
#' formed. Default is `FALSE`.
#'
#' @param level The level of confidence
#' for the bootstrap confidence
#' interval. Default is .95.
#'
#' @param boot_out If `boot_ci` is
#' `TRUE`, users can supply pregenerated
#' bootstrap estimates. This can be the
#' output of [do_boot()]. For
#' [indirect_effect()] and
#' [cond_indirect_effects()], this can
#' be the output of a previous call to
#' [cond_indirect_effects()],
#' [indirect_effect()], or
#' [cond_indirect()] with bootstrap
#' confidence intervals requested. These
#' stored estimates will be reused such
#' that there is no need to do
#' bootstrapping again. If not supplied,
#' the function will try to generate
#' them from `fit`.
#'
#' @param R Integer. If `boot_ci` is
#' `TRUE`, `boot_out` is `NULL`, and
#' bootstrap standard errors not
#' requested if `fit` is a
#' [lavaan-class] object, this function
#' will do bootstrapping on `fit`. `R`
#' is the number of bootstrap samples.
#' Default is 100.
#'
#' @param seed If bootstrapping is
#' conducted, this is the seed for the
#' bootstrapping. Default is `NULL` and
#' seed is not set.
#'
#' @param parallel Logical. If
#' bootstrapping is conducted, whether
#' parallel processing will be used.
#' Default is `TRUE`. If `fit` is a list
#' of [lm()] outputs, parallel
#' processing will not be used.
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
#' `make_cluster_args` in [do_boot()].
#'
#' @param make_cluster_args A named list
#' of additional arguments to be passed
#' to [parallel::makeCluster()]. For
#' advanced users. See
#' [parallel::makeCluster()] for
#' details. Default is `list()`.
#'
#' @param progress Logical. Display
#' bootstrapping progress or not.
#' Default is `TRUE`.
#'
#' @param wlevels The output of
#' [merge_mod_levels()], or the
#' moderator(s) to be passed to
#' [mod_levels_list()]. If all the
#' moderators can be represented by one
#' variable, that is, each moderator is
#' (a) a numeric variable, (b) a
#' dichotomous categorical variable, or
#' (c) a factor or string variable used
#' in [lm()] in `fit`, then it is a
#' vector of the names of the moderators
#' as appeared in the data frame. If at
#' least one of the moderators is a
#' categorical variable represented by
#' more than one variable, such as
#' user-created dummy variables used in
#' [lavaan::sem()], then it must be a
#' list of the names of the moderators,
#' with such moderators represented by a
#' vector of names. For example:
#' `list("w1", c("gpgp2", "gpgp3")`, the
#' first moderator `w1` and the second
#' moderator a three-categorical
#' variable represented by `gpgp2` and
#' `gpgp3`.
#'
#' @param ... Arguments to be passed to
#' [cond_indirect()]
#'
#' @param output_type The type of output
#' of [cond_indirect_effects()]. If
#' `"data.frame"`, the default, the
#' output will be converted to a data
#' frame. If any other values, the
#' output is a list of the outputs from
#' [cond_indirect()].
#'
#' @param save_boot_full If `TRUE`, full
#' bootstrapping results will be stored.
#' Default is `FALSE.`
#'
#' @param prods The product terms found. For internal use.
#'
#' @param get_prods_only IF `TRUE`, will
#' quit early and return the product
#' terms found. The results can be
#' passed to the `prod` argument when
#' calling this function. Default is
#' `FALSE`. This function is for
#' internal use.
#'
#' @param save_boot_out If `boot_out` is
#' supplied, whether it will be saved in
#' the output. Default is `TRUE`.
#'
#'
#' @seealso [mod_levels()] and
#' [merge_mod_levels()] for generating
#' levels of moderators. [do_boot] for
#' doing bootstrapping before calling
#' these functions.
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x  + d1 * w1 + e1 * x:w1
#' m2 ~ a2 * x
#' y  ~ b1 * m1 + b2 * m2 + cp * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#' hi_w1 <- mean(dat$w1) + sd(dat$w1)
#'
#' # Examples for cond_indirect():
#'
#' # Conditional effect from x to m1 when w1 is 1 SD above mean
#' cond_indirect(x = "x", y = "m1",
#'               wvalues = c(w1 = hi_w1), fit = fit)
#'
#' # Indirect effect from x1 through m2 to y
#' indirect_effect(x = "x", y = "y", fit = fit)
#'
#' # Conditional Indirect effect from x1 through m1 to y, when w1 is 1 SD above mean
#' cond_indirect(x = "x", y = "y", m = "m1",
#'               wvalues = c(w1 = hi_w1), fit = fit)
#'
#'
#'
#' @export
#'
#' @describeIn cond_indirect Compute
#' conditional, indirect, or conditional
#' indirect effects for one set of
#' levels.
#'
#' @order 1

cond_indirect <- function(x,
                     y,
                     m = NULL,
                     fit = NULL,
                     est = NULL,
                     implied_stats = NULL,
                     wvalues = NULL,
                     standardized_x = FALSE,
                     standardized_y = FALSE,
                     boot_ci = FALSE,
                     level = .95,
                     boot_out = NULL,
                     R = 100,
                     seed = NULL,
                     parallel = TRUE,
                     ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                     make_cluster_args = list(),
                     progress = TRUE,
                     save_boot_full = FALSE,
                     prods = NULL,
                     get_prods_only = FALSE,
                     save_boot_out = TRUE) {
    fit_type <- cond_indirect_check_fit(fit)
    chkpath <- check_path(x = x, y = y, m = m, fit = fit, est = est)
    if (!chkpath) {
        msg <- paste0("No path from ", sQuote(x), " to ", sQuote(y),
                      ifelse(is.null(m), "",
                                         paste0(" through ",
                                                paste0(sQuote(m), collapse = ", "))),
                      ". ",
                      "Please check the arguments x, y, and m.")
        stop(msg)
      }
    if (boot_ci) {
        if (!is.null(boot_out)) {
            if (inherits(boot_out, "cond_indirect_effects")) {
                boot_out <- attr(boot_out, "boot_out")
                if (is.null(boot_out)) {
                    stop("boot_out not found in the supplied object for 'boot_out'")
                  }
              }
            if (inherits(boot_out, "indirect")) {
                boot_out <- boot_out$boot_out
                if (is.null(boot_out)) {
                    stop("boot_out not found in the supplied object for 'boot_out'")
                  }
              }
            if (!inherits(boot_out, "boot_out")) {
                stop("The object at 'boot_out' must be of the class 'boot_out'.")
              }
          } else {
            boot_out <- do_boot(fit = fit,
                                R = R,
                                seed = seed,
                                parallel = parallel,
                                ncores = ncores,
                                make_cluster_args = make_cluster_args,
                                progress = progress)
          }
      }
    if (fit_type == "lavaan") {
        fit0 <- fit
        if (is.null(est)) est <- lavaan::parameterEstimates(fit)
        if (is.null(implied_stats)) implied_stats <- lav_implied_all(fit)
        fit_data <- lavaan::lavInspect(fit, "data")
      }
    if (fit_type == "lm") {
        fit0 <- NULL
        lm_est <- lm2ptable(fit)
        if (is.null(est)) est <- lm_est$est
        if (is.null(implied_stats)) implied_stats <- lm_est$implied_stats
        fit_data <- lm_est$data
      }
    if (is.null(prods)) {
        prods <- indirect_i(x = x,
                        y = y,
                        m = m,
                        fit = fit0,
                        est = est,
                        implied_stats = implied_stats,
                        wvalues = wvalues,
                        standardized_x = standardized_x,
                        standardized_y = standardized_y,
                        get_prods_only = TRUE,
                        data = fit_data,
                        expand = TRUE)
      }
    if (get_prods_only) return(prods)
    out0 <- indirect_i(x = x,
                     y = y,
                     m = m,
                     fit = fit0,
                     est = est,
                     implied_stats = implied_stats,
                     wvalues = wvalues,
                     standardized_x = standardized_x,
                     standardized_y = standardized_y,
                     prods = prods)
    if (boot_ci) {
        out_boot <- mapply(indirect_i,
                           est = lapply(boot_out, function(x) x$est),
                           implied_stats = lapply(boot_out, function(x) x$implied_stats),
                           MoreArgs = list(x = x,
                                           y = y,
                                           m = m,
                                           fit = fit0,
                                           wvalues = wvalues,
                                           standardized_x = standardized_x,
                                           standardized_y = standardized_y,
                                           warn = FALSE,
                                           prods = prods),
                           SIMPLIFY = FALSE)
        if (save_boot_full) {
            out0$boot_full <- out_boot
          }
        nboot <- length(out_boot)
        out0$boot_indirect <- sapply(out_boot, function(x) x$indirect)
        tmp <- list(t = matrix(out0$boot_indirect, nrow = nboot, ncol = 1),
                    t0 = out0$indirect,
                    R = nboot)
        boot_ci <- boot::boot.ci(tmp, conf = level, type = "perc")
        boot_ci1 <- boot_ci$percent[4:5]
        names(boot_ci1) <- paste0(formatC(c(100 * (1 - level) / 2,
                                     100 * (1 - (1 - level) / 2)), 2,
                                     format = "f"), "%")
        out0$boot_ci <- boot_ci1
        out0$level <- level
        if (save_boot_out) {
            out0$boot_out <- boot_out
          } else {
            out0$boot_out <- NULL
          }
      }
    out0$cond_indirect_call <- match.call()
    out0
  }

#' @export
#'
#' @describeIn cond_indirect Compute the
#' indirect effect. A wrapper of
#' [cond_indirect()]. Can be used when
#' there is no moderator.
#'
#' @order 3

indirect_effect <- function(x,
                     y,
                     m = NULL,
                     fit = NULL,
                     est = NULL,
                     implied_stats = NULL,
                     standardized_x = FALSE,
                     standardized_y = FALSE,
                     boot_ci = FALSE,
                     level = .95,
                     boot_out = NULL,
                     R = 100,
                     seed = NULL,
                     parallel = TRUE,
                     ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                     make_cluster_args = list(),
                     progress = TRUE,
                     save_boot_full = FALSE) {
    cond_indirect(x = x,
                  y = y,
                  m = m,
                  fit = fit,
                  est = est,
                  implied_stats = implied_stats,
                  standardized_x = standardized_x,
                  standardized_y = standardized_y,
                  boot_ci = boot_ci,
                  level = level,
                  boot_out = boot_out,
                  R = R,
                  seed = seed,
                  parallel = parallel,
                  ncores = ncores,
                  make_cluster_args = make_cluster_args,
                  progress = progress,
                  save_boot_full = save_boot_full)
  }

#' @param w_type Character. Whether the
#' moderator is a `"numeric"` variable
#' or a `"categorical"` variable. If
#' `"auto"`, the function will try to
#' determine the type automatically.
#' See [mod_levels_list()] for further
#' information.
#'
#' @param w_method Character, either
#' `"sd"` or `"percentile"`. If `"sd"`,
#' the levels are defined by the
#' distance from the mean in terms of
#' standard deviation. if
#' `"percentile"`, the levels are
#' defined in percentiles.  See
#' [mod_levels_list()] for further
#' information.
#'
#' @param sd_from_mean A numeric vector.
#' Specify the distance in standard
#' deviation from the mean for each
#' level. Default is `c(-1, 0, 1)` when
#' there is only one moderator, and
#' `c(-1, 1)` when there are more than
#' one moderator. Ignored if `w_method`
#' is not equal to `"sd"`. See
#' [mod_levels_list()] for further
#' information.
#'
#' @param percentiles A numeric vector.
#' Specify the percentile (in
#' proportion) for each level. Default
#' is `c(.16, .50, .84)` if there is one
#' moderator, and `c(.16, .84)` when
#' there are more than one moderator.
#' Ignored if `w_method` is not equal to
#' `"percentile"`. See
#' [mod_levels_list()] for further
#' information.
#'
#' @param mod_levels_list_args
#' Additional arguments to be passed to
#' [mod_levels_list()] if it is called
#' for creating the levels of
#' moderators. Default is `list()`.
#'
#' @examples
#' # Examples for cond_indirect_effects():
#'
#' # Create levels of w1, the moderators
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = w1levels, fit = fit)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the levels
#' cond_indirect_effects(x = "x", y = "y", m = "m1",
#'                       wlevels = w1levels, fit = fit)
#'
#' @export
#'
#' @describeIn cond_indirect Compute the
#' conditional effects or conditional
#' indirect effects for several sets of
#' levels of the moderator(s).
#'
#' @order 2


cond_indirect_effects <- function(wlevels,
                                  x,
                                  y,
                                  m = NULL,
                                  fit = NULL,
                                  w_type = "auto",
                                  w_method = "sd",
                                  sd_from_mean = NULL,
                                  percentiles = NULL,
                                  est = NULL,
                                  implied_stats = NULL,
                                  boot_ci = FALSE,
                                  R = 100,
                                  seed = NULL,
                                  parallel = TRUE,
                                  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                                  make_cluster_args = list(),
                                  progress = TRUE,
                                  boot_out = NULL,
                                  output_type = "data.frame",
                                  mod_levels_list_args = list(),
                                  ...) {
    if (!missing(wlevels)) {
        wlevels_check <- check_wlevels(wlevels)
        if (!is.null(wlevels_check)) {
            wlevels <- wlevels_check
          } else {
            # Call mod_levels_list
            # Case 1: A character vector
            # Case 2: A list of character vectors
            mod_levels_list_args_final <-
              utils::modifyList(mod_levels_list_args,
                                list(w_type = w_type,
                                     w_method = w_method,
                                     sd_from_mean = sd_from_mean,
                                     percentiles = percentiles,
                                     fit = fit,
                                     merge = TRUE))
            wlevels <- do.call(mod_levels_list,
                          args = c(as.list(wlevels),
                                   mod_levels_list_args_final))

          }
      } else {
        stop("wlevels is required.")
      }
    k <- nrow(wlevels)
    wlevels1 <- split(wlevels, seq_len(k))
    wlevels2 <- lapply(wlevels1, unlist)
    names(wlevels2) <- rownames(wlevels)
    fit_type <- cond_indirect_check_fit(fit)
    if ((fit_type == "lm") && !inherits(fit, "lm_list") &&
        is.list(fit)) {
        fit <- lm2list(fit)
      }
    if (boot_ci) {
        if (!is.null(boot_out)) {
            if (inherits(boot_out, "cond_indirect_effects")) {
                boot_out <- attr(boot_out, "boot_out")
                if (is.null(boot_out)) {
                    stop("boot_out not found in the supplied object for 'boot_out'")
                  }
              }
            if (inherits(boot_out, "indirect")) {
                boot_out <- boot_out$boot_out
                if (is.null(boot_out)) {
                    stop("boot_out not found in the supplied object for 'boot_out'")
                  }
              }
            if (!inherits(boot_out, "boot_out")) {
                stop("The object at 'boot_out' must be of the class 'boot_out'.")
              }
          } else {
            boot_out <- do_boot(fit = fit,
                                R = R,
                                seed = seed,
                                parallel = parallel,
                                ncores = ncores,
                                make_cluster_args = make_cluster_args,
                                progress = progress)
          }
      }
    prods <- cond_indirect(wvalues = wlevels2[[1]],
                            x = x,
                            y = y,
                            m = m,
                            fit = fit,
                            est = est,
                            implied_stats = implied_stats,
                            get_prods_only = TRUE,
                            ...)
    out <- lapply(wlevels2,
                  function(wv,
                           x,
                           y,
                           m,
                           fit,
                           est,
                           implied_stats,
                           boot_ci,
                           boot_out,
                           R,
                           seed,
                           prods,
                           save_boot_out,
                           ...) {
                              cond_indirect(wvalues = wv,
                                            x = x,
                                            y = y,
                                            m = m,
                                            fit = fit,
                                            est = est,
                                            implied_stats = implied_stats,
                                            boot_ci = boot_ci,
                                            boot_out = boot_out,
                                            R = R,
                                            seed = seed,
                                            prods = prods,
                                            save_boot_out = FALSE,
                                            ...)
                           },
                  x = x,
                  y = y,
                  m = m,
                  fit = fit,
                  est = est,
                  implied_stats = implied_stats,
                  boot_ci = boot_ci,
                  boot_out = boot_out,
                  R = R,
                  seed = seed,
                  prods = prods,
                  save_boot_out = FALSE,
                  ...)
    if (output_type == "data.frame") {
        out1 <- cond_indirect_effects_to_df(out, wlevels = wlevels)
        class(out1) <- c("cond_indirect_effects", class(out1))
        attr(out1, "call") <- match.call()
        attr(out1, "full_output") <- out
        attr(out1, "wlevels") <- wlevels
        attr(out1, "fit") <- fit
        attr(out1, "est") <- est
        attr(out1, "implied_stats") <- implied_stats
        attr(out1, "boot_out") <- boot_out
        attr(out1, "prods") <- prods
        attr(out1, "x") <- x
        attr(out1, "y") <- y
        attr(out1, "m") <- m
        return(out1)
      } else {
        return(out)
      }
  }

#' @param paths The output of [all_indirect_paths()]
#'
#' @param ... For [many_indirect_effects()],
#' these are arguments to be passed to
#' [indirect_effect()].
#'
#' @examples
#'
#' # Examples for many_indirect_effects():
#'
#' library(lavaan)
#' data(data_serial_parallel)
#' mod <-
#' "
#' m11 ~ x + c1 + c2
#' m12 ~ m11 + x + c1 + c2
#' m2 ~ x + c1 + c2
#' y ~ m12 + m2 + m11 + x + c1 + c2
#' "
#' fit <- sem(mod, data_serial_parallel,
#'            fixed.x = FALSE)
#'
#' # All indirect paths from x to y
#' paths <- all_indirect_paths(fit,
#'                            x = "x",
#'                            y = "y")
#' paths
#'
#' # Indirect effect estimates
#' out <- many_indirect_effects(paths,
#'                              fit = fit)
#' out
#'
#'
#' @export
#'
#' @describeIn cond_indirect Compute the
#' indirect effects along more than one paths.
#' It call [indirect_effect()] once for
#' each of the path.
#'
#' @order 4

many_indirect_effects <- function(paths, ...) {
    path_names <- names(paths)
    xym <- all_paths_to_df(paths)
    out <- mapply(indirect_effect,
                  x = xym$x,
                  y = xym$y,
                  m = xym$m,
                  MoreArgs = list(...),
                  SIMPLIFY = FALSE)
    names(out) <- path_names
    class(out) <- c("indirect_list", class(out))
    attr(out, "paths") <- paths
    attr(out, "call") <- match.call()
    out
  }

# Check the type of `fit` and return the type as a string
#' @noRd
#'

cond_indirect_check_fit <- function(fit) {
    fit_type <- NA
    if (inherits(fit, "lavaan")) {
        fit_type <- "lavaan"
      }
    if (inherits(fit, "list")) {
        tmp <- sapply(fit, inherits, what = "lm")
        if (isTRUE(all(tmp))) {
            fit_type <- "lm"
          } else {
            stop("'fit' is a list but not all the elements are lm outputs.")
          }
      }
    if (is.na(fit_type)) {
        stop("'fit' is neither a lavaan object or a list of lm outputs.")
      }
    fit_type
  }

# Convert a list of `indirect`-class objects to a data frame, with
# information on the levels of moderators.
#' @noRd

cond_indirect_effects_to_df <- function(x, wlevels) {
    k <- nrow(wlevels)
    wlevels_label <- attr(wlevels, "wlevels")
    colnames(wlevels_label) <- paste0("[", colnames(wlevels_label), "]")
    wlevels2 <- wlevels
    colnames(wlevels2) <- paste0("(", colnames(wlevels2), ")")
    standardized_x <- x[[1]]$standardized_x
    standardized_y <- x[[1]]$standardized_y
    if (standardized_x || standardized_y) {
        standardized_any <- TRUE
      } else {
        standardized_any <- FALSE
      }
    indirect <- data.frame(ind = sapply(x,
                              function(x) {x$indirect_raw}))
    if (standardized_x || standardized_y) {
        indirect_std <- sapply(x, function(x) x$indirect)
      } else {
        indirect_std <- NULL
      }
    cc <- do.call(rbind, sapply(x, function(x) {x$components_conditional},
                                simplify = FALSE))
    if (!is.null(x[[1]]$boot_ci)) {
        boot_ci <- TRUE
        bc <- do.call(rbind,
                      sapply(x, function(x) {x$boot_ci}, simplify = FALSE))
        if (standardized_any) {
            colnames(bc) <- paste0(c("CILo:", "CIHi:"), colnames(bc))
            colnames(bc) <- c("CI.lo", "CI.hi")
          } else {
            colnames(bc) <- paste0(c("CIStdLo:", "CIStdHi:"), colnames(bc))
            colnames(bc) <- c("CI.lo", "CI.hi")
          }
      } else {
        boot_ci <- FALSE
      }
    if (is.null(indirect_std)) {
        if (boot_ci) {
            out <- data.frame(ind = indirect, bc, cc, check.names = FALSE)
          } else {
            out <- data.frame(ind = indirect, cc, check.names = FALSE)
          }
      } else {
        if (boot_ci) {
            out <- data.frame(std = indirect_std, bc, cc, ustd = indirect, check.names = FALSE)
          } else {
            out <- data.frame(std = indirect_std, cc, ustd = indirect, check.names = FALSE)
          }
      }
    out1 <- cbind(wlevels_label, wlevels2, out)
    out1
  }

# Check the argument `wlevels` and convert it to a valid data
# frame of wlevels if possible.
#' @noRd

check_wlevels <- function(ws) {
    if (is.data.frame(ws)) {
        # A data frame. Assumed to be merged levels
        return(ws)
      }
    if (is.list(ws)) {
        tmp <- sapply(ws, function(x) !is.null(attr(x, which = "wlevels")))
        if (all(tmp)) {
            # A list of wlevels. Merge them
            out <- merge_mod_levels(ws)
            return(out)
          }
      }
    # Cannot convert to wlevels
    return(NULL)
  }