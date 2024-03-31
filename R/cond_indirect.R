#' @title Conditional, Indirect, and
#' Conditional Indirect Effects
#'
#' @description Compute the conditional
#' effects, indirect effects, or
#' conditional indirect effects in a
#' structural model fitted by [lm()],
#' [lavaan::sem()], or [semTools::sem.mi()].
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
#' nonparametric bootstrapping (for
#' `lavaan` or `lm` outputs) or
#' Monte Carlo simulation (for
#' `lavaan` outputs only) to form
#' confidence intervals.
#' Bootstrapping or Monte Carlo
#' simulation only needs to be done
#' once. These are the possible ways to
#' form bootstrapping:
#'
#' 1. Do bootstrapping or Monte Carlo
#' simulation in the first call
#' to one of these functions, by setting
#' `boot_ci` or `mc_ci` to `TRUE` and
#' `R` to the
#' number of bootstrap samples or
#' replications, `level`
#' to the level of confidence (default
#' .95 or 95%), and `seed` to reproduce
#' the results (`parallel` and `ncores`
#' are optional for bootstrapping).
#' This will take some
#' time to run for bootstrapping. The
#' output will have all
#' bootstrap or Monte Carlo estimates
#' stored. This
#' output, whether it is from
#' [indirect_effect()],
#' [cond_indirect_effects()], or
#' [cond_indirect()], can be reused by
#' any of these three functions by
#' setting `boot_out` (for bootstrapping)
#' or `mc_out` (for Monte Carlo
#' simulation) to this output.
#' They will form the confidence
#' intervals using the stored bootstrap
#' or Monte Carlo
#' estimates.
#'
#' 2. Do bootstrapping using
#' [do_boot()] or Monte Carlo simulation
#' us8ing [do_mc()]. The output can be used
#' in the `boot_out` (for bootstrapping)
#' or `mc_out` (for Monte Carlo simulation)
#' argument of
#' [indirect_effect()],
#' [cond_indirect_effects()] and
#' [cond_indirect()].
#'
#' 3. For bootstrapping,
#' if [lavaan::sem()] is used to fit
#' a model and `se = "boot"` is used,
#' [do_boot()] can extract them to
#' generate a `boot_out`-class object
#' that again can be used in the
#' `boot_out` argument.
#'
#' If `boot_out` or `mc_out`
#' is set, arguments such
#' as `R`, `seed`, and `parallel` will
#' be ignored.
#'
#' ## Multigroup Models
#'
#' Since Version 0.1.14.2, support for
#' multigroup models has been added for models
#' fitted by `lavaan`. Both bootstrapping
#' and Monte Carlo confidence intervals
#' are supported. When used on
#' a multigroup model:
#'
#' - For [cond_indirect()] and
#' [indirect_effect()], users need to
#' specify the `group` argument
#' (by number or label). When using
#' [cond_indirect_effects()], if
#' `group` is not set, all groups wil
#' be used and the indirect effect
#' in each group will be computed,
#' kind of treating group as a moderator.
#'
#' - For [many_indirect_effects()],
#' the paths can be generated from a
#' multigroup models.
#'
#' - Currently, [cond_indirect_effects()]
#' does not support a multigroup model
#' with moderators on the path selected.
#' The function [cond_indirect()] does
#' not have this limitation but users
#' need to manually specify the desired
#' value of the moderator(s).
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
#' the results (see [print.indirect()] and [print.cond_indirect_effects()]).
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
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [semTools::runMI()] or
#' its wrapper, such as [semTools::sem.mi()].
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
#' [lavaan::lavaan-class] object, this function
#' will do bootstrapping on `fit`. `R`
#' is the number of bootstrap samples.
#' Default is 100. For Monte Carlo
#' simulation, this is the number
#' of replications.
#'
#' @param seed If bootstrapping
#' or Monte Carlo simulation is
#' conducted, this is the seed for the
#' bootstrapping or simulation.
#' Default is `NULL` and
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
#' @param mc_ci Logical. Whether
#' Monte Carlo confidence interval will be
#' formed. Default is `FALSE`.
#'
#' @param mc_out If `mc_ci` is
#' `TRUE`, users can supply pregenerated
#' Monte Carlo estimates. This can be the
#' output of [do_mc()]. For
#' [indirect_effect()] and
#' [cond_indirect_effects()], this can
#' be the output of a previous call to
#' [cond_indirect_effects()],
#' [indirect_effect()], or
#' [cond_indirect()] with Monte Carlo
#' confidence intervals requested. These
#' stored estimates will be reused such
#' that there is no need to do
#' Monte Carlo simulation again. If not
#' supplied,
#' the function will try to generate
#' them from `fit`.
#'
#' @param save_mc_full If `TRUE`, full
#' Monte Carlo results will be stored.
#' Default is `FALSE.`
#'
#' @param save_mc_out If `mc_out` is
#' supplied, whether it will be saved in
#' the output. Default is `TRUE`.
#'
#' @param save_ci_full If `TRUE`, full
#' bootstrapping or Monte Carlo results
#' will be stored.
#' Default is `FALSE.`
#'
#' @param save_ci_out If either `mc_out`
#' or `boot_out` is
#' supplied, whether it will be saved in
#' the output. Default is `TRUE`.
#'
#' @param ci_out If `ci_type` is supplied,
#' this is the corresponding argument.
#' If `ci_type` is `"boot"`, this
#' argument will be used as `boot_out`.
#' If `ci_type` is `"mc"`, this
#' argument will be used as `mc_out`.
#'
#' @param ci_type The type of
#' confidence intervals to be formed.
#' Can be either `"boot"` (bootstrapping)
#' or `"mc"` (Monte Carlo). If not
#' supplied or is `NULL`, will check
#' other arguments
#' (e.g, `boot_ci` and `mc_ci`). If
#' supplied, will override `boot_ci`
#' and `mc_ci`.
#'
#' @param group Either the group number
#' as appeared in the [summary()]
#' or [lavaan::parameterEstimates()]
#' output of a [lavaan::lavaan-class] object,
#' or the group label as used in
#' the [lavaan::lavaan-class] object.
#' Used only when the number of
#' groups is greater than one. Default
#' is `NULL`.
#'
#' @param groups Either a vector of
#' group numbers
#' as appeared in the [summary()]
#' or [lavaan::parameterEstimates()]
#' output of a [lavaan::lavaan-class] object,
#' or a vector of group labels as used in
#' the [lavaan::lavaan-class] object.
#' Used only when the number of
#' groups is greater than one. Default
#' is `NULL`.
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
                     save_boot_out = TRUE,
                     mc_ci = FALSE,
                     mc_out = NULL,
                     save_mc_full = FALSE,
                     save_mc_out = TRUE,
                     ci_out = NULL,
                     save_ci_full = FALSE,
                     save_ci_out = TRUE,
                     ci_type = NULL,
                     group = NULL) {
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

    # Fix arguments
    call_args <- names(match.call())
    if (!is.null(ci_type)) {
        if (ci_type == "mc") {
            mc_ci <- TRUE
            boot_ci <- FALSE
            if (is.null(mc_out) && !is.null(ci_out)) {
                mc_out <- ci_out
                ci_out <- NULL
              }
            if (is.na(match("save_mc_full", call_args))) {
                save_mc_full <- save_ci_full
              }
            if (is.na(match("save_mc_out", call_args))) {
                save_mc_out <- save_ci_out
              }
          }
        if (ci_type == "boot") {
            boot_ci <- TRUE
            mc_ci <- FALSE
            if (is.null(boot_out) && !is.null(ci_out)) {
                boot_out <- ci_out
                ci_out <- NULL
              }
            if (is.na(match("save_boot_full", call_args))) {
                save_boot_full <- save_ci_full
              }
            if (is.na(match("save_boot_out", call_args))) {
                save_boot_out <- save_ci_out
              }
          }
      }
    if (all(boot_ci, mc_ci)) stop("Can only request one type of confidence intervals.")
    if (mc_ci) {
        if (!is.null(mc_out)) {
            if (inherits(mc_out, "cond_indirect_effects")) {
                mc_out <- attr(mc_out, "mc_out")
                if (is.null(mc_out)) {
                    stop("mc_out not found in the supplied object for 'mc_out'")
                  }
              }
            if (inherits(mc_out, "indirect")) {
                mc_out <- mc_out$mc_out
                if (is.null(mc_out)) {
                    stop("mc_out not found in the supplied object for 'mc_out'")
                  }
              }
            if (!inherits(mc_out, "mc_out")) {
                stop("The object at 'mc_out' must be of the class 'mc_out'.")
              }
          } else {
            mc_out <- do_mc(fit = fit,
                            R = R,
                            seed = seed,
                            progress = progress)
          }
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
    if (fit_type == "lavaan.mi") {
        fit0 <- fit
        if (is.null(est)) est <- lav_est(fit)
        if (is.null(implied_stats)) implied_stats <- lav_implied_all(fit)
        fit_data <- lav_data_used(fit, drop_colon = FALSE)
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
                        expand = TRUE,
                        group = group)
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
                     prods = prods,
                     group = group)
    if (mc_ci) {
        out_mc <- mapply(indirect_i,
                           est = lapply(mc_out, function(x) x$est),
                           implied_stats = lapply(mc_out, function(x) x$implied_stats),
                           MoreArgs = list(x = x,
                                           y = y,
                                           m = m,
                                           fit = fit0,
                                           wvalues = wvalues,
                                           standardized_x = standardized_x,
                                           standardized_y = standardized_y,
                                           warn = FALSE,
                                           prods = prods,
                                           group = group),
                           SIMPLIFY = FALSE)
        if (save_mc_full) {
            out0$mc_full <- out_mc
          }
        nboot <- length(out_mc)
        out0$mc_indirect <- sapply(out_mc, function(x) x$indirect)
        out0$mc_scale_x <- unname(sapply(out_mc, function(x) x$scale_x))
        out0$mc_scale_y <- unname(sapply(out_mc, function(x) x$scale_y))
        tmp <- list(t = matrix(out0$mc_indirect, nrow = nboot, ncol = 1),
                    t0 = out0$indirect,
                    R = nboot)
        boot_ci0 <- boot::boot.ci(tmp, conf = level, type = "perc")
        boot_ci1 <- boot_ci0$percent[4:5]
        names(boot_ci1) <- paste0(formatC(c(100 * (1 - level) / 2,
                                     100 * (1 - (1 - level) / 2)), 2,
                                     format = "f"), "%")
        out0$mc_ci <- boot_ci1
        out0$level <- level
        out0$mc_se <- stats::sd(out0$mc_indirect, na.rm = TRUE)
        if (save_mc_out) {
            out0$mc_out <- mc_out
          } else {
            out0$mc_out <- NULL
          }
      }
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
                                           prods = prods,
                                           group = group),
                           SIMPLIFY = FALSE)
        if (save_boot_full) {
            out0$boot_full <- out_boot
          }
        nboot <- length(out_boot)
        out0$boot_indirect <- sapply(out_boot, function(x) x$indirect)
        out0$boot_scale_x <- unname(sapply(out_boot, function(x) x$scale_x))
        out0$boot_scale_y <- unname(sapply(out_boot, function(x) x$scale_y))
        tmp <- list(t = matrix(out0$boot_indirect, nrow = nboot, ncol = 1),
                    t0 = out0$indirect,
                    R = nboot)
        boot_ci0 <- boot::boot.ci(tmp, conf = level, type = "perc")
        boot_ci1 <- boot_ci0$percent[4:5]
        names(boot_ci1) <- paste0(formatC(c(100 * (1 - level) / 2,
                                     100 * (1 - (1 - level) / 2)), 2,
                                     format = "f"), "%")
        out0$boot_ci <- boot_ci1
        out0$level <- level
        out0$boot_p <- est2p(out0$boot_indirect)
        out0$boot_se <- stats::sd(out0$boot_indirect, na.rm = TRUE)
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
#' @examples
#'
#' # Multigroup model with indirect effects
#'
#' dat <- data_med_mg
#' mod <-
#' "
#' m ~ x + c1 + c2
#' y ~ m + x + c1 + c2
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE,
#'            group = "group")
#'
#' # If a model has more than one group,
#' # the argument 'group' must be set.
#' ind1 <- indirect_effect(x = "x",
#'                         y = "y",
#'                         m = "m",
#'                         fit = fit,
#'                         group = "Group A")
#' ind1
#' ind2 <- indirect_effect(x = "x",
#'                         y = "y",
#'                         m = "m",
#'                         fit = fit,
#'                         group = 2)
#' ind2
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
                     save_boot_full = FALSE,
                     save_boot_out = TRUE,
                     mc_ci = FALSE,
                     mc_out = NULL,
                     save_mc_full = FALSE,
                     save_mc_out = TRUE,
                     ci_out = NULL,
                     save_ci_full = FALSE,
                     save_ci_out = TRUE,
                     ci_type = NULL,
                     group = NULL) {
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
                  save_boot_full = save_boot_full,
                  save_boot_out = save_boot_out,
                  mc_ci = mc_ci,
                  mc_out = mc_out,
                  save_mc_full = save_mc_full,
                  save_mc_out = save_mc_out,
                  ci_out = ci_out,
                  save_ci_full = save_ci_full,
                  save_ci_out = save_ci_out,
                  ci_type = ci_type,
                  group = group)
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
                                  mc_ci = FALSE,
                                  mc_out = NULL,
                                  ci_out = NULL,
                                  ci_type = NULL,
                                  groups = NULL,
                                  ...) {
    # Check the number of groups and handle multiple-group models
    has_group <- FALSE
    ngroups <- 1
    group_numbers <- NULL
    group_labels <- NULL
    if (inherits(fit, "lavaan")) {
        ngroups <- lavaan::lavTech(fit, "ngroups")
        if (ngroups > 1) {
            has_group <- TRUE
            tmp <- group_labels_and_numbers(groups = groups,
                                            fit = fit)
            group_numbers <- tmp$number
            group_labels <- tmp$label
          } else {
            if (!is.null(groups)) {
                stop("The model has only one group but 'groups' is set.")
              }
          }
      }
    # Check and process the levels of moderators
    has_wlevels <- FALSE
    if (!missing(wlevels)) {
        has_wlevels <- TRUE
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
        wlevels <- NULL
        if (!has_group) {
            stop("wlevels is required for single-group models.")
          }
      }
    if (has_group && has_wlevels) {
        stop("Multiple group models with moderators not yet supported.",
             "Will be supported soon.")
      }
    if (has_wlevels) {
        k <- nrow(wlevels)
        wlevels1 <- split(wlevels, seq_len(k))
        wlevels2 <- lapply(wlevels1, unlist)
        names(wlevels2) <- rownames(wlevels)
      }
    fit_type <- cond_indirect_check_fit(fit)
    if ((fit_type == "lm") && !inherits(fit, "lm_list") &&
        is.list(fit)) {
        fit <- lm2list(fit)
      }

    # Fix arguments
    call_args <- names(match.call())
    if (!is.null(ci_type)) {
        if (ci_type == "mc") {
            mc_ci <- TRUE
            boot_ci <- FALSE
            if (is.null(mc_out) && !is.null(ci_out)) {
                mc_out <- ci_out
                ci_out <- NULL
              }
          }
        if (ci_type == "boot") {
            boot_ci <- TRUE
            mc_ci <- FALSE
            if (is.null(boot_out) && !is.null(ci_out)) {
                boot_out <- ci_out
                ci_out <- NULL
              }
          }
      }

    if (all(boot_ci, mc_ci)) stop("Can only request one type of confidence intervals.")
    if (mc_ci) {
        if (!is.null(mc_out)) {
            if (inherits(mc_out, "cond_indirect_effects")) {
                mc_out <- attr(mc_out, "mc_out")
                if (is.null(mc_out)) {
                    stop("mc_out not found in the supplied object for 'mc_out'")
                  }
              }
            if (inherits(mc_out, "indirect")) {
                mc_out <- mc_out$mc_out
                if (is.null(mc_out)) {
                    stop("mc_out not found in the supplied object for 'mc_out'")
                  }
              }
            if (!inherits(mc_out, "mc_out")) {
                stop("The object at 'mc_out' must be of the class 'mc_out'.")
              }
          } else {
            mc_out <- do_mc(fit = fit,
                            R = R,
                            seed = seed)
          }
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
    # TODO:
    # Revise cond_indirect and friends such that
    # no need to have three very similar blocks.
    if (has_wlevels && !has_group) {
        prods <- cond_indirect(wvalues = wlevels2[[1]],
                                x = x,
                                y = y,
                                m = m,
                                fit = fit,
                                est = est,
                                implied_stats = implied_stats,
                                get_prods_only = TRUE,
                                ...)
      }
    if (!has_wlevels && has_group) {
        prods <- cond_indirect(x = x,
                               y = y,
                               m = m,
                               fit = fit,
                               est = est,
                               implied_stats = implied_stats,
                               get_prods_only = TRUE,
                               group = 1,
                               ...)
      }
    if (has_wlevels && has_group) {
        prods <- cond_indirect(wvalues = wlevels2[[1]],
                               x = x,
                               y = y,
                               m = m,
                               fit = fit,
                               est = est,
                               implied_stats = implied_stats,
                               get_prods_only = TRUE,
                               group = 1,
                               ...)
      }
    # TODO:
    # Revise cond_indirect and friends such that
    # no need to have three very similar blocks.
    if (has_wlevels && !has_group) {
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
                              mc_ci,
                              mc_out,
                              save_mc_out,
                              ci_type,
                              ci_out,
                              save_ci_out,
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
                                                mc_ci = mc_ci,
                                                mc_out = mc_out,
                                                save_mc_out = FALSE,
                                                ci_type = ci_type,
                                                ci_out = ci_out,
                                                save_ci_out = FALSE,
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
                      mc_ci = mc_ci,
                      mc_out = mc_out,
                      save_mc_out = FALSE,
                      ci_type = ci_type,
                      ci_out = ci_out,
                      save_ci_out = FALSE,
                      ...)
      }
    if (!has_wlevels && has_group) {
        out <- lapply(group_numbers,
                      function(gn,
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
                              mc_ci,
                              mc_out,
                              save_mc_out,
                              ci_type,
                              ci_out,
                              save_ci_out,
                              ...) {
                                  indirect_effect(x = x,
                                                  y = y,
                                                  m = m,
                                                  fit = fit,
                                                  est = est,
                                                  implied_stats = implied_stats,
                                                  boot_ci = boot_ci,
                                                  boot_out = boot_out,
                                                  R = R,
                                                  seed = seed,
                                                  save_boot_out = FALSE,
                                                  mc_ci = mc_ci,
                                                  mc_out = mc_out,
                                                  save_mc_out = FALSE,
                                                  ci_type = ci_type,
                                                  ci_out = ci_out,
                                                  save_ci_out = FALSE,
                                                  group = gn,
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
                      save_boot_out = FALSE,
                      mc_ci = mc_ci,
                      mc_out = mc_out,
                      save_mc_out = FALSE,
                      ci_type = ci_type,
                      ci_out = ci_out,
                      save_ci_out = FALSE,
                      ...)
      }
    if (has_wlevels && has_group) {
        # TODO
        # - Not yet supported.
        # - Need to use expand.grid to create
        #   all combinations of group and wlevels
        group_numbers_long <- NULL
        wlevels2_long <- NULL
        out <- mapply(function(gn,
                              wv,
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
                              mc_ci,
                              mc_out,
                              save_mc_out,
                              ci_type,
                              ci_out,
                              save_ci_out,
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
                                                mc_ci = mc_ci,
                                                mc_out = mc_out,
                                                save_mc_out = FALSE,
                                                ci_type = ci_type,
                                                ci_out = ci_out,
                                                save_ci_out = FALSE,
                                                group = gn,
                                                ...)
                              },
                      gn = group_numbers_long,
                      wv = wlevels2_long,
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
                      mc_ci = mc_ci,
                      mc_out = mc_out,
                      save_mc_out = FALSE,
                      ci_type = ci_type,
                      ci_out = ci_out,
                      save_ci_out = FALSE,
                      ...)
      }
    if (output_type == "data.frame") {
        out1 <- cond_indirect_effects_to_df(out,
                                            wlevels = wlevels,
                                            group_numbers = group_numbers,
                                            group_labels = group_labels)
        class(out1) <- c("cond_indirect_effects", class(out1))
        attr(out1, "call") <- match.call()
        attr(out1, "full_output") <- out
        attr(out1, "wlevels") <- wlevels
        attr(out1, "fit") <- fit
        attr(out1, "est") <- est
        attr(out1, "implied_stats") <- implied_stats
        if (boot_ci) attr(out1, "boot_out") <- boot_out
        if (mc_ci) attr(out1, "ci_out") <- ci_out
        attr(out1, "prods") <- prods
        attr(out1, "x") <- x
        attr(out1, "y") <- y
        attr(out1, "m") <- m
        # TODO:
        # - Store the expanded combination of group and wlevels
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
#' # All indirect paths from x to y
#' paths <- all_indirect_paths(fit,
#'                            x = "x",
#'                            y = "y")
#' paths
#' # Indirect effect estimates
#' out <- many_indirect_effects(paths,
#'                              fit = fit)
#' out
#'
#' # Multigroup models
#'
#' data(data_med_complicated_mg)
#' mod <-
#' "
#' m11 ~ x1 + x2 + c1 + c2
#' m12 ~ m11 + c1 + c2
#' m2 ~ x1 + x2 + c1 + c2
#' y1 ~ m11 + m12 + x1 + x2 + c1 + c2
#' y2 ~ m2 + x1 + x2 + c1 + c2
#' "
#' fit <- sem(mod, data_med_complicated_mg, group = "group")
#' summary(fit)
#'
#' paths <- all_indirect_paths(fit,
#'                             x = "x1",
#'                             y = "y1")
#' paths
#' # Indirect effect estimates for all paths in all groups
#' out <- many_indirect_effects(paths,
#'                              fit = fit)
#' out
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
    if ("group_label" %in% colnames(xym)) {
        out <- mapply(indirect_effect,
                      x = xym$x,
                      y = xym$y,
                      m = xym$m,
                      group = xym$group_number,
                      MoreArgs = list(...),
                      SIMPLIFY = FALSE)
      } else {
        out <- mapply(indirect_effect,
                      x = xym$x,
                      y = xym$y,
                      m = xym$m,
                      MoreArgs = list(...),
                      SIMPLIFY = FALSE)
      }
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
    if (inherits(fit, "lavaan.mi")) {
        fit_type <- "lavaan.mi"
      }
    if (is.na(fit_type)) {
        stop("'fit' is neither a lavaan object or a list of lm outputs.")
      }
    fit_type
  }

# Convert a list of `indirect`-class objects to a data frame, with
# information on the levels of moderators.
#' @noRd

cond_indirect_effects_to_df <- function(x,
                                        wlevels = NULL,
                                        group_numbers = NULL,
                                        group_labels = NULL) {
    has_wlevels <- !is.null(wlevels)
    has_group <- !is.null(group_numbers) || !is.null(group_labels)
    if (has_wlevels) {
        # TOFIX
        wlevels_label <- attr(wlevels, "wlevels")
        colnames(wlevels_label) <- paste0("[", colnames(wlevels_label), "]")
        wlevels2 <- wlevels
        colnames(wlevels2) <- paste0("(", colnames(wlevels2), ")")
      }
    if (has_group) {
        group_numbers <- sapply(x, function(x) x$group_number)
        group_labels <- sapply(x, function(x) x$group_label)
        gp_df <- data.frame(Group = group_labels,
                            Group_ID = group_numbers)
      }
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
    has_ci <- FALSE
    if (!is.null(x[[1]]$boot_ci)) {
        has_ci <- TRUE
        ci_type <- "boot"
        ci_cname <- "boot_ci"
      }
    if (!is.null(x[[1]]$mc_ci)) {
        has_ci <- TRUE
        ci_type <- "mc"
        ci_cname <- "mc_ci"
      }
    if (has_ci) {
        bc <- do.call(rbind,
                      sapply(x, function(x) {x[[ci_cname]]}, simplify = FALSE))
        if (standardized_any) {
            colnames(bc) <- paste0(c("CILo:", "CIHi:"), colnames(bc))
            colnames(bc) <- c("CI.lo", "CI.hi")
          } else {
            colnames(bc) <- paste0(c("CIStdLo:", "CIStdHi:"), colnames(bc))
            colnames(bc) <- c("CI.lo", "CI.hi")
          }
      }
    if (is.null(indirect_std)) {
        if (has_ci) {
            out <- data.frame(ind = indirect, bc, cc, check.names = FALSE)
          } else {
            out <- data.frame(ind = indirect, cc, check.names = FALSE)
          }
      } else {
        if (has_ci) {
            out <- data.frame(std = indirect_std, bc, cc, ustd = indirect, check.names = FALSE)
          } else {
            out <- data.frame(std = indirect_std, cc, ustd = indirect, check.names = FALSE)
          }
      }
    if (has_wlevels) {
        out <- cbind(wlevels_label, wlevels2, out)
      }
    if (has_group) {
        out <- cbind(gp_df, out)
      }
    out
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