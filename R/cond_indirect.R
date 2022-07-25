#' @title Conditional, Indirect, and Conditional Indirect Effects
#'
#' @description Compute the conditional effects, indirect effects, or
#'   conditional indirect effects in a structural model fitted by
#'   [lm()] or [lavaan::lavaan()].
#'
#' @details
#'
#' For a model with a mediation pathway moderated by one or more
#' moderators, [cond_indirect()] and [cond_indirect_effects()] can be
#' used to compute the conditional indirect effect from one variable
#' to another variable, at selected level(s) of the moderator(s).
#'
#' If only the mediator(s) is/are specified (`m`) and no values of
#' moderator(s) are specified, then the indirect effect from one
#' variable (`x`) to another variable (`y`) is computed.
#'
#' If only the value(s) of moderator(s) is/are specified (`wvalues` or
#' `wlevels`) and no mediators (`m`) are specified, then the
#' conditional direct effects from one variable to another are
#' computed.
#'
#' @return
#' [cond_indirect()] returns an `indirect`-class object.
#'
#' [cond_indirect_effects()] returns a `cond_indirect_effects`-class
#' object.
#'
#' These two classes of objects their own print methods for printing
#' the results (see [print.indirect()] and
#' [print.cond_indirect_effects()])
#'
#' @param x Character. The name of predictor at the start of the
#'    pathway.
#' @param y Character. The name of the outcome variable at the end of
#'    the pathway.
#' @param m A vector of the variable names of the moderators. The
#'    pathway goes from the first mediator successively to the last
#'    mediator. If `NULL`, the default, the pathway goes from `x` to
#'    `y`.
#' @param fit The fit object. Can be a
#'    [lavaan::lavaan-class] object or a list of [lm()] outputs.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`,
#'    the default, it will be generated from `fit`. If supplied, `fit`
#'    will ge ignored.
#' @param implied_stats Implied means, variances, and covariances of
#'    observed variables, of the form of the output of
#'    [lavaan::lavInspect()] with `what` set to `"implied"`. The
#'    standard deviations are extracted from this object for
#'    standardization. Default is `NULL`, and implied statistics will
#'    be computed from `fit` if required.
#' @param wvalues A numeric vector of named elements. The names are
#'    the variable names of the moderators, and the values are the
#'    values to which the moderators will be set to. Default is
#'    `NULL`.
#' @param standardized_x Logical. Whether `x` will be standardized.
#'    Default is `FALSE`.
#' @param standardized_y Logical. Whether `y` will be standardized.
#'    Default is `FALSE`.
#' @param boot_ci Logical. Whether bootstrap confidence interval will
#'    be formed. Default is `FALSE`.
#' @param level The level of confidence for the bootstrap confidence
#'    interval. Default is .95.
#' @param boot_out If `boot_ci` is `TRUE`, users can supply
#'    pregenerated bootstrap results. This can be the output of
#'    [fit2boot_out()] or [lm2boot_out()]. If not supplied, the
#'    function will try to generate them from `fit`.
#' @param R Integer. If `boot_ci` is `TRUE`, `fit` is a list of [lm()]
#'    outputs, and `boot_out` is `NULL`, this function will do
#'    bootstrapping on `fit`. `R` is the number of bootstrap samples.
#'    Default is 100.
#' @param seed If `boot_ci` is `TRUE`, `fit` is a list of [lm()]
#'    outputs, and `boot_out` is `NULL`, this function will do
#'    bootstrapping on `fit`. This is the seed for the bootstrapping.
#'    Default is `NULL` and seed is not set.
#' @param wlevels The output of [merge_mod_levels()].
#' @param ... Arguments to be passed to [cond_indirect()]
#' @param output_type The type of output of [cond_indirect_effects()].
#'    If `"data.frame"`, the default, the output will be converted to
#'    a data frame. If any other value, the output is a list of the
#'    outputs from [cond_indirect()].
#' @param save_boot_full If `TRUE`, full bootstrapping results will be
#'    stored. Default is `FALSE.`
#' @param prods The product terms found. For internal use.
#' @param get_prods_only IF `TRUE`, will quit early and return the product
#'             terms found. The results can be passed to the `prod` argument
#'             when calling this function. Default is `FALSE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#' cond_indirect(x = "x", y = "y", m = "m2", fit = fit)
#'
#' # Conditional Indirect effect from x1 through m1 to y, when w1 is 1 SD above mean
#' cond_indirect(x = "x", y = "y", m = "m1",
#'               wvalues = c(w1 = hi_w1), fit = fit)
#'
#'
#'
#' @export
#'
#' @describeIn cond_indirect Compute conditional, indirect, or
#'                           conditional indirect effects for one set
#'                           of levels.
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
                     save_boot_full = FALSE,
                     prods = NULL,
                     get_prods_only = FALSE) {
    fit_type <- cond_indirect_check_fit(fit)
    chkpath <- check_path(x = x, y = y, m = m, fit = fit, est = est)
    if (!chkpath) {
        msg <- paste0("No path from ", sQuote(x), " to ", sQuote(y), ".",
                      "Please check the arguments x, y, and m.")
        stop(msg)
      }
    if (boot_ci) {
        if (!is.null(boot_out)) {
            if (!inherits(boot_out, "boot_out")) {
                stop("The object at 'boot_out' must be of the class 'boot_out'.")
              }
          }
        if (fit_type == "lavaan") {
            opt <- lavaan::lavInspect(fit, "options")
            if (opt$se != "bootstrap" && is.null(boot_out)) {
                stop("If 'boot_ci' is TRUE, 'se' needs to be 'bootstrap' in 'fit'.")
              }
            if (is.null(boot_out)) {
                boot_out <- fit2boot_out(fit = fit)
              }
          }
        if (fit_type == "lm") {
            if (is.null(boot_out)) {
                # Do bootstrap here.
                boot_out <- lm2boot_out(outputs = fit,
                                        R = R,
                                        seed = seed)
              }
          }
      }
    if (fit_type == "lavaan") {
        fit0 <- fit
        if (is.null(est)) est <- lavaan::parameterEstimates(fit)
        # if (is.null(implied_stats)) lavaan::lavInspect(fit, "implied")
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
      }
    out0$cond_indirect_call <- match.call()
    out0
  }

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
#' @describeIn cond_indirect Compute the conditional, indirect, or
#'                           conditional indirect effects for several
#'                           sets of levels.
#' @order 2

cond_indirect_effects <- function(wlevels,
                                  ...,
                                  fit = fit,
                                  boot_ci = FALSE,
                                  boot_out = NULL,
                                  R = 100,
                                  seed = NULL,
                                  output_type = "data.frame") {
    if (is.list(wlevels) && !is.data.frame(wlevels)) {
        wlevels <- merge_mod_levels(wlevels)
      }
    k <- nrow(wlevels)
    wlevels1 <- split(wlevels, seq_len(k))
    wlevels2 <- lapply(wlevels1, unlist)
    names(wlevels2) <- rownames(wlevels)
    fit_type <- cond_indirect_check_fit(fit)
    if (boot_ci) {
        if (!is.null(boot_out)) {
            if (!inherits(boot_out, "boot_out")) {
                stop("The object at 'boot_out' must be of the class 'boot_out'.")
              }
          }
        if (fit_type == "lavaan") {
            opt <- lavaan::lavInspect(fit, "options")
            if (opt$se != "bootstrap" && is.null(boot_out)) {
                stop("If 'boot_ci' is TRUE, 'se' needs to be 'bootstrap' in 'fit'.")
              }
            if (is.null(boot_out) && opt$se == "bootstrap") {
                boot_out <- fit2boot_out(fit = fit)
              }
          }
        if (fit_type == "lm") {
            if (is.null(boot_out)) {
                # Do bootstrap here.
                boot_out <- lm2boot_out(outputs = fit,
                                        R = R,
                                        seed = seed)
              }
          }
      }
    prods <- cond_indirect(wvalues = wlevels2[[1]],
                            ...,
                            fit = fit,
                            get_prods_only = TRUE)
    out <- lapply(wlevels2,
                  function(wv,
                           ...,
                           fit = fit,
                           boot_ci,
                           boot_out,
                           R,
                           seed) {
                              cond_indirect(wvalues = wv,
                                            ...,
                                            fit = fit,
                                            boot_ci = boot_ci,
                                            boot_out = boot_out,
                                            R = R,
                                            seed = seed,
                                            prods = prods)
                           },
                  ...,
                  fit = fit,
                  boot_ci = boot_ci,
                  boot_out = boot_out,
                  R = R,
                  seed = seed)
    if (output_type == "data.frame") {
        out1 <- cond_indirect_effects_to_df(out, wlevels = wlevels)
        class(out1) <- c("cond_indirect_effects", class(out1))
        attr(out1, "call") <- match.call()
        attr(out1, "full_output") <- out
        attr(out1, "wlevels") <- wlevels
        return(out1)
      } else {
        return(out)
      }
  }

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