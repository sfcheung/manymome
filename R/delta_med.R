#' @title Delta_Med by Liu, Yuan, and Li (2023)
#'
#' @description It computes the
#' Delta_Med proposed by Liu, Yuan,
#' and Li (2023), an \eqn{R^2}-like measure of
#' indirect effect.
#'
#' @details
#' It computes Delta_Med, an
#' \eqn{R^2}-like effect
#' size measure for the indirect effect
#' from one variable (the `y`-variable)
#' to another variable (the `x`-variable)
#' through one or more mediators
#' (`m`, or `m1`, `m2`, etc. when
#' there are more than one mediator).
#'
#' The Delta_Med of one or more
#' mediators was computed as the
#' difference between
#' two \eqn{R^2}s:
#'
#'  - \eqn{R^2_1}, the \eqn{R^2} when `y`
#'    is predicted by `x` and all
#'    mediators.
#'
#'  - \eqn{R^2_2}, the \eqn{R^2} when the
#'    mediator(s) of
#'    interest is/are removed from the
#'    models, while the error term(s)
#'    of the mediator(s) is/are kept.
#'
#' Delta_Med is given by
#' \eqn{R^2_1 - R^2_2}.
#'
#' Please refer to Liu et al. (2023)
#' for the technical details.
#'
#' The function can also form a
#' nonparametric percentile bootstrap
#' confidence of Delta_Med.
#'
#' # Implementation
#'
#' The function identifies all the
#' path(s) pointing to the mediator(s)
#' of concern and fixes the path(s) to
#' zero, effectively removing the
#' mediator(s). However, the model is
#' not refitted, hence keeping the
#' estimates of all other parameters
#' unchanged.
#' It then uses [lavaan::lav_model_set_parameters()]
#' to update the parameters,
#' [lavaan::lav_model_implied()] to
#' update the implied statistics, and
#' then calls [lavaan::lavInspect()] to
#' retrieve the implied variance of the
#' predicted values of `y` for computing
#' the \eqn{R^2_2}. Subtracting this
#' \eqn{R^2_2} from \eqn{R^2_1} of
#' `y` can then yield Delta_Med.
#'
#' # Model Requirements
#'
#' For now, by default, it only
#' computes Delta_Med
#' for the types
#' of models discussed in
#' Liu et al. (2023):
#'
#' - Having one predictor (the
#'  `x`-variable).
#'
#' - Having one or more mediators, the
#'  `m`-variables, with
#'  arbitrary way to mediate the effect
#'  of `x` on the outcome variable
#'  (`y`-variable).
#'
#' - Having one or more outcome variables.
#'  Although their models only have
#'  outcome variables, the computation
#'  of the Delta_Med is not affected
#'  by the presence of other outcome
#'  variables.
#'
#' - Having no control variables.
#'
#' - The mediator(s), `m`, and the
#'  `y`-variable are continuous.
#'
#' - `x` can be continuous
#'  or categorical. If categorical, it
#'  needs to be handle appropriately
#'  when fitting the model.
#'
#' - `x` has a direct
#'  path to `y`.
#'
#' - All the mediators listed in the
#'  argument `m` is present in at least
#'  one path from `x` to `y.`
#'
#' - None of the paths from `x` to `y`
#'  are moderated.
#'
#' It can be used for other kinds
#' of models but support for them is
#' disabled by default. To use
#' this function for
#' cases not discussed in
#' Liu et al. (2023), please disable
#' relevant requirements stated above
#' using the relevant
#' `skip_check_*` arguments. An error
#' will be raised if the models failed
#' any of the checks not skipped by
#' users.
#'
#' @return
#' A `delta_med` class object.
#' It is a list-like object with these
#' major elements:
#'
#' - `delta_med`: The Delta_Med.
#'
#' - `x`: The name of the `x`-variable.
#'
#' - `y`: The name of the `y`-variable.
#'
#' - `m`: A character vector of the
#' mediator(s) along a path. The path
#' runs from the first element to the
#' last element.
#'
#' This class has a `print` method,
#' a `coef` method, and a `confint`
#' method. See [print.delta_med()],
#' [coef.delta_med()], and
#' [confint.delta_med()].
#'
#' @param x The name of the `x` variable.
#' Must be supplied as a quoted string.
#'
#' @param y The name of the `y` variable.
#' Must be supplied as a quoted string.
#'
#' @param m A vector of the variable
#' names of the mediator(s). If more
#' than one mediators, they do not have
#' to be on the same path from `x`
#' to `y`.
#' Cannot be `NULL` for this function.
#'
#' @param fit The fit object. Must be a
#' [lavaan::lavaan-class] object.
#'
#' @param paths_to_remove A character
#' vector of paths users want to
#' manually remove, specified in
#' `lavaan` model syntax. For example,
#' `c("m2~x", "m3~m2")` removes the
#' path from `x` to `m2` and the path
#' from `m2` to `m3`. The default is
#' `NULL`, and the paths to remove will
#' be determined using the method
#' by Liu et al. (2023). If supplied,
#' then only paths specified explicitly
#' will be removed.
#'
#' @param boot_out The
#' output of [do_boot()]. If supplied,
#' the stored bootstrap estimates will
#' be used to form the nonparametric
#' percentile bootstrap confidence
#' interval of Delta_Med.
#'
#' @param level The level of confidence
#' of the bootstrap confidence interval.
#' Default is .95.
#'
#' @param progress Logical. Display
#' bootstrapping progress or not.
#' Default is `TRUE`.
#'
#' @param skip_check_single_x Logical
#' Check whether the model has one and
#' only one x-variable. Default is `TRUE`.
#'
#' @param skip_check_m_between_x_y
#' Logical. Check whether all `m`
#' variables are along a path from `x`
#' to `y`. Default is `TRUE`.
#'
#' @param skip_check_x_to_y Logical.
#' Check whether there is a direct path
#' from `x` to `y`. Default is `TRUE`.
#'
#' @param skip_check_latent_variables
#' Logical. Check whether the model
#' has any latent variables. Default
#' is `TRUE`.
#'
#' @param boot_type If bootstrap
#' confidence interval is to be formed,
#' the type of bootstrap confidence
#' interval. The supported types
#' are `"perc"` (percentile bootstrap
#' confidence interval, the default and
#' recommended type) and `"bc"`
#' (bias-corrected, or BC, bootstrap
#' confidence interval).
#'
#' @references
#' Liu, H., Yuan, K.-H., & Li, H. (2023).
#' A systematic framework for defining
#' R-squared measures in mediation
#' analysis. *Psychological Methods*.
#' Advance online publication.
#' https://doi.org/10.1037/met0000571
#'
#' @seealso [print.delta_med()],
#' [coef.delta_med()], and
#' [confint.delta_med()].
#'
#' @examples
#'
#' library(lavaan)
#' dat <- data_med
#' mod <-
#' "
#' m ~ x
#' y ~ m + x
#' "
#' fit <- sem(mod, dat)
#' dm <- delta_med(x = "x",
#'                 y = "y",
#'                 m = "m",
#'                 fit = fit)
#' dm
#' print(dm, full = TRUE)
#'
#' # Call do_boot() to generate
#' # bootstrap estimates
#' # Use 2000 or even 5000 for R in real studies
#' # Set parallel to TRUE in real studies for faster bootstrapping
#' boot_out <- do_boot(fit,
#'                     R = 45,
#'                     seed = 879,
#'                     parallel = FALSE,
#'                     progress = FALSE)
#' # Remove 'progress = FALSE' in practice
#' dm_boot <- delta_med(x = "x",
#'                      y = "y",
#'                      m = "m",
#'                      fit = fit,
#'                      boot_out = boot_out,
#'                      progress = FALSE)
#' dm_boot
#' confint(dm_boot)
#'
#'
#' @export

delta_med <- function(x,
                      y,
                      m,
                      fit,
                      paths_to_remove = NULL,
                      boot_out = NULL,
                      level = .95,
                      progress = TRUE,
                      skip_check_single_x = FALSE,
                      skip_check_m_between_x_y = FALSE,
                      skip_check_x_to_y = FALSE,
                      skip_check_latent_variables = FALSE,
                      boot_type = c("perc", "bc")
                      ) {
    boot_type <- match.arg(boot_type)
    if (missing(x) || missing(m) || missing(y)) {
        stop("x, m, and y must all be specified.")
      }
    if (!delta_med_check_fit(x = x,
                             y = y,
                             m = m,
                             fit = fit)) {
        stop("Something's wrong with the model and/or",
             "the x, y, and m variables selected. Please check.")
      }
    out <- delta_med_i(fit = fit,
                       x = x,
                       m = m,
                       y = y,
                       paths_to_remove = paths_to_remove)
    out$x <- x
    out$m <- m
    out$y <- y
    out$paths_removed <- out$paths_removed
    dm <- out$delta_med
    if (!is.null(boot_out)) {
        est <- lapply(boot_out, `[[`, "est")
        if (progress &&
            requireNamespace("pbapply", quietly = TRUE)) {
            out_boot <- pbapply::pbsapply(est,
                                          delta_med_i,
                                          fit = fit,
                                          x = x,
                                          m = m,
                                          y = y,
                                          paths_to_remove = paths_to_remove,
                                          simplify = FALSE)
          } else {
            out_boot <- sapply(est,
                              delta_med_i,
                              fit = fit,
                              x = x,
                              m = m,
                              y = y,
                              paths_to_remove = paths_to_remove,
                              simplify = FALSE)
          }
        R <- length(boot_out)
        dm_boot <- sapply(out_boot, `[[`, "delta_med")
        # TOCHEDCK (BC): Add support for BC bootstrap CI [DONE]
        dm_boot_out <- form_boot_ci(est = dm,
                                    boot_est = dm_boot,
                                    level = level,
                                    boot_type = boot_type)
        dm_boot_ci <- dm_boot_out$boot_ci
        dm_boot_p <- dm_boot_out$boot_p
        dm_boot_se <- dm_boot_out$boot_se
        boot_rsq_full <- sapply(out_boot, `[[`, "rsq_full")
        boot_rsq_no_mediators <- sapply(out_boot, `[[`, "rsq_no_mediators")
        boot_var_y <- sapply(out_boot, `[[`, "var_y")
        boot_var_predicted_full <- sapply(out_boot, `[[`, "var_predicted_full")
        boot_var_predicted_no_mediators <- sapply(out_boot, `[[`, "var_predicted_no_mediators")
      } else {
        dm_boot <- NULL
        R <- NULL
        dm_boot_ci <- NULL
        dm_boot_p <- NULL
        dm_boot_se <- NULL
        boot_rsq_full <- NULL
        boot_rsq_no_mediators <- NULL
        boot_var_y <- NULL
        boot_var_predicted_full <- NULL
        boot_var_predicted_no_mediators <- NULL
      }
    out$boot_est <- dm_boot
    out$R <- R
    out$boot_ci <- dm_boot_ci
    out$boot_p <- dm_boot_p
    out$boot_se <- dm_boot_se
    out$boot_rsq_full <- boot_rsq_full
    out$boot_rsq_no_mediators <- boot_rsq_no_mediators
    out$boot_var_y <- boot_var_y
    out$boot_var_predicted_full <- boot_var_predicted_full
    out$boot_var_predicted_no_mediators <- boot_var_predicted_no_mediators
    out$level <- level
    out$boot_type <- boot_type
    out$call <- match.call()
    class(out) <- "delta_med"
    out
  }

#' @title The Internal Function for Delta_Med
#' @noRd
delta_med_i <- function(fit,
                        x = "x",
                        m = "m",
                        y = "y",
                        est = NULL,
                        paths_to_remove = NULL,
                        delta_med_only = FALSE) {
    fit1 <- fit
    if (!is.null(est)) {
        coef0 <- est_2_coef(fit = fit,
                            est = est)
      } else {
        coef0 <- methods::getMethod("coef",
                      signature = "lavaan",
                      where = asNamespace("lavaan"))(fit)
      }
    coef1 <- coef0
    ptable0 <- lavaan::parameterTable(fit)
    # Identify all free paths pointing toward the mediator(s)
    # Then fix those paths to zeros
    if (!is.null(paths_to_remove)) {
        p2drop <- lavaan::lavaanify(paths_to_remove)
        p2drop <- p2drop[p2drop$op == "~", c("lhs", "op", "rhs")]
        p_tmp <- merge(p2drop, ptable0,
                       all.x = TRUE,
                       all.y = FALSE)
        id <- p_tmp$free
        id <- id[id > 0]
      } else {
        id <- ptable0[(ptable0$lhs %in% m) &
                      (ptable0$op == "~"),
                      "free"]
        id <- id[id > 0]
      }

    fit0 <- fit
    fit0@Model <- lavaan::lav_model_set_parameters(fit@Model, coef0)
    fit0@implied <- lavaan::lav_model_implied(fit0@Model)

    coef1[id] <- 0
    fit1@Model <- lavaan::lav_model_set_parameters(fit@Model, coef1)
    fit1@implied <- lavaan::lav_model_implied(fit1@Model)

    est0 <- lavaan::parameterEstimates(fit0, rsquare = TRUE)
    implied_cov0 <- lavaan::lavInspect(fit0, "implied")$cov

    est1 <- lavaan::parameterEstimates(fit1, rsquare = TRUE)
    implied_cov1 <- lavaan::lavInspect(fit1, "implied")$cov

    var_y0 <- diag(implied_cov0)[y]

    var_y1 <- diag(implied_cov1)[y]

    rsq_y0 <- est0[(est0$lhs == y) &
                   (est0$op == "r2"),
                   "est"]

    rsq_y1 <- est1[(est1$lhs == y) &
                   (est1$op == "r2"),
                   "est"]

    expvar_y1 <- rsq_y1 * var_y1
    rsq_y1_new <- expvar_y1 / var_y0
    dm <- rsq_y0 - rsq_y1_new
    dm <- unname(dm)
    if (delta_med_only) {
      return(dm)
    } else {
      ptable_tmp <- ptable0
      ptable_tmp$label <- ""
      paths_removed <- lavaan::lav_partable_labels(ptable_tmp)[id]
      out <- list(delta_med = dm,
                  paths_removed = paths_removed,
                  rsq_full = unname(rsq_y0),
                  rsq_no_mediators = unname(rsq_y1_new),
                  var_y = unname(var_y0),
                  var_predicted_full = unname(rsq_y0 * var_y0),
                  var_predicted_no_mediators = unname(expvar_y1))
      return(out)
    }
  }

#' @title Get Coefs from Estimates
#' @noRd

est_2_coef <- function(est,
                       fit) {
    ptable <- lavaan::parameterTable(fit)
    ptable$order <- seq_len(nrow(ptable))
    ptable <- merge(ptable, est,
                    by = c("lhs", "op", "rhs"),
                    suffixes = c("", ".est"),
                    all.x = TRUE,
                    all.y = FALSE,
                    sort = FALSE)
    coef0 <- methods::getMethod("coef",
                  signature = "lavaan",
                  where = asNamespace("lavaan"))(fit)
    coef0[] <- ptable[, "est.est"][ptable$free > 0]
    coef0
  }

#' @title Form Percentile CI
# Can be used by other functions.
#' @noRd

form_boot_ci <- function(est,
                         boot_est,
                         level = .95,
                         boot_type = c("perc", "bc"),
                         internal_options = list()) {
    boot_type <- match.arg(boot_type)
    out <- list()
    out$est <- est
    out$boot_est <- boot_est
    if (isTRUE(internal_options$skip_ci)) {
      boot_ci1 <- as.numeric(c(NA, NA))
    } else {
      boot_ci1 <- boot_ci_internal(t0 = est,
                          t = boot_est,
                          level = level,
                          boot_type = boot_type)
    }
    out$boot_ci <- boot_ci1
    out$level <- level
    # Do not use %||% for now. Too new.
    if (is.null(internal_options$pvalue_min_size)) {
      tmp <- formals(est2p)$min_size
    } else {
      tmp <- internal_options$pvalue_min_size
    }
    out$boot_p <- est2p(out$boot_est,
                        min_size = tmp)
    out$boot_se <- stats::sd(out$boot_est, na.rm = TRUE)
    out
  }

#' @noRd

delta_med_check_fit <- function(x,
                                y,
                                m,
                                fit,
                                skip_check_single_x = FALSE,
                                skip_check_m_between_x_y = FALSE,
                                skip_check_x_to_y = FALSE,
                                skip_check_latent_variables = FALSE
                                ) {
    if (!skip_check_single_x) {
        if (length(lavaan::lavNames(fit, "ov.x")) != 1) {
            stop("The model does not have exactly one x-variable.")
          }
      }
    if (!skip_check_m_between_x_y) {
        tmp <- sapply(m,
                      check_path,
                      x = x,
                      y = y,
                      fit = fit)
        if (!all(tmp)) {
            stop("At least one m-variable not between x and y.")
          }
      }
    if (!skip_check_x_to_y) {
        if (!check_path(x = x,
                       y = y,
                       fit = fit)) {
            stop("x does not have a direct path to y.")
          }
      }
    if (!skip_check_latent_variables) {
        if (length(lavaan::lavNames(fit, "lv")) != 0) {
            stop("Does not support a model with latent variable(s).")
          }
      }
    TRUE
  }