#' @title Delta_Med by Liu, Yuan, and Li (2023)
#'
#' @description It computes the
#' Delta_Med proposed by Liu, Yuan,
#' and Li (2023), an $R^2$-like measure of
#' indirect effect.
#'
#' @details
#' It computes Delta_Med an
#' $R^2$-like effect
#' size measure for the indirect effect
#' from one variable (the `y`-variable)
#' to another variable (the `x`-variable)
#' through one or more mediators
#' (`m`, or `m1`, `m2`, etc. when
#' there are more than one mediators).
#'
#' The Delta_Med of one or more
#' mediators was computed as the
#' difference between
#' two $R^2$s:
#'
#'  - The $R^2$ when `y`
#'    is predicted by `x` and all
#'    mediators.
#'
#'  - The $R^2$ when the mediator(s) of
#'    interest is/are removed from the
#'    models, while the error term(s)
#'    of the mediator(s) is/are kept.
#'
#' Please refer to Liu et al. (2023)
#' for the technical details.
#'
#' It can also be used for for
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
#' not refitted, hence keeping all
#' estimates of all parameters unchanged.
#' It then uses [lavaan::lav_model_set_parameters()]
#' to update the parameters,
#' [lavaan::lav_model_implied()] to
#' update the implied statistics, and
#' then call [lavaan::lavInspect()] to
#' retrieve the implied variance of the
#' predicted values of `y` for computing
#' the second $R^2$. Subtracting this
#' $R^2$ from the original $R^2$ of
#' `y` can then yield the $Delta^{Med}.
#'
#' # Limitations
#'
#' For now, it only supports the types
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
#' It may be applicable to other kinds
#' of models but more studies are needed
#' to investigate these situations.
#' Although the function can be used
#' in cases not discussed in
#' Liu et al. (2023), using it in those
#' cases should be considered as
#' experimental, or for simulation
#' studies.
#'
#' For research purpose, most of the
#' requirements above can be removed
#' by users using the relevant
#' arguments.
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
#' method. (TODO: To write)
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
#' `lavaan`` syntax. For example,
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
#' @references
#' Liu, H., Yuan, K.-H., & Li, H. (2023).
#' A systematic framework for defining
#' R-squared measures in mediation
#' analysis. *Psychological Methods*.
#' Advance online publication.
#' https://doi.org/10.1037/met0000571
#'
#'
#' @examples
#' # TO PREPARE
#' # library(lavaan)
#' # dat <- data_med
#' # head(dat)
#' # mod <-
#' # "
#' # m ~ x + c1 + c2
#' # y ~ m + x + c1 + c2
#' # "
#' # fit <- sem(mod, dat, fixed.x = FALSE)
#' # out <- indirect_proportion(x = "x",
#' #                            y = "y",
#' #                            m = "m",
#' #                            fit = fit)
#' # out
#'
#' @export

delta_med <- function(x,
                      y,
                      m,
                      fit,
                      paths_to_remove = NULL,
                      boot_out = NULL,
                      level = .95,
                      progress = TRUE) {
    if (missing(x) || missing(m) || missing(y)) {
        stop("x, m, and y must all be specified.")
      }
    out <- delta_med_i(fit = fit,
                       x = x,
                       m = m,
                       y = y,
                       paths_to_remove = paths_to_remove)
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
        dm_boot_out <- form_boot_ci(est = dm,
                                    boot_est = dm_boot,
                                    level = level)
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
                         level = .95) {
    out <- list()
    out$est <- est
    out$boot_est <- boot_est
    nboot <- length(boot_est)
    tmp <- list(t = matrix(boot_est, nrow = nboot, ncol = 1),
                t0 = est,
                R = nboot)
    boot_ci0 <- boot::boot.ci(tmp, conf = level, type = "perc")
    boot_ci1 <- boot_ci0$percent[4:5]
    names(boot_ci1) <- paste0(formatC(c(100 * (1 - level) / 2,
                                  100 * (1 - (1 - level) / 2)), 2,
                                  format = "f"), "%")
    out$boot_ci <- boot_ci1
    out$level <- level
    out$boot_p <- est2p(out$boot_est)
    out$boot_se <- stats::sd(out$boot_est, na.rm = TRUE)
    out
  }

#' @noRd

delta_med_check_fit <- function(x,
                                y,
                                m,
                                fit,
                                skip_check_single_x = FALSE,
                                skip_check_m_between_x_y = FALSE
                                ) {
    # TODO
  }