#' @title Confidence Intervals of
#' Indirect Effects or Conditional
#' Indirect Effects
#'
#' @description Return the confidence
#' intervals of the conditional indirect
#' effects or conditional effects in the
#' output of [cond_indirect_effects()].
#'
#' @details It extracts and returns the
#' columns for confidence intervals, if
#' available.
#'
#' The type of confidence intervals
#' depends on the call used to
#' compute the effects. If confidence
#' intervals have already been formed
#' (e.g., by bootstrapping or Monte
#' Carlo), then this function
#' merely retrieves the confidence
#' intervals stored.
#'
#' If the following conditions are met, the
#' stored standard errors, if available,
#' will be used test an effect and
#' form it confidence interval:
#'
#' - Confidence intervals have not been
#'  formed (e.g., by bootstrapping or
#'  Monte Carlo).
#'
#' - The path has no mediators.
#'
#' - The model has only one group.
#'
#' - The path is moderated by one or
#'  more moderator.
#'
#' - Both the `x`-variable and the
#'  `y`-variable are not standardized.
#'
#' If the model is fitted by OLS
#' regression (e.g., using [stats::lm()]),
#' then the variance-covariance matrix
#' of the coefficient estimates will be
#' used, and confidence
#' intervals are computed from the *t*
#' statistic.
#'
#' If the model is fitted by structural
#' equation modeling using `lavaan`, then
#' the variance-covariance computed by
#' `lavaan` will be used,
#' and confidence intervals are computed
#' from the *z* statistic.
#'
#' ## Caution
#'
#' If the model is fitted by structural
#' equation modeling and has moderators,
#' the standard errors, *p*-values,
#' and confidence interval computed
#' from the variance-covariance matrices
#' for conditional effects
#' can only be trusted if all covariances
#' involving the product terms are free.
#' If any of them are fixed, for example,
#' fixed to zero, it is possible
#' that the model is not invariant to
#' linear transformation of the variables.
#'
#' @inheritParams confint.indirect
#'
#' @param object The output of
#' [cond_indirect_effects()].
#'
#' @param parm Ignored. Always returns
#' the confidence intervals of
#' the effects for all levels stored.

#' @param ...  Additional arguments.
#' To be passed to [confint.indirect()].
#' (This new behavior applies to 0.3.6.15
#' and later version.)
#'
#' @return A data frame with two
#' columns, one for each confidence
#' limit of the confidence intervals.
#' The number of rows is equal to the
#' number of rows of `object`.
#'
#' @seealso [cond_indirect_effects()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ x  + w1 + x:w1
#' m2 ~ m1
#' y  ~ m2 + x + w4 + m2:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for cond_indirect():
#'
#' # Create levels of w1 and w4
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#' w4levels <- mod_levels("w4", fit = fit)
#' w4levels
#' w1w4levels <- merge_mod_levels(w1levels, w4levels)
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' # R should be at least 2000 or 5000 in real research.
#' out1 <- suppressWarnings(cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = w1levels, fit = fit,
#'                       boot_ci = TRUE, R = 20, seed = 54151,
#'                       parallel = FALSE,
#'                       progress = FALSE))
#' confint(out1)
#'
#'
#' @export

confint.cond_indirect_effects <- function(
  object,
  parm,
  level = NULL,
  ...
) {
    level_default <- .95
    has_wlevels <- cond_indirect_effects_has_wlevels(object)
    has_groups <- cond_indirect_effects_has_groups(object)
    out0 <- as.data.frame(object)
    full_output <- attr(object, "full_output")
    x_i <- full_output[[1]]
    has_ci <- FALSE
    if (!is.null(full_output[[1]]$boot_ci)) {

        # ==== Boot CI found ====

        has_ci <- TRUE
        ci_type <- "boot"
      }
    if (!is.null(full_output[[1]]$mc_ci)) {

        # ==== Monte Carlo CI found ====

        has_ci <- TRUE
        ci_type <- "mc"
      }

    # ==== Handle level ====

    if (has_ci) {
      if (is.null(level)) {
        # Use stored level if possible
        # All objects should have the same `level`
        all_levels <- sapply(
          full_output,
          function(x) x$level
        )
        if (isTRUE(all.equal(max(all_levels), min(all_levels)))) {
          level <- max(all_levels)
        } else {
          warning("The levels of confidence cannot be determined. ",
                  level_default,
                  " is used instead.")
          level <- level_default
        }
      } else {
        # Always use explicitly specified level
        # Spaceholder
      }
    } else {
      # No boot CI or Monte Carlo CI
      if (is.null(level)) {
        level <- level_default
      }
    }

    # ==== SE CI: Try ====

    se_out <- cond_effects_original_se(object,
                                       level = level,
                                       append = FALSE)
    has_original_se <- !is.null(se_out)
    has_m <- isTRUE(!is.null(x_i$m))
    standardized_x <- x_i$standardized_x
    standardized_y <- x_i$standardized_y
    se_ci <- FALSE

    if (!has_ci &&
        !has_m &&
        !has_groups &&
        has_wlevels &&
        !standardized_x &&
        !standardized_y &&
        has_original_se) {

        # ==== Use SE CI ====

        out0[, c("CI.lo")] <- se_out$cilo
        out0[, c("CI.hi")] <- se_out$cihi
        se_ci <- TRUE
        has_ci <- TRUE
        # For se_ci, level_default is used if not set
        if (is.null(level)) {
          level <- level_default
        }
      }
    # ==== has_ci? ====

    if (!has_ci) {
          warning("Confidence intervals not in the object.")
          out <- data.frame("CI.o" = rep(NA, nrow(object)),
                             "CI.hi" = rep(NA, nrow(object)))
      } else {
        # out is the tentative output
        out <- out0[, c("CI.lo", "CI.hi")]
        # The CIs in out may be SE CI
        if (!se_ci) {

          # ==== Boot CI or Monte Carlo CI ====

          # Update the content of out

          # Always call confint(),
          # such that dotdotdot can be used,
          # except when SEs are used.
          # confint.indirect may still use stored CIs.
          ci_out <- lapply(
            full_output,
            stats::confint,
            level = level,
            ...
          )
          ci_out <- do.call(
                  rbind,
                  ci_out
                )
          # Column names reflect ci_type and level
          colnames(out) <- colnames(ci_out)
          out[] <- ci_out
        }
      }

    if (se_ci) {

      # out's CIs are already SE CIs. Just update the column names

      # ==== Override the column names (SE CI)====

      # Borrowed from stats::confint()
      probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
      cnames <- paste(format(100 * probs,
                            trim = TRUE,
                            scientific = FALSE,
                            digits = 2), "%")
      colnames(out) <- cnames
    }
    if (has_wlevels && !has_groups) {
        wlevels <- attr(object, "wlevels")
        rownames(out) <- rownames(wlevels)
      }
    if (!has_wlevels && has_groups) {
        tmp <- paste0(object$Group, " [", object$Group_ID, "]")
        rownames(out) <- tmp
      }
    if (has_wlevels && has_groups) {
        # TODO:
        # - Support for having both wlevels and groups
      }
    out
  }
