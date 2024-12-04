
#' @title Convert a 'cond_indirect_effects' Class
#' Object to a Data Frame
#'
#' @description Coerce a 'cond_indirect_effects'
#' object to a data frame, with columns
#' such as *p*-values and standard errors
#' added if requested.
#'
#' @details
#' If bootstrapping confidence intervals
#' were requested, this method has the
#' option to print
#' *p*-values computed by the
#' method presented in Asparouhov and Muthén (2021).
#' Note that these *p*-values are asymmetric
#' bootstrap *p*-values based on the
#' distribution of the bootstrap estimates.
#' They not computed based on the
#' distribution under the null hypothesis.
#'
#' For a *p*-value of *a*, it means that
#' a 100(1 - *a*)% bootstrapping confidence
#' interval
#' will have one of its limits equal to
#' 0. A confidence interval
#' with a higher confidence level will
#' include zero, while a confidence
#' interval with a lower confidence level
#' will exclude zero.
#'
#' ## Using Original Standard Errors
#'
#' If these conditions are met, the
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
#' used, and the *p*-value and confidence
#' intervals are computed from the *t*
#' statistic.
#'
#' If the model is fitted by structural
#' equation modeling using `lavaan`, then
#' the variance-covariance computed by
#' `lavaan` will be used, and the *p*-value
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
#' @return A data frame with the
#' conditional effects and
#' confidence intervals (if available),
#' with other requested columns added.
#'
#' @param x A `cond_indirect_effect`
#' object, such as the output of
#' [cond_indirect_effects()]
#' or [cond_effects()].
#'
#' @param row.names Not used. Included
#' to be compatible with the generic
#' methods.
#'
#' @param optional Not used. Included
#' to be compatible with the generic
#' methods.
#'
#' @param add_sig Whether a column
#' of significance test results
#' will be added. Default is `TRUE`.
#'
#' @param digits Number of digits to
#' display. Default is 3. Ignored
#' if `to_string` is `FALSE`.
#'
#' @param pvalue Logical. If `TRUE`,
#' asymmetric *p*-values based on
#' bootstrapping will be printed if
#' available. Default to `FALSE` if
#' confidence intervals have already
#' computed. Default to `TRUE` if
#' no confidence intervals have been
#' computed and the original standard
#' errors are to be used. See Details
#' on when the original standard errors
#' will be used by default.
#'
#' @param pvalue_digits Number of decimal
#' places to display for the *p*-values.
#' Default is 3. Ignored
#' if `to_string` is `FALSE`.
#'
#' @param se Logical. If `TRUE` and
#' confidence intervals are available,
#' the standard errors of the estimates
#' are also printed. They are simply the
#' standard deviations of the bootstrap
#' estimates or Monte Carlo simulated
#' values, depending on the method used
#' to form the confidence intervals.
#' Default to `FALSE` if
#' confidence intervals are available.
#' Default to `TRUE` if
#' no confidence intervals have been
#' computed and the original standard
#' errors are to be used. See Details
#' on when the original standard errors
#' will be used by default.
#'
#' @param level The level of confidence
#' for the confidence intervals computed
#' from the original standard errors. Used only for
#' paths without mediators and both
#' `x`- and `y`-variables are not
#' standardized.
#'
#' @param se_ci Logical. If `TRUE` and
#' confidence interval has not been
#' computed, the function will try
#' to compute them from stored
#' standard errors if the original
#' standard errors are to be used.
#' Ignored
#' if confidence intervals have already
#' been computed. Default
#' to `TRUE`.
#'
#' @param to_string If `TRUE`, numeric
#' columns will be converted to string
#' columns, formatted based on `digits`
#' and `pvalue_digits`. For printing.
#' Default is `FALSE`.
#'
#' @param ...  Other arguments. Not
#' used.
#'
#' @seealso [cond_indirect_effects()] and
#' [cond_effects()].
#'
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
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
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the default levels
#' cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = "w1", fit = fit)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the default levels
#' out <- cond_indirect_effects(x = "x", y = "y", m = "m1",
#'                       wlevels = "w1", fit = fit)
#' out
#'
#' as.data.frame(out)
#'
#' as.data.frame(out, to_string = TRUE)
#'
#'
#' @export

as.data.frame.cond_indirect_effects <- function(x,
                                                row.names = NULL,
                                                optional = NULL,
                                                digits = 3,
                                                add_sig = TRUE,
                                                pvalue = NULL,
                                                pvalue_digits = 3,
                                                se = NULL,
                                                level = .95,
                                                se_ci = TRUE,
                                                to_string = FALSE,
                                                ...) {
  # Copied from print.cond_indirect_effects()
  # TODO:
  # - Update print.cond_indirect_effects() to call this function
  #   instead of creating the data frame itself.
  full_output <- attr(x, "full_output")
  x_i <- full_output[[1]]
  my_call <- attr(x, "call")
  cc_call <- x_i$cond_indirect_call
  boot_ci <- !is.null(x_i$boot_ci)
  has_ci <- FALSE
  ci_type <- NULL
  boot_type <- NULL
  has_groups <- ("group" %in% tolower(colnames(x)))
  if (has_groups) {
      group_labels <- unique(x$Group)
      group_numbers <- unique(x$Group_ID)
    } else {
      group_labels <- NULL
      group_numbers <- NULL
    }
  has_wlevels <- !is.null(attr(x, "wlevels"))
  if (!is.null(x_i$boot_ci)) {
      has_ci <- TRUE
      ci_type <- "boot"
      ind_name <- "boot_indirect"
      se_name <- "boot_se"
      boot_type <- x_i$boot_type
      if (is.null(boot_type)) boot_type <- "perc"
    }
  if (!is.null(x_i$mc_ci)) {
      has_ci <- TRUE
      ci_type <- "mc"
      ind_name <- "mc_indirect"
      se_name <- "mc_se"
    }
  standardized_x <- x_i$standardized_x
  standardized_y <- x_i$standardized_y
  has_m <- isTRUE(!is.null(x_i$m))

  # Default to OLS or Wald SE
  se_out <- cond_effects_original_se(x,
                                     level = level,
                                     append = FALSE)
  has_original_se <- !is.null(se_out)
  print_original_se <- FALSE
  if (!has_ci &&
      !has_m &&
      !has_groups &&
      has_wlevels &&
      !standardized_x &&
      !standardized_y &&
      has_original_se) {
      print_original_se <- TRUE
      if (is.null(pvalue)) {
          pvalue <- TRUE
        }
      if (is.null(se)) {
          se <- TRUE
        }
    } else {
      if (is.null(pvalue)) {
          pvalue <- FALSE
        }
      if (is.null(se)) {
          se <- FALSE
        }
      level <- x_i$level
    }

  # R <- ifelse(has_ci, length(x_i[[ind_name]]),
  #                      NA)
  # x0 <- attr(x, "x")
  # y0 <- attr(x, "y")
  # m0 <- attr(x, "m")
  # if (has_m) {
  #     path <- path <- paste0(x0, " -> ",
  #                            paste0(eval(m0), collapse = " -> "),
  #                            " -> ", y0)
  #   } else {
  #     path <- paste(x0, "->", y0)
  #   }
  if (to_string) {
      out <- lapply(x, format_numeric, digits = digits)
    } else {
      out <- lapply(x, function(x) x)
    }
  if (has_ci) {
      Sig <- ifelse((x$CI.lo > 0) | (x$CI.hi < 0), "Sig", "")
      i <- which(names(out) == "CI.hi")
      j <- length(out)
      out <- c(out[1:i], list(Sig = Sig), out[(i + 1):j])
      if ((ci_type == "boot") && pvalue) {
          boot_p <- sapply(attr(x, "full_output"), function(x) x$boot_p)
          boot_p <- unname(boot_p)
          if (to_string) {
              boot_p1 <- sapply(boot_p, function(xx) {
                  if (!is.na(xx)) {
                      return(formatC(xx, digits = pvalue_digits, format = "f"))
                    } else {
                      return("NA")
                    }
                })
            } else {
              boot_p1 <- boot_p
            }
          i <- which(names(out) == "Sig")
          j <- length(out)
          out <- c(out[1:i], list(pvalue = boot_p1), out[(i + 1):j])
        }
      if (se) {
          ind_se <- sapply(attr(x, "full_output"), function(x) x[[se_name]])
          ind_se <- unname(ind_se)
          if (to_string) {
              ind_se <- sapply(ind_se, function(xx) {
                  if (!is.na(xx)) {
                      return(formatC(xx, digits = digits, format = "f"))
                    } else {
                      return("NA")
                    }
                })
            }
          i <- which(names(out) == "Sig")
          j <- length(out)
          out <- c(out[1:i], list(SE = ind_se), out[(i + 1):j])
        }
    }
  if (!has_ci &&
      !has_m &&
      !has_groups &&
      has_wlevels &&
      !standardized_x &&
      !standardized_y &&
      has_original_se) {
      # OLS or Wald SE
      # Moderation only
      print_original_se <- TRUE
      # t or Wald SE, CI, and p-values
      # TODO: Support multiple-group models
      out_original <- list()
      if (se) {
          out_se <- unname(se_out$se)
          if (to_string) {
              out_se <- sapply(out_se, function(xx) {
                  if (!is.na(xx)) {
                      return(formatC(xx, digits = digits, format = "f"))
                    } else {
                      return("NA")
                    }
                })
            }
          out_original <- c(out_original,
                            list(SE = out_se))
        }
      if (pvalue) {
          out_stat <- unname(se_out$stat)
          if (to_string) {
              out_stat <- sapply(out_stat, function(xx) {
                  if (!is.na(xx)) {
                      return(formatC(xx, digits = digits, format = "f"))
                    } else {
                      return("NA")
                    }
                })
            }
          out_p <- unname(se_out$p)
          if (to_string) {
              out_p <- sapply(out_p, function(xx) {
                            if (!is.na(xx)) {
                                return(formatC(xx, digits = pvalue_digits, format = "f"))
                              } else {
                                return("NA")
                              }
                          })
            }
          out_sig0 <- as.character(unname(se_out$sig))
          out_sig <- tryCatch(format_stars(out_sig0),
                              error = function(e) e)
          if (inherits(out_sig, "error")) {
              out_sig <- out_sig0
            }
          out_original <- c(out_original,
                            list(Stat = unname(out_stat),
                                 pvalue = unname(out_p),
                                 Sig = unname(out_sig)))
        }
      if (se_ci) {
          out_cilo <- unname(se_out$cilo)
          out_cihi <- unname(se_out$cihi)
          if (to_string) {
              out_cilo <- sapply(out_cilo, function(xx) {
                  if (!is.na(xx)) {
                      return(formatC(xx, digits = digits, format = "f"))
                    } else {
                      return("NA")
                    }
                })
              out_cihi <- sapply(out_cihi, function(xx) {
                  if (!is.na(xx)) {
                      return(formatC(xx, digits = digits, format = "f"))
                    } else {
                      return("NA")
                    }
                })
            }
          out_original <- c(out_original,
                            list(`CI.lo` = out_cilo,
                                 `CI.hi` = out_cihi))
        }
      rownames(out_original) <- NULL
      i <- which(names(out) == "ind")
      j <- length(out)
      out <- c(out[1:i], out_original, out[(i + 1):j])
    }
  # Drop the component column if the model has no mediator
  if (!has_m) {
      i <- which(names(out) %in% names(x_i$components))
      if (length(i) > 0) {
          out <- out[-i]
        }
    }
  if (!add_sig && isTRUE("Sig" %in% names(out))) {
      out <- out[-which(names(out) == "Sig")]
    }
  out1 <- data.frame(out, check.names = FALSE)
  return(out1)
}
