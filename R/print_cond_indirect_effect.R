#' @title Print a
#' 'cond_indirect_effects' Class Object
#'
#' @description Print the content of the
#' output of [cond_indirect_effects()]
#'
#' @details The `print` method of the
#' `cond_indirect_effects`-class object.
#'
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
##' ## Caution
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
#' @return `x` is returned invisibly.
#'  Called for its side effect.
#'
#' @param x The output of
#' [cond_indirect_effects()].
#'
#' @param digits Number of digits to
#' display. Default is 3.
#'
#' @param annotation Logical. Whether
#' the annotation after the table of
#' effects is to be printed. Default is
#' `TRUE.`
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
#' Default is 3.
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
#' @param ...  Other arguments. Not
#' used.
#'
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#'
#' @seealso [cond_indirect_effects()]
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
#' print(out, digits = 5)
#'
#' print(out, annotation = FALSE)
#'
#'
#' @export

print.cond_indirect_effects <- function(x, digits = 3,
                                        annotation = TRUE,
                                        pvalue = NULL,
                                        pvalue_digits = 3,
                                        se = NULL,
                                        level = .95,
                                        se_ci = TRUE,
                                        ...) {
  # TODO:
  # - Support cases with both moderators and groups.
  fit_type <- tryCatch(cond_indirect_check_fit(attr(x, "fit")),
                       error = function(e) e)
  if (inherits(fit_type, "error")) {
      fit_type <- "unknown"
    }
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

  R <- ifelse(has_ci, length(x_i[[ind_name]]),
                       NA)
  x0 <- attr(x, "x")
  y0 <- attr(x, "y")
  m0 <- attr(x, "m")
  if (has_m) {
      path <- path <- paste0(x0, " -> ",
                             paste0(eval(m0), collapse = " -> "),
                             " -> ", y0)
    } else {
      path <- paste(x0, "->", y0)
    }
  out <- lapply(x, format_numeric, digits = digits)
  if (has_ci) {
      Sig <- ifelse((x$CI.lo > 0) | (x$CI.hi < 0), "Sig", "")
      i <- which(names(out) == "CI.hi")
      j <- length(out)
      out <- c(out[1:i], list(Sig = Sig), out[(i + 1):j])
      if ((ci_type == "boot") && pvalue) {
          boot_p <- sapply(attr(x, "full_output"), function(x) x$boot_p)
          boot_p <- unname(boot_p)
          boot_p1 <- sapply(boot_p, function(xx) {
              if (!is.na(xx)) {
                  return(formatC(xx, digits = pvalue_digits, format = "f"))
                } else {
                  return("NA")
                }
            })
          i <- which(names(out) == "Sig")
          j <- length(out)
          out <- c(out[1:i], list(pvalue = boot_p1), out[(i + 1):j])
        }
      if (se) {
          ind_se <- sapply(attr(x, "full_output"), function(x) x[[se_name]])
          ind_se <- unname(ind_se)
          ind_se <- sapply(ind_se, function(xx) {
              if (!is.na(xx)) {
                  return(formatC(xx, digits = digits, format = "f"))
                } else {
                  return("NA")
                }
            })
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
          out_se <- sapply(out_se, function(xx) {
              if (!is.na(xx)) {
                  return(formatC(xx, digits = digits, format = "f"))
                } else {
                  return("NA")
                }
            })
          out_original <- c(out_original,
                            list(SE = out_se))
        }
      if (pvalue) {
          out_stat <- unname(se_out$stat)
          out_stat <- sapply(out_stat, function(xx) {
              if (!is.na(xx)) {
                  return(formatC(xx, digits = digits, format = "f"))
                } else {
                  return("NA")
                }
            })
          out_p <- unname(se_out$p)
          out_p <- sapply(out_p, function(xx) {
                        if (!is.na(xx)) {
                            return(formatC(xx, digits = pvalue_digits, format = "f"))
                          } else {
                            return("NA")
                          }
                      })
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
  out1 <- data.frame(out, check.names = FALSE)
  if (has_wlevels) {
      wlevels <- attr(x, "wlevels")
      w0 <- colnames(attr(wlevels, "wlevels"))
      w1 <- colnames(wlevels)
    } else {
      wlevels <- NULL
      w0 <- NULL
      w1 <- NULL
    }
  mcond <- names(x_i$components)
  cond_str <- ""
  cond_str2 <- ""
  if (has_m) {
      if (has_wlevels) {
          cond_str <- "conditional indirect"
          cond_str2 <- "Conditional indirect"
        } else {
          cond_str <- "indirect"
          cond_str2 <- "Indirect"
        }
    } else {
      if (has_wlevels) {
          cond_str <- "conditional"
          cond_str2 <- "Conditional"
        } else {
          cond_str <- "direct"
          cond_str2 <- "Direct"
        }
    }
  if (has_m) {
      cat("\n== Conditional indirect effects ==\n")
    } else {
      cat("\n== Conditional effects ==\n")
    }
  cat("\n Path:", path)
  if (has_wlevels) {
      cat("\n Conditional on moderator(s):", paste0(w0, collapse = ", "))
      cat("\n Moderator(s) represented by:", paste0(w1, collapse = ", "))
    }
  if (has_groups) {
      tmp <- paste0(group_labels, "[", group_numbers, "]",
                    collapse = ", ")
      cat("\n Conditional on group(s):", tmp)
    }
  xold <- x
  x <- out1
  cat("\n\n")
  NextMethod()
  if (annotation) {
      if (has_ci) {
          level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
          cat("\n ")
          tmp <- switch(ci_type,
                        boot = switch(boot_type,
                                      perc = "percentile",
                                      bc = "bias-corrected"),
                        mc = NULL)
          tmp1 <- switch(ci_type,
                    boot = paste(tmp, "confidence intervals by nonparametric bootstrapping"),
                    mc = "Monte Carlo confidence intervals")
          tmp2 <- switch(ci_type,
                    boot = paste("with", R, "samples."),
                    mc = paste("with", R, "replications."))
          cat(strwrap(paste("- [CI.lo to CI.hi] are",
                            level_str,
                            tmp1,
                            tmp2), exdent = 3), sep = "\n")
          if (pvalue && (ci_type == "boot")) {
              tmp1 <- " - [pvalue] are asymmetric bootstrap p-values."
              cat(tmp1, sep = "\n")
            }
          if (se) {
              tmp1 <- " - [SE] are standard errors."
              cat(tmp1, sep = "\n")
            }
        } else if (print_original_se) {
          # Original SE used
          cat("\n ")
          level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
          if (se) {
            tmp <- switch(fit_type,
                          lavaan = "'lavaan' standard errors.",
                          lavaan.mi = "'lavaan' standard errors.",
                          lm = "regression standard errors.",
                          "standard errors.")
                cat(strwrap(paste(" - [SE] are",
                                  tmp), exdent = 3), sep = "\n")
            }
          if ("Stat" %in% colnames(x)) {
              tmp <- switch(fit_type,
                            lavaan = "the z statistics used to test the effects.",
                            lavaan.mi = "the z statistics used to test the effects.",
                            lm = "the t statistics used to test the effects.",
                            "the statistics used to test the effects.")
              cat(" ")
              cat(strwrap(paste("- [Stat] are",
                                tmp), exdent = 3), sep = "\n")
            }
          if (pvalue) {
              tmp <- switch(fit_type,
                            lavaan = "p-values computed from 'Stat'.",
                            lavaan.mi = "p-values computed from 'Stat'.",
                            lm = "p-values computed from 'Stat'.",
                            "p-values computed from 'Stat'.")
              cat(" ")
              cat(strwrap(paste("- [pvalue] are",
                                tmp), exdent = 3), sep = "\n")
            }
          if ("Sig" %in% colnames(x)) {
              tmp <- attr(se_out$sig, "legend")
              cat(" ")
              cat(strwrap(paste0("- [Sig]: ",
                                tmp, "."), exdent = 3), sep = "\n")
            }
          if (se_ci) {
              tmp <- switch(fit_type,
                            lavaan = "confidence interval computed from 'lavaan' standard errors.",
                            lavaan.mi = "confidence interval computed from 'lavaan' standard errors.",
                            lm = "confidence interval computed from regression standard errors.",
                            "confidence interval computed from standard errors.")
              cat(" ")
              cat(strwrap(paste("- [CI.lo to CI.hi] are",
                                level_str,
                                tmp), exdent = 3), sep = "\n")
            }
          if (fit_type %in% c("lavaan", "lavaan.mi")) {
              cat(" ")
              tmp <- paste("- IMPORTANT: For a model fitted by structural equation model,",
                           "the p-values and confidence intervals",
                           "for the conditional effects",
                           "computed from standard errors can only be trusted if",
                           "all covariances involving the product terms are free.",
                           "Otherwise, the model may not be invariant to",
                           "linear transformation of the variables.")
              cat(strwrap(tmp, exdent = 3), sep = "\n")
            }
        } else {
          cat("\n")
        }
      if (standardized_x && standardized_y) {
          cat(" - std: The standardized", cond_str, "effects.",
              "\n - ind: The unstandardized", cond_str, "effects.", sep = " ")
        }
      if (standardized_x && !standardized_y) {
          cat(" - std: The partially standardized", cond_str, "effects.",
              "\n - ", sQuote(x0), " is standardized.",
              "\n - ind: The unstandardized", cond_str, "effects.", sep = " ")
        }
      if (!standardized_x && standardized_y) {
          cat(" - std: Te partially standardized", cond_str, "effects.",
              "\n - ", sQuote(y0), " is standardized.",
              "\n - ind: The unstandardized", cond_str, "effects.", sep = " ")
        }
      if (!standardized_x & !standardized_y) {
          cat(" - The 'ind' column shows the", cond_str, "effects.", sep = " ")
        }
      cat("\n ")
      tmp <- ifelse(has_wlevels && has_groups,
                    "the moderators and/or groups.",
                ifelse(has_wlevels,
                       "the moderator(s).",
                       "the group(s)."))
      if (has_m) {
          cat(strwrap(paste("\n -", paste(sQuote(mcond), collapse = ","),
              "is/are the path coefficient(s) along the path",
              "conditional on",
              tmp), exdent = 3), sep = "\n")
        }
      cat("\n")
    }
  invisible(x)
}

format_numeric <- function(xi, digits = 3, check_integer = TRUE) {
    if (!is.numeric(xi)) return(format(xi))
    if (isTRUE(all.equal(round(xi), xi))) return(round(xi, 0))
    xi <- formatC(xi, digits = digits, format = "f")
    xi
  }

format_stars <- function(sigs) {
    if (!is.character(sigs)) return(format(sigs))
    max_width <- max(sapply(sigs, nchar), na.rm = TRUE)
    out <- sapply(sigs,
            function(xx) {
                if (is.na(xx)) return(xx)
                yy <- max_width - nchar(xx)
                if (yy > 0) {
                    xx <- paste0(xx,
                                 paste0(rep(" ", yy), collapse = ""))
                  } else {
                    return(xx)
                  }
              })
    unname(out)
  }

cond_effects_original_se <- function(object,
                                     level = .95,
                                     append = FALSE) {
    full_output <- attr(object, "full_output")
    full_output_1 <- full_output[[1]]
    if (full_output_1$standardized_x ||
        full_output_1$standardized_y) {
        if (append) {
            return(object)
          } else {
            return(NULL)
          }
      }
    if (!is.null(full_output_1$m)) {
        if (append) {
            return(object)
          } else {
            return(NULL)
          }
      }
    if (is.null(full_output_1$original_se)) {
        if (append) {
            return(object)
          } else {
            return(NULL)
          }
      }
    est <- object$ind
    se <- sapply(full_output,
                 function(x) x$original_se)
    if (all(is.na(se))) {
        return(NULL)
      }
    dfres <- sapply(full_output,
                    function(x) x$df_residual)
    test_stat <- est / se
    p <- 2 * stats::pt(abs(test_stat),
                       df = dfres,
                       lower.tail = FALSE)
    sig <- stats::symnum(p,
                         corr = FALSE,
                         na = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                         symbols = c("***", "**", "*", " "))
    z_crit <- -1 * stats::qt((1 - level) / 2,
                             df = dfres,
                             lower.tail = TRUE)
    cilo <- est - z_crit * se
    cihi <- est + z_crit * se
    if (append) {
        object$SE <- se
        object$Stat <- test_stat
        object$P <- p
        object$Sig <- sig
        object$CI.Lo <- cilo
        object$CI.Hi <- cihi
        attr(object, "sig_legend") <- attr(sig, "legend")
        attr(object, "original_se_level") <- level
        return(object)
      } else {
        out <- list(se = se,
                    stat = test_stat,
                    pvalue = p,
                    sig = sig,
                    cilo = cilo,
                    cihi = cihi)
        attr(out, "sig_legend") <- attr(sig, "legend")
        attr(out, "original_se_level") <- level
        return(out)
      }
  }
