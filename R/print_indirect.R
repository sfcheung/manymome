#' @title Print an 'indirect' Class
#' Object
#'
#' @description Print the content of the
#' output of [indirect_effect()] or
#' [cond_indirect()].
#'
#' @details The `print` method of the
#' `indirect`-class object.
#'
#' If bootstrapping confidence interval
#' was requested, this method has the
#' option to print a
#' *p*-value computed by the
#' method presented in Asparouhov and Muthén (2021).
#' Note that this *p*-value is asymmetric
#' bootstrap *p*-value based on the
#' distribution of the bootstrap estimates.
#' It is not computed based on the
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
#' We recommend using confidence interval
#' directly. Therefore, *p*-value is not
#' printed by default. Nevertheless,
#' users who need it can request it
#' by setting `pvalue` to `TRUE`.
#'
#' ## Using Original Standard Errors
#'
#' If these conditions are met, the
#' stored standard error, if available,
#' will be used to test an effect and
#' form it confidence interval:
#'
#' - Confidence interval has not been
#'  formed (e.g., by bootstrapping or
#'  Monte Carlo).
#'
#' - The path has no mediators.
#'
#' - The model has only one group.
#'
#' - Both the `x`-variable and the
#'  `y`-variable are not standardized.
#'
#' If the model is fitted by OLS
#' regression (e.g., using [stats::lm()]),
#' then the variance-covariance matrix
#' of the coefficient estimates will be
#' used, and the *p*-value and confidence
#' interval are computed from the *t*
#' statistic.
#'
#' If the model is fitted by structural
#' equation modeling using `lavaan`, then
#' the variance-covariance computed by
#' `lavaan` will be used, and the *p*-value
#' and confidence interval are computed
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
#' If any some of them are fixed, for example,
#' fixed to zero, it is possible
#' that the model is not invariant to
#' linear transformation of the variables.
#'
#' @return `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x The output of
#' [indirect_effect()] or
#' [cond_indirect()].
#'
#' @param digits Number of digits to
#' display. Default is 3.
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
#' will be used by default. Default is
#' `NULL` and its value determined as
#' stated above.
#'
#' @param pvalue_digits Number of decimal
#' places to display for the *p*-value.
#' Default is 3.
#'
#' @param se Logical. If `TRUE` and
#' confidence interval has been formed,
#' the standard error of the estimates
#' are also printed. It is simply the
#' standard deviation of the bootstrap
#' estimates or Monte Carlo simulated
#' values, depending on the method used
#' to form the confidence intervals.
#' Default to `FALSE` if
#' confidence interval has been
#' formed.
#' Default to `TRUE` if
#' no confidence interval has been
#' computed and the original standard
#' errors are to be used. See Details
#' on when the original standard errors
#' will be used by default.
#' Default is `NULL` and its value
#' determined as stated above.
#'
#' @param level The level of confidence
#' for the confidence interval computed
#' from the original standard errors. Used only for
#' paths without mediators and both
#' `x`- and `y`-variables are not
#' standardized.
#'
#' @param se_ci Logical. If `TRUE` and
#' confidence interval has not been
#' computed, the function will try
#' to compute them from stored
#' standard error if the original
#' standard error is to be used.
#' Ignored
#' if confidence interval has already
#' been computed. Default
#' to `TRUE`.
#'
#'
#' @param ... Other arguments. Not used.
#'
#'
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' @seealso [indirect_effect()] and
#' [cond_indirect()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
#' m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
#' m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
#' y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
#'
#' indirect_1 <- cond_indirect(x = "x", y = "y",
#'                             m = c("m1", "m2", "m3"),
#'                             fit = fit,
#'                             wvalues = wvalues)
#' indirect_1
#'
#' dat <- modmed_x1m3w4y1
#' mod2 <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + x
#' "
#' fit2 <- sem(mod2, dat,
#'             meanstructure = TRUE, fixed.x = FALSE,
#'             se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' indirect_2 <- indirect_effect(x = "x", y = "y",
#'                               m = c("m1", "m2", "m3"),
#'                               fit = fit2)
#' indirect_2
#' print(indirect_2, digits = 5)
#'
#'
#' @export

print.indirect <- function(x,
                           digits = 3,
                           pvalue = NULL,
                           pvalue_digits = 3,
                           se = NULL,
                           level = .95,
                           se_ci = TRUE,
                           ...) {
    xold <- x
    my_call <- x$call
    wvalues <- x$wvalues
    standardized_x <- x$standardized_x
    standardized_y <- x$standardized_y
    standardized <- (standardized_x && standardized_y)
    has_groups <- !is.null(x$group_number)
    has_ci <- FALSE
    ci_type <- NULL
    boot_type <- NULL
    if (is.numeric(x$group_number)) {
        has_group <- TRUE
      } else {
        has_group <- FALSE
      }
    if (isTRUE(!is.null(x$boot_ci))) {
        has_ci <- TRUE
        ci_type <- "boot"
        ci_name <- "boot_ci"
        se_name <- "boot_se"
        R <- length(x$boot_indirect)
        boot_type <- x$boot_type
        if (is.null(boot_type)) boot_type <- "perc"
      }
    if (isTRUE(!is.null(x$mc_ci))) {
        has_ci <- TRUE
        ci_type <- "mc"
        ci_name <- "mc_ci"
        se_name <- "mc_se"
        R <- length(x$mc_indirect)
      }
    has_w <- isTRUE(!is.null(wvalues))
    if (has_w) {
        w0 <- wvalues
        wnames <- names(w0)
      } else {
        w0 <- NA
        wnames <- NA
      }
    has_m <- isTRUE(!is.null(x$m))
    if (has_m) {
        m0 <- x$components
        m0c <- x$components_conditional
        mnames <- x$m
        if (is.list(m0)) {
            mpathnames <- lapply(m0, names)
          } else {
            mpathnames <- names(m0)
          }
      } else {
        m0 <- NA
        m0c <- NA
        mnames <- NA
        mpathnames <- NA
      }
    x0 <- x$x
    y0 <- x$y

    # Default to OLS or Wald SE
    se_out <- indirect_effect_original_se(xold,
                                          level = level)
    has_original_se <- !is.null(se_out)
    print_original_se <- FALSE
    if (!has_ci &&
        !has_m &&
        !has_groups &&
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
        if (!is.null(x$level)) {
            level <- x$level
          }
      }

    if (has_m) {
        if (is.list(mnames)) {
          path <- sapply(mnames, function(mm) {
                  if (is.null(mm)) {
                      out <- paste(x0, "->", y0)
                    } else {
                      out <- paste(x0, "->",
                                paste(eval(mm), collapse = " -> "),
                                "->", y0)
                    }
                  return(out)
                })
        } else {
          path <- paste(x0, "->",
                        paste(eval(mnames), collapse = " -> "),
                        "->", y0)
        }
      } else {
        path <- paste(x0, "->", y0)
      }
    if (has_group) {
        path <- paste0(x$group_label, "[",
                       x$group_number, "]: ",
                       path)
      }
    std_str <- ""
    if (standardized) {
        std_str <- paste0("(Both ", sQuote(x0),
                          " and ", sQuote(y0), " Standardized)")
      } else {
        if (standardized_x) {
            std_str <- paste0("(",sQuote(x0), " Standardized)")
          }
        if (standardized_y) {
            std_str <- paste0("(", sQuote(y0), " Standardized)")
          }
      }
    cond_str <- ""
    cond_str2 <- ""
    if (has_m) {
        cond_str <- "indirect"
        cond_str2 <- "Indirect"
      }
    if (has_w) {
        cat("\n== Conditional", cond_str2, "Effect",
            std_str, " ==")
      } else {
        cat("\n==", cond_str2, "Effect",
            std_str, "==")
      }
    cat("\n")
    ptable <- data.frame(Factor = "Path:", Value = path)
    if (has_w) {
      ptable <- rbind(ptable,
                      c("Moderators:", paste0(wnames, collapse = ", ")))
    }
    if (has_ci) {
        tmp1 <- switch(ci_type,
                    boot = " Bootstrap CI:",
                    mc = " Monte Carlo CI:")
        b_str1 <- paste0(formatC(x$level * 100, 1, format = "f"), "%",
                         tmp1)
        b_str2 <- paste0("[",
                         paste0(formatC(x[[ci_name]], digits, format = "f"),
                                collapse = " to "),
                         "]")
        b_str1b <- "Null Hypothesis Significant Test:"
        b_str2b <- ifelse((x[[ci_name]][1] > 0) || (x[[ci_name]][2] < 0),
                          paste0("Sig. (Level of Significance ",
                                formatC(1 - x$level, digits, format = "f"), ")"),
                          paste0("Not Sig. (Level of Significance ",
                                formatC(1 - x$level, digits, format = "f"), ")"))
        b_row <- c(b_str1, b_str2)
        if (isTRUE(ci_type == "boot") && pvalue) {
            tmpp <- ifelse(!is.null(x$boot_p) && is.numeric(x$boot_p),
                           formatC(x$boot_p, digits = pvalue_digits, format = "f"),
                           "Not available"
                           )
            b_row2 <- c("Bootstrap p-value:", tmpp)
          } else {
            b_row2 <- NULL
          }
        if (se) {
            tmpp <- ifelse(!is.null(x[[se_name]]) && is.numeric(x[[se_name]]),
                           formatC(x[[se_name]], digits = digits, format = "f"),
                           "Not available"
                           )
            tmp1 <- switch(ci_type,
                           boot = "Bootstrap SE:",
                           mc = "Monte Carlo SE:")
            b_row3 <- c(tmp1, tmpp)
          } else {
            b_row3 <- NULL
          }
      }
    if (has_w) {
        if (is.null(x$op)) {
            ptable <- rbind(ptable,
                            c(ifelse(has_m, "Conditional Indirect Effect:",
                                            "Conditional Effect:"),
                              formatC(x$indirect, digits = digits, format = "f")))
          } else {
            ptable <- rbind(ptable,
                            c("Function of Effects:",
                              formatC(x$indirect, digits = digits, format = "f")))
          }
        tmp <- paste(paste(wnames, "=", formatC(w0,
                                                digits = digits,
                                                format = "f")), collapse = ", ")
        if (has_ci) {ptable <- rbind(ptable, b_row, b_row2, b_row3)}
        ptable <- rbind(ptable,
                        c("When:", tmp))
      } else {
        if (is.null(x$op)) {
            ptable <- rbind(ptable,
                            c(ifelse(has_m, "Indirect Effect:", "Effect:"),
                              formatC(x$indirect, digits = digits, format = "f")))
          } else {
            ptable <- rbind(ptable,
                            c("Function of Effects:",
                              formatC(x$indirect, digits = digits, format = "f")))
          }
        if (has_ci) {ptable <- rbind(ptable, b_row, b_row2, b_row3)}
      }
    if (print_original_se) {
        if (!is.null(se_out$se) && se) {
            tmp1 <- data.frame(Factor = "Standard Error:",
                               Value = formatC(se_out$se,
                                               digits = digits,
                                               format = "f"))
            tmp2 <- data.frame(Factor = "Test Statistic:",
                               Value = formatC(se_out$stat,
                                               digits = digits,
                                               format = "f"))
            ptable <- rbind(ptable, tmp1, tmp2)
          }
        if (!is.null(se_out$se) &&
            se &&
            is.numeric(se_out$dfres)) {
            if (isFALSE(identical(se_out$dfres, Inf))) {
                tmp1 <- data.frame(Factor = "df (Residual)",
                                   Value = se_out$dfres)
                ptable <- rbind(ptable, tmp1)
              }
          }
        if (!is.null(se_out$pvalue) && pvalue) {
            tmp1 <- data.frame(Factor = "P-Value:",
                               Value = formatC(se_out$pvalue,
                                               digits = pvalue_digits,
                                               format = "f"))
            ptable <- rbind(ptable, tmp1)
          }
        if (!is.null(se_out$cilo) && se_ci) {
            tmp1 <- paste0("[",
                           paste0(formatC(se_out$cilo, digits, format = "f"),
                                  " to ",
                                  formatC(se_out$cihi, digits, format = "f")),
                           "]")
            str0 <- paste0(formatC(level * 100, 1, format = "f"), "%",
                           " Confidence Interval:")
            tmp2 <- data.frame(Factor = str0,
                               Value = tmp1)
            ptable <- rbind(ptable, tmp2)
          }
      }
    if (has_group) {
        # ptable <- rbind(ptable,
        #                 c("Group Label:", x$group_label))
        # ptable <- rbind(ptable,
        #                 c("Group Number:", x$group_number))
      }
    ptable <- data.frame(lapply(ptable, format))
    colnames(ptable) <- c("", "")
    print(ptable, row.names = FALSE)
    if (!is.null(x$op)) {
        cat("\nComputation of the Function of Effects:")
        cat("\n", x$op, "\n")
      }
    if (!is.null(x$computation_symbol)) {
        if (is.list(x$computation_symbol)) {
            # cat("\nComputation Formulas:")
            # cat("\n", paste(x$op, x$computation_symbol, collapse = "\n"))
          } else {
            cat("\nComputation Formula:")
            cat("\n ", x$computation_symbol)
          }
      }
    if (!is.null(x$computation_values)) {
        if (is.list(x$computation_values)) {
            # cat("\nComputation:")
            # cat("\n", paste(x$op, x$computation_values, collapse = "\n"))
          } else {
            cat("\nComputation:")
            cat("\n ", x$computation_values)
          }
      }
    if (has_ci) {
        cat("\n\n")
        tmp <- switch(ci_type,
                      boot = switch(boot_type,
                                    perc = "Percentile",
                                    bc = "Bias-corrected"),
                      mc = NULL)
        tmp1 <- switch(ci_type,
                  boot = paste(tmp, "confidence interval formed by nonparametric bootstrapping",
                          "with ",
                          R,
                          " bootstrap samples."),
                  mc = paste("Monte Carlo confidence interval",
                          "with ",
                          R,
                          " replications."))
        cat(strwrap(tmp1), sep = "\n")
        if (se) {
            tmp1 <- switch(ci_type,
                      boot = paste("Standard error (SE) based on nonparametric bootstrapping",
                              "with ",
                              R,
                              " bootstrap samples."),
                      mc = paste("Standard error (SE) based on Monte Carlo simulation",
                              "with ",
                              R,
                              " replications."))
            cat(strwrap(tmp1), sep = "\n")
          }
      }
    if (print_original_se) {
        cat("\n\n")
        tmp <- character(0)
        if (se) {
            tmp1 <- paste("- Standard error extracted or computed",
                          "from the 'lavaan' or regression output.")
            tmp2 <- paste("- Test statistic is t statistic for regression",
                          "and z statistic for 'lavaan'.")
            tmp <- c(tmp1, tmp2)
          }
        if (pvalue) {
            tmp1 <- paste("- P-value computed from the t statistic",
                          "or z statistic.")
            tmp <- c(tmp, tmp1)
          }
        if (se_ci) {
            tmp1 <- paste("- Confidendce interval formed from the t statistic",
                          "or z statistic.")
            tmp <- c(tmp, tmp1)
          }
        cat(strwrap(tmp, exdent = 2), sep = "\n")
      }
    print_note <- FALSE
    if (standardized_x ||
        standardized_y ||
        has_group) {
        print_note <- TRUE
      }
    note_str <- character(0)
    if (has_m && !is.list(mpathnames)) {
        if (has_w) {
          out <- data.frame(mpathnames, m0c, m0)
          rownames(out) <- NULL
          colnames(out) <- c("Path", "Conditional Effect", "Original Coefficient")
        } else {
          out <- data.frame(mpathnames, m0)
          rownames(out) <- NULL
          colnames(out) <- c("Path", "Coefficient")
        }
        cat("\nCoefficients of Component Paths:")
        cat("\n")
        print(out, digits = digits, row.names = FALSE)
        if (standardized_x || standardized_y) {
            note_str <- c(note_str,
                  strwrap("- The effects of the component paths are from the model, not standardized.",
                          exdent = 2))
            if (has_group) {
                note_str <- c(note_str,
                  strwrap("- SD(s) in the selected group is/are used in standardiziation.",
                          exdent = 2))
              }
          }
      }
    if (has_group) {
      note_str <- c(note_str,
          strwrap("- The group label is printed before each path.",
                  exdent = 2))
      note_str <- c(note_str,
          strwrap("- The group number in square brackets is the number used internally in lavaan.",
                  exdent = 2))
    }
    if (length(note_str) > 0) {
        cat("\nNOTE:\n")
        cat(note_str, sep = "\n")
      }
    cat("\n")
    invisible(x)
  }

#' @noRd

indirect_effect_original_se <- function(object,
                                        level = .95) {
    if (object$standardized_x ||
        object$standardized_y) {
        return(NULL)
      }
    if (!is.null(object$m)) {
        return(NULL)
      }
    if (is.null(object$original_se)) {
        return(NULL)
      }
    est <- object$indirect
    se <- object$original_se
    if (is.na(se)) {
        return(NULL)
      }
    dfres <- object$df_residual
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
    out <- list(se = se,
                stat = test_stat,
                pvalue = p,
                sig = sig,
                cilo = cilo,
                cihi = cihi,
                dfres = dfres)
    attr(out, "sig_legend") <- attr(sig, "legend")
    attr(out, "original_se_level") <- level
    return(out)
  }
