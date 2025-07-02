#' @title Print an 'indirect_list' Class
#' Object
#'
#' @description Print the content of the
#' output of [many_indirect_effects()].
#'
#' @details The `print` method of the
#' `indirect_list`-class object.
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
#' @return `x` is returned invisibly.
#' Called for its side effect.
#'
#' @param x The output of
#' [many_indirect_effects()].
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
#' available.
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
#'
#' @param for_each_path Logical. If
#' `TRUE`, each of the paths will be
#' printed individually, using the
#' `print`-method of the output of
#' [indirect_effect()]. Default is
#' `FALSE`.
#'
#' @param ... Other arguments. If
#' `for_each_path` is `TRUE`, they
#' will be passed to the print method
#' of the output of [indirect_effect()].
#' Ignored otherwise.
#'
#'
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#'
#' @seealso [many_indirect_effects()]
#'
#' @examples
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
#'
#' @export

print.indirect_list <- function(x, digits = 3,
                                annotation = TRUE,
                                pvalue = FALSE,
                                pvalue_digits = 3,
                                se = FALSE,
                                for_each_path = FALSE,
                                ...) {
    if (for_each_path) {
        section_sep <- paste0(rep("-",
                              round(getOption("width") * .80)),
                              collapse = "")
        for (xx in x) {
            cat(section_sep, "\n")
            print(xx,
                  digits = digits,
                  pvalue = pvalue,
                  pvalue_digits = pvalue_digits,
                  se = se,
                  ...)
          }
        return(invisible(x))
      }
    xold <- x
    my_call <- attr(x, "call")
    x_paths <- attr(x, "paths")
    path_names <- names(x_paths)
    wvalues <- x[[1]]$wvalues
    standardized_x <- x[[1]]$standardized_x
    standardized_y <- x[[1]]$standardized_y
    standardized <- (standardized_x && standardized_y)
    has_ci <- FALSE
    ci_type <- NULL
    boot_type <- NULL
    if (isTRUE(!is.null(x[[1]]$boot_ci))) {
        has_ci <- TRUE
        ci_type <- "boot"
        ind_name <- "boot_indirect"
        boot_type <- x[[1]]$boot_type
        if (is.null(boot_type)) boot_type <- "perc"
      }
    if (isTRUE(!is.null(x[[1]]$mc_ci))) {
        has_ci <- TRUE
        ci_type <- "mc"
        ind_name <- "mc_indirect"
      }
    level <- x[[1]]$level
    R <- ifelse(has_ci,
                length(x[[1]][[ind_name]]),
                NA)
    # Does not support conditional indirect effects for now
    # has_w <- isTRUE(!is.null(wvalues))
    has_w <- FALSE
    if (has_w) {
        w0 <- wvalues
        wnames <- names(w0)
      } else {
        w0 <- NA
        wnames <- NA
      }
    # Should always have mediators
    has_m <- TRUE
    all_m_null <- sapply(x,
                         function(xx) {is.null(xx$m)})
    if (all(all_m_null)) has_m <- FALSE
    coef0 <- indirect_effects_from_list(xold,
                                        add_sig = TRUE,
                                        pvalue = pvalue,
                                        se = se)
    if (has_ci && (ci_type == "boot") && pvalue) {
        coef0$pvalue <- formatC(as.numeric(coef0$pvalue),
                                digits = pvalue_digits,
                                format = "f")
      }
    if (has_ci && se) {
        coef0$SE <- formatC(as.numeric(coef0$SE),
                            digits = digits,
                            format = "f")
      }
    std_str <- ""
    if (standardized) {
        std_str <- paste0(" (Both ", "x-variable(s)",
                          " and ", "y-variable(s)", " Standardized)")
      } else {
        if (standardized_x) {
            std_str <- paste0(" (", "x-variable(s)", " Standardized)")
          }
        if (standardized_y) {
            std_str <- paste0(" (", "y-variable(s)", " Standardized)")
          }
      }
    cond_str <- "effect(s)"
    cond_str2 <- "Effect(s)"
    if (has_m) {
        cond_str <- "indirect effect(s)"
        cond_str2 <- "Indirect Effect(s)"
      }
    if (has_w) {
        cat("\n== Conditional ", cond_str2,
            std_str, " ==", sep = "")
      } else {
        cat("\n== ", cond_str2,
            std_str, " ==", sep = "")
      }
    cat("\n\n")

    coef1 <- data.frame(lapply(coef0, format_numeric, digits = digits))
    rownames(coef1) <- rownames(coef0)
    print(coef1)
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
                      boot = paste(tmp, "confidence intervals",
                                   "by nonparametric bootstrapping with"),
                      mc = "Monte Carlo confidence intervals with")
            tmp2 <- switch(ci_type,
                      boot = paste(R, "samples."),
                      mc = paste(R, "replications."))
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
          } else if (has_ci && ci_type == "mc") {
            level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
            cat("\n ")
            cat(strwrap(paste("- [CI.lo to CI.hi] are",
                              level_str,
                              "Monte Carlo confidence intervals",
                              "with",
                              R,
                              "replications."), exdent = 3), sep = "\n")
          } else {
            cat("\n")
          }
        if (standardized_x && standardized_y) {
            cat(" - std: The standardized ", cond_str, ".",
                sep = "")
          }
        if (standardized_x && !standardized_y) {
            cat(" - std: The partially standardized ", cond_str, ".",
                "\n - x-variable(s) standardized.",
                sep = "")
          }
        if (!standardized_x && standardized_y) {
            cat(" - std: The partially standardized ", cond_str, ".",
                "\n - y-variable(s) standardized.",
                sep = "")
          }
        if (!standardized_x && !standardized_y) {
            cat(" - The 'ind' column shows the", cond_str, ".", sep = "")
          }
        cat("\n ")
      }
    invisible(x)
  }

#' @title Coefficient Table of an 'indirect_list' Class
#' Object
#'
#' @description Create a coefficient table
#' for the point estimates and
#' confidence intervals (if available)
#' in the
#' output of [many_indirect_effects()].
#'
#' @details
#' If bootstrapping confidence interval
#' was requested, this method has the
#' option to add
#' *p*-values computed by the
#' method presented in Asparouhov and Muthén (2021).
#' Note that these *p*-values is asymmetric
#' bootstrap *p*-values based on the
#' distribution of the bootstrap estimates.
#' They are not computed based on the
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
#'
#' @return A data frame with the
#' indirect effect estimates and
#' confidence intervals (if available).
#' It also has A string column, `"Sig"`,
#' for #' significant test results
#' if `add_sig` is `TRUE` and
#' confidence intervals are available.
#'
#' @param object The output of
#' [many_indirect_effects()] or other
#' functions that return an object
#' of the class `indirect_list`.
#'
#' @param add_sig Whether a column
#' of significance test results
#' will be added. Default is `TRUE`.
#'
#' @param pvalue Logical. If `TRUE`,
#' asymmetric *p*-values based on
#' bootstrapping will be added
#' available. Default is `FALSE`.
#'
#' @param se Logical. If `TRUE` and
#' confidence intervals are available,
#' the standard errors of the estimates
#' are also added. They are simply the
#' standard deviations of the bootstrap
#' estimates or Monte Carlo simulated
#' values, depending on the method used
#' to form the confidence intervals.
#'
#' @seealso [many_indirect_effects()]
#'
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' @examples
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
#' # Create a data frame of the indirect effect estimates
#'
#' out_df <- indirect_effects_from_list(out)
#' out_df
#'
#'
#' @export

indirect_effects_from_list <- function(object,
                                       add_sig = TRUE,
                                       pvalue = FALSE,
                                       se = FALSE) {
    if (!inherits(object, "indirect_list")) {
        stop("object is not an indirect_list class object.")
      }
    my_call <- attr(object, "call")
    x_paths <- attr(object, "paths")
    path_names <- names(x_paths)
    wvalues <- object[[1]]$wvalues
    standardized_x <- object[[1]]$standardized_x
    standardized_y <- object[[1]]$standardized_y
    standardized <- (standardized_x && standardized_y)
    coef0 <- sapply(object, stats::coef)
    if (standardized_x || standardized_y) {
        out <- data.frame(std = coef0)
      } else {
        out <- data.frame(ind = coef0)
      }
    rownames(out) <- path_names
    has_ci <- FALSE
    ci_type <- NULL
    if (isTRUE(!is.null(object[[1]]$boot_ci))) {
        has_ci <- TRUE
        ci_type <- "boot"
        se_name <- "boot_se"
      }
    if (isTRUE(!is.null(object[[1]]$mc_ci))) {
        has_ci <- TRUE
        ci_type <- "mc"
        se_name <- "mc_se"
      }
    if (has_ci) {
        ci0 <- t(sapply(object, stats::confint))
        out$CI.lo <- ci0[, 1]
        out$CI.hi <- ci0[, 2]
        if (add_sig) {
            Sig <- ifelse((out$CI.lo > 0) | (out$CI.hi < 0), "Sig", "")
            out$Sig <- Sig
          }
        if (pvalue && (ci_type == "boot")) {
            boot_p <- sapply(object, function(x) x$boot_p)
            boot_p <- unname(boot_p)
            out$pvalue <- boot_p
          }
        if (se) {
            est_se <- sapply(object, function(x) x[[se_name]])
            est_se <- unname(est_se)
            out$SE <- est_se
          }
      }
    out
  }