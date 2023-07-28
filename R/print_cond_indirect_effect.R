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
#' available. Default is `FALSE.`
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
                                        pvalue = FALSE,
                                        pvalue_digits = 3,
                                        se = FALSE,
                                        ...) {
  full_output <- attr(x, "full_output")
  x_i <- full_output[[1]]
  my_call <- attr(x, "call")
  cc_call <- x_i$cond_indirect_call
  boot_ci <- !is.null(x_i$boot_ci)
  has_ci <- FALSE
  ci_type <- NULL
  if (!is.null(x_i$boot_ci)) {
      has_ci <- TRUE
      ci_type <- "boot"
      ind_name <- "boot_indirect"
      se_name <- "boot_se"
    }
  if (!is.null(x_i$mc_ci)) {
      has_ci <- TRUE
      ci_type <- "mc"
      ind_name <- "mc_indirect"
      se_name <- "mc_se"
    }
  standardized_x <- x_i$standardized_x
  standardized_y <- x_i$standardized_y
  level <- x_i$level
  has_m <- isTRUE(!is.null(x_i$m))
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
  out1 <- data.frame(out, check.names = FALSE)
  wlevels <- attr(x, "wlevels")
  w0 <- colnames(attr(wlevels, "wlevels"))
  w1 <- colnames(wlevels)
  mcond <- names(x_i$components)
  cond_str <- ""
  cond_str2 <- ""
  if (has_m) {
      cond_str <- "indirect"
      cond_str2 <- "Indirect"
    }
  if (has_m) {
      cat("\n== Conditional indirect effects ==\n")
    } else {
      cat("\n== Conditional effects ==\n")
    }
  cat("\n Path:", path)
  cat("\n Conditional on moderator(s):", paste0(w0, collapse = ", "))
  cat("\n Moderator(s) represented by:", paste0(w1, collapse = ", "))
  xold <- x
  x <- out1
  cat("\n\n")
  NextMethod()
  if (annotation) {
      if (has_ci) {
          level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
          cat("\n ")
          tmp1 <- switch(ci_type,
                    boot = "percentile confidence intervals by nonparametric bootstrapping",
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
      cat(strwrap(paste("\n -", paste(sQuote(mcond), collapse = ","),
          "is/are the path coefficient(s) along the path",
          "conditional on the moderators."), exdent = 3), sep = "\n")
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
