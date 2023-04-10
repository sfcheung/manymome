#' @title Print an 'indirect_list' Class
#' Object
#'
#' @description Print the content of the
#' output of [many_indirect_effects()].
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
#' @param ... Other arguments. Not used.
#'
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
                                annotation = TRUE, ...) {
    xold <- x
    my_call <- attr(x, "call")
    x_paths <- attr(x, "paths")
    path_names <- names(x_paths)
    wvalues <- x[[1]]$wvalues
    standardized_x <- x[[1]]$standardized_x
    standardized_y <- x[[1]]$standardized_y
    standardized <- (standardized_x && standardized_y)
    has_boot_ci <- isTRUE(!is.null(x[[1]]$boot_ci))
    level <- x[[1]]$level
    R <- ifelse(has_boot_ci,
                length(x[[1]]$boot_indirect),
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
    coef0 <- indirect_effects_from_list(xold, add_sig = TRUE)
    std_str <- ""
    if (standardized) {
        std_str <- paste0("(Both ", "x-variable(s)",
                          " and ", "y-variable(s)", " Standardized)")
      } else {
        if (standardized_x) {
            std_str <- paste0("(", "x-variable(s)", " Standardized)")
          }
        if (standardized_y) {
            std_str <- paste0("(", "y-variable(s)", " Standardized)")
          }
      }
    cond_str <- ""
    cond_str2 <- ""
    if (has_m) {
        cond_str <- "indirect"
        cond_str2 <- "Indirect"
      }
    if (has_w) {
        cat("\n== Conditional", cond_str2, "Effect(s)",
            std_str, " ==")
      } else {
        cat("\n== ", cond_str2, "Effect(s)",
            std_str, " ==")
      }
    cat("\n")

    coef1 <- data.frame(lapply(coef0, format_numeric, digits = digits))
    rownames(coef1) <- rownames(coef0)
    print(coef1)
    if (annotation) {
        if (has_boot_ci) {
            level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
            cat("\n ")
            cat(strwrap(paste("- [CI.lo to CI.hi] are",
                              level_str,
                              "percentile confidence intervals",
                              "by nonparametric bootstrapping with",
                              R,
                              "samples."), exdent = 3), sep = "\n")
          } else {
            cat("\n")
          }
        if (standardized_x && standardized_y) {
            cat(" - std: The standardized", cond_str, "effects.",
                sep = " ")
          }
        if (standardized_x && !standardized_y) {
            cat(" - std: The partially standardized", cond_str, "effects.",
                "\n -", "x-variable(s)", "standardized.",
                sep = " ")
          }
        if (!standardized_x && standardized_y) {
            cat(" - std: The partially standardized", cond_str, "effects.",
                "\n -", "y-variable(s)", "standardized.",
                sep = " ")
          }
        if (!standardized_x && !standardized_y) {
            cat(" - The 'ind' column shows the", cond_str, "effects.", sep = " ")
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
#' @return A data frame with the
#' indirect effect estimates and
#' confidence intervals (if available).
#' It also has A string column, `"Sig"`,
#' for #' significant test results
#' if `add_sig` is `TRUE` and bootstrap
#' confidence intervals are available.
#'
#' @param object The output of
#' [indirect_effect()] or
#' [cond_indirect()].
#'
#' @param add_sig Whether a column
#' of significance test results
#' will be added. Default is `TRUE`.
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

indirect_effects_from_list <- function(object, add_sig = TRUE) {
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
    if (standardized_x || standardized_x) {
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
      }
    if (isTRUE(!is.null(object[[1]]$mc_ci))) {
        has_ci <- TRUE
        ci_type <- "mc"
      }
    if (has_ci) {
        ci0 <- t(sapply(object, stats::confint))
        out$CI.lo <- ci0[, 1]
        out$CI.hi <- ci0[, 2]
        if (add_sig) {
            Sig <- ifelse((out$CI.lo > 0) | (out$CI.hi < 0), "Sig", "")
            out$Sig <- Sig
          }
      }
    out
  }