#' @title Print a 'delta_med' Class Object
#'
#' @description Print the content of
#' a `delta_med`-class object.
#'
#' @details It prints the output of
#' `delta_med()`, which is a
#' `delta_med`-class object.
#'
#' @return
#' `x` is returned invisibly. Called
#' for its side effect.
#'
#' @param x A `delta_med`-class object.
#'
#' @param digits The number of digits
#' after the decimal. Default is 3.
#'
#' @param level The level of confidence
#' of bootstrap confidence interval,
#' if requested when created. If `NULL`,
#' the default, the level requested when
#' calling [delta_med()] is used. If
#' not null, then this level will be
#' used.
#'
#' @param full Logical. Whether
#' additional information will be printed.
#' Default is `FALSE`.
#'
#' @param ...  Optional arguments.
#' Ignored.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [delta_med()]
#'
#' @examples
#'
#' # TODO: To prepare
#' x <- 123
#' print(x)
#'
#' @export
print.delta_med <- function(x,
                            digits = 3,
                            level = NULL,
                            full = FALSE,
                            ...) {
    x_call <- x$call
    call_x <- x_call$x
    call_m <- x_call$m
    call_y <- x_call$y
    dm <- x$delta_med
    if (!is.null(x$boot_ci)) {
        has_boot_ci <- TRUE
      } else {
        has_boot_ci <- FALSE
      }
    if (has_boot_ci) {
        dm_boot <- x$boot_est
        R <- length(stats::na.omit(dm_boot))
        if (!is.null(level)) {
            dm_boot_out <- form_boot_ci(est = dm,
                                        boot_est = dm_boot,
                                        level = level)
            dm_boot_ci <- dm_boot_out$boot_ci
            dm_boot_p <- dm_boot_out$boot_p
            dm_boot_se <- dm_boot_out$boot_se
          } else {
            level <- x$level
            dm_boot_ci <- x$boot_ci
            dm_boot_p <- x$boot_p
            dm_boot_se <- x$boot_se
          }
      } else {
        dm_boot <- NULL
        R <- NULL
        dm_boot_ci <- NULL
        dm_boot_p <- NULL
        dm_boot_se <- NULL
      }
    cat("Call:\n")
    print(x_call)
    cat("\n")
    cat("Predictor (x)       :", call_x, "\n")
    cat("Mediator(s) (m)     :", paste0(call_m, collapse = ", "), "\n")
    cat("Outcome variable (y):", call_y, "\n")
    cat("\n")
    tmp1 <- "Delta_med"
    tmp2 <- formatC(dm, digits = digits, format = "f")
    if (has_boot_ci) {
        tmp1 <- c(tmp1,
                  paste0(formatC(level*100, digits = 1, format = "f"),
                         "% Bootstrap confidence interval",
                         collapse = ""))
        tmp2 <- c(tmp2,
                  paste0("[",
                        paste(formatC(dm_boot_ci,
                                      digits = digits,
                                      format = "f"),
                              collapse = ", "),
                         "]", collapse = ""))
        tmp1 <- c(tmp1,
                  "Number of bootstrap samples")
        tmp2 <- c(tmp2, R)
      }
    out_df <- paste0(add_whitespace(tmp1, mode = "right"),
                     ": ",
                     add_whitespace(tmp2, mode = "left"))
    cat(out_df, sep = "\n")
    cat("\n")
    cat("Paths removed:\n")
    cat(paste0(" ", x$paths_removed, collapse = "\n"))
    cat("\n")
    if (full) {
        tmp <- c("R-sq: Original",
                 "R-sq: Mediator(s) removed",
                 "Variance of y",
                 "Variance of predicted y",
                 "Variance of predicted: mediator(s) removed")
        tmp2 <- add_whitespace(tmp, mode = "right")
        tmp2 <- paste0(tmp2, ": ")
        tmp3 <- format(c(x$rsq_full,
                         x$rsq_no_mediators,
                         x$var_y,
                         x$var_predicted_full,
                         x$var_predicted_no_mediators),
                       digits = digits,
                       format = "f")
        tmp4 <- add_whitespace(tmp3, mode = "left")
        out_df <- paste0(tmp2, tmp4)
        cat("\n")
        cat("Additional information:\n")
        cat(out_df, sep = "\n")
      }
    invisible(x)
  }