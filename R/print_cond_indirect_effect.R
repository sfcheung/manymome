#' @title Print the a 'cond_indirect_effects' Class Object
#'
#' @description Print the content of the output of [cond_indirect_effects()]
#'
#' @return
#'  `x` is returned invisibly.
#'
#' @param x The output of [cond_indirect_effects()].
#' @param digits Number of digits to display. Default is 3.
#' @param ...  Other arguments. Not used.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#'
#' # Examples for cond_indirect_effects():
#'
#' # Create levels of w1, the moderators
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = w1levels, fit = fit)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the levels
#' cond_indirect_effects(x = "x", y = "y", m = "m1",
#'                       wlevels = w1levels, fit = fit)
#'
#'
#' @export

print.cond_indirect_effects <- function(x, digits = 3, ...) {
  full_output <- attr(x, "full_output")
  x_i <- full_output[[1]]
  my_call <- attr(x, "call")
  cc_call <- x_i$cond_indirect_call
  boot_ci <- !is.null(x_i$boot_ci)
  standardized_x <- x_i$standardized_x
  standardized_y <- x_i$standardized_y
  level <- x_i$level
  has_m <- isTRUE(!is.null(x_i$m))
  R <- ifelse(boot_ci, length(x_i$boot_indirect),
                       NA)
  x0 <- my_call$x
  y0 <- my_call$y
  m0 <- my_call$m
  if (has_m) {
      path <- path <- paste0(x0, " -> ",
                             paste0(m0, collapse = " -> "),
                             " -> ", y0)
    } else {
      path <- paste(x0, "->", y0)
    }
  out <- lapply(x, format_numeric, digits = digits)
  if (boot_ci) {
      Sig <- ifelse((x$CI.lo > 0) | (x$CI.hi < 0), "Sig", "")
      i <- which(names(out) == "CI.hi")
      j <- length(out)
      out <- c(out[1:i], list(Sig = Sig), out[(i + 1):j])
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
  cat("\n Conditioned on moderator(s):", paste0(w0, collapse = ", "))
  cat("\n Moderator(s) represented by:", paste0(w1, collapse = ", "))
  xold <- x
  x <- out1
  cat("\n\n")
  NextMethod()
  if (boot_ci) {
      level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
      cat("\n ")
      cat(strwrap(paste("- [CI.lo to CI.hi] are",
                        level_str,
                        "percentile confidence intervals",
                        "by nonparametric bootstrapping with",
                        R,
                        "samples."), exdent = 2), sep = "\n")
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
      "conditional on the moderators."), exdent = 2), sep = "\n")
  cat("\n")
  invisible(x)
}

format_numeric <- function(xi, digits = 3, check_integer = TRUE) {
    if (!is.numeric(xi)) return(format(xi))
    if (isTRUE(all.equal(round(xi), xi))) return(round(xi, 0))
    xi <- formatC(xi, digits = digits, format = "f")
    xi
  }
