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
#' m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
#' m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
#' m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
#' y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
#'
#' indirect_1 <- indirect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
#'                        wvalues = wvalues)
#' indirect_2 <- (est[est$label == "a1", "est"] +
#'                 wvalues["w1"] * est[est$label == "d1", "est"]) *
#'               (est[est$label == "a2", "est"] +
#'                 wvalues["w2"] * est[est$label == "d2", "est"]) *
#'               (est[est$label == "a3", "est"] +
#'                 wvalues["w3"] * est[est$label == "d3", "est"]) *
#'               (est[est$label == "a4", "est"] +
#'                 wvalues["w4"] * est[est$label == "d4", "est"])
#' indirect_1$indirect
#' indirect_2
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
  R <- ifelse(boot_ci, length(x_i$boot_indirect),
                       NA)
  x0 <- my_call$x
  y0 <- my_call$y
  m0 <- my_call$m
  path <- paste0(x0, " -> ", paste0(m0, collapse = " -> "), " -> ", y0)
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
  cat("\n== Conditional indirect effects ==\n")
  cat("\n Path:", path)
  cat("\n Conditioned on moderator(s):", paste0(w0, collapse = ", "))
  cat("\n Moderator(s) represented by:", paste0(w1, collapse = ", "))
  xold <- x
  x <- out1
  cat("\n\n")
  NextMethod()
  if (boot_ci) {
      level_str <- paste0(formatC(level * 100, 1, format = "f"), "%")
      cat(strwrap(paste("\n - [CI.lo to CI.hi] are",
                        level_str,
                        "percentile confidence intervals",
                        "by nonparametric bootstrapping with",
                        R,
                        "samples."), exdent = 2), sep = "\n")
    }
  if (standardized_x & standardized_y) {
      cat("\n - std: The standardized indirect effects.",
          "\n - ind: The unstandardized indirect effects.")
    }
  if (standardized_x & !standardized_y) {
      cat("\n - std: The partially standardized indirect effects.",
          "\n - ", sQuote(x0), " is standardized.",
          "\n - ind: The unstandardized indirect effects.", sep = "")
    }
  if (!standardized_x & standardized_y) {
      cat("\n - std: Te partially standardized indirect effects.",
          "\n - ", sQuote(y0), " is standardized.",
          "\n - ind: The unstandardized indirect effects.", sep = "")
    }
  if (!standardized_x & !standardized_y) {
      cat("\n - The 'ind' column shows the indirect effects.", sep = "")
    }
  cat("\n -", paste(sQuote(mcond), collapse = ","),
      "is/are the path coefficient(s) along the path",
      "conditional on the moderators.")
  cat("\n")
  invisible(x)
}

format_numeric <- function(xi, digits = 3, check_integer = TRUE) {
    if (!is.numeric(xi)) return(format(xi))
    if (isTRUE(all.equal(round(xi), xi))) return(round(xi, 0))
    xi <- formatC(xi, digits = digits, format = "f")
    xi
  }
