#' @title Summary of an `lm_list`-Class
#' Object
#'
#' @description The summary of content
#' of the output of [lm2list()].
#'
#' @return [summary.lm_list()] returns a
#' `summary_lm_list`-class object, which
#' is a list of the [summary()] outputs
#' of the [lm()] outputs stored.
#'
#' [print.summary_lm_list()] returns `x`
#' invisibly. Called for its side
#' effect.
#'
#' @param object The output of
#' [lm2list()].
#'
#' @param betaselect If `TRUE`,
#' standardized coefficients are
#' computed and included in the
#' printout. Only numeric variables will
#' be computed, and any derived terms,
#' such as product terms, will be formed
#' *after* standardization. Default
#' is `FALSE`.
#'
#' @param ci If `TRUE`, confidence
#' interval based on *t* statistic
#' and standard error will be computed
#' and added to the output. Default is
#' `FALSE`.
#'
#' @param level The level of confidence
#' of the confidence interval. Ignored
#' if `ci` is not `TRUE`.
#'
#' @param x An object of class
#' `summary_lm_list`.
#'
#' @param digits The number of
#' significant digits in printing
#' numerical results.
#'
#' @param digits_decimal The number of
#' digits after the decimal in printing
#' numerical results. Default is `NULL`.
#' If set to an integer, numerical
#' results in the coefficient table
#' will be printed according this setting,
#' and `digits` will be ignored.
#'
#' @param ...  Other arguments. Not
#' used.
#'
#'
#'
#' @examples
#'
#' data(data_serial_parallel)
#' lm_m11 <- lm(m11 ~ x + c1 + c2, data_serial_parallel)
#' lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, data_serial_parallel)
#' lm_m2 <- lm(m2 ~ x + c1 + c2, data_serial_parallel)
#' lm_y <- lm(y ~ m11 + m12 + m2 + x + c1 + c2, data_serial_parallel)
#' # Join them to form a lm_list-class object
#' lm_serial_parallel <- lm2list(lm_m11, lm_m12, lm_m2, lm_y)
#' lm_serial_parallel
#' summary(lm_serial_parallel)
#'
#' @export

summary.lm_list <- function(object,
                            betaselect = FALSE,
                            ci = FALSE,
                            level = .95,
                            ...) {
    out <- lapply(object, summary)
    if (betaselect) {
      out_beta <- lapply(object,
                         std_numeric)
      out <- mapply(function(xx,
                             yy) {
                      xx$betaselect <- yy
                      xx
                    },
                    xx = out,
                    yy = out_beta,
                    SIMPLIFY = FALSE)
      out <- lapply(out, function(xx,
                                  level) {
                    xx$coefficients <- summary_lm_add_beta(xx)
                    xx
                  })
    }
    if (ci) {
      out <- lapply(out, function(xx,
                                  level) {
                    xx$coefficients <- summary_lm_add_ci(xx,
                                         level = level)
                    xx$ci_level <- level
                    xx
                  },
                  level = level)
    }
    class(out) <- c("summary_lm_list", class(out))
    out
  }

#' @describeIn summary.lm_list Print
#' method for output of summary for
#' lm_list.
#'
#' @export

print.summary_lm_list <- function(x,
                                  digits = 3,
                                  digits_decimal = NULL,
                                  ...) {
    for (xi in x) {
        cat("\n\nModel:\n")
        print(xi$call$formula)
        if (is.null(digits_decimal)) {
          stats::printCoefmat(xi$coefficients,
                              digits = digits,
                              ...)
        } else {
          # Adapted from stats::printCoefmat
          sig <- stats::symnum(xi$coefficients[, "Pr(>|t|)", drop = TRUE],
                              corr = FALSE,
                              na = FALSE,
                              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                              symbols = c("***", "**", "*", ".", " "))
          coef_tmp <- lapply(as.data.frame(xi$coefficients),
                            format_numeric,
                            digits = digits_decimal)
          coef_tmp <- data.frame(coef_tmp,
                                 Sig = sig,
                                 check.names = FALSE)
          rownames(coef_tmp) <- rownames(xi$coefficients)
          print(coef_tmp)
          cat("Signif. codes:  ",
              attr(sig, "legend"), "\n")
        }
        if (!is.null(xi$betaselect)) {
          vars_std <- attr(xi$betaselect, "standardized")
          tmp <- strwrap(paste0("- BetaS are standardized coefficients with (a) ",
                                "only numeric variables standardized and (b) ",
                                "product terms formed after standardization. ",
                                "Variable(s) standardized is/are: ",
                                paste0(vars_std, collapse = ", ")),
                         exdent = 2)
          cat(tmp,
              sep = "\n")
        }
        if (!is.null(xi$ci_level)) {
          tmp0 <- paste0(formatC(xi$ci_level * 100,
                                 digits = 1,
                                 format = "f"),
                         "%")
          tmp <- strwrap(paste0("- CI.lo and CI.hi are the ", tmp0,
                                " confidence levels of 'Estimate' ",
                                "computed from the t values and ",
                                "standard errors."),
                         exdent = 2)
          cat(tmp,
              sep = "\n")
        }
        rsq0 <- formatC(xi$r.squared, digits = digits, format = "f")
        adjrsq0 <- formatC(xi$adj.r.squared, digits = digits, format = "f")
        f0 <- paste0("F(", round(xi$fstatistic["numdf"]),
                     ", ", xi$fstatistic["dendf"],
                     ") = ",
                     formatC(xi$fstatistic["value"], digits = digits, format = "f"))
        p1 <- stats::pf(xi$fstatistic["value"],
                        xi$fstatistic["numdf"],
                        xi$fstatistic["dendf"], lower.tail = FALSE)
        p0 <- ifelse(p1 < .001,
                     "p < .001",
                     paste0("p = ", formatC(p1, digits = digits, format = "f")))
        fstr <- paste0(f0, ", ", p0)
        tmp <- paste0("R-square = ", rsq0,
                      ". Adjusted R-square = ", adjrsq0,
                      ". ", fstr)
        cat(tmp)
      }
    cat("\n")
    invisible(x)
  }

#' @noRd

summary_lm_add_ci <- function(object,
                              level = .95) {
  coef_mat <- object$coefficients
  if (("Std. Error" %in% colnames(coef_mat))) {
    t_values <- coef_mat[, "t value", drop = TRUE]
    est <- coef_mat[, "Estimate", drop = TRUE]
    se <- coef_mat[, "Std. Error", drop = TRUE]
    dfres <- object$df[2]
    z_crit <- -1 * stats::qt((1 - level) / 2,
                             df = dfres,
                             lower.tail = TRUE)
    cilo <- est - z_crit * se
    cihi <- est + z_crit * se

    # Borrowed from stats::confint()
    probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
    cnames <- paste0(format(100 * probs,
                            trim = TRUE,
                            scientific = FALSE,
                            digits = 2), "%")
    cnames <- paste(c("CI.lo", "CI.hi"),
                    cnames)
    cnames <- c("CI.lo", "CI.hi")
    ci_df <- cbind(cilo, cihi)
    colnames(ci_df) <- cnames
    i <- match("Estimate", colnames(coef_mat))
    out <- cbind(coef_mat[, i, drop = FALSE],
                 ci_df,
                 coef_mat[, seq(i + 1, ncol(coef_mat))])
    rownames(out) <- rownames(coef_mat)
    return(out)
  } else {
    return(out)
  }
}

#' @noRd

summary_lm_add_beta <- function(object) {
  if (!isTRUE(length(object$betaselect) > 0)) {
    return(object)
  }
  betas <- object$betaselect
  coef_mat <- object$coefficients
  i <- match(c("Std. Error", "t value", "Pr(>|t|)"),
             colnames(coef_mat))
  i <- i[!is.na(i)][1]
  if (length(i) == 0) {
    i <- match("Estimate", colnames(coef_mat)) + 1
    if (is.na(i)) {
      # No valid columns. Something's wrong. Don't add.
      return(object)
    }
  }
  # betas_df <- data.frame(betaS = betas)
  out <- cbind(coef_mat[, i - 1, drop = FALSE],
               betaS = betas,
               coef_mat[, seq(i, ncol(coef_mat))])
  rownames(out) <- rownames(coef_mat)
  return(out)
}


#' @noRd
std_numeric <- function(object) {
  # Copied from stdmod

  dat <- object$model
  k <- ncol(dat)

  std_f <- stats::as.formula(~ .)
  std_terms <- attr(stats::terms(std_f,
                                 data = dat),
                                 "term.labels")

  # Prepare for Centering

  var_means <- suppressWarnings(sapply(dat,
                                       mean))
  var_a <- rep(0, k)
  names(var_a) <- colnames(dat)
  var_a[std_terms] <- var_means[std_terms]

  # Prepare for Scaling

  var_sds <- suppressWarnings(sapply(dat,
                                     sd2))
  var_b <- rep(1, k)
  names(var_b) <- colnames(dat)
  var_b[std_terms] <- var_sds[std_terms]

  # Do centering and scaling

  dat_mod <- sweep2(dat, var_a, FUN = "-")
  dat_mod <- sweep2(dat_mod, var_b, FUN = "/")

  lm_out_mod <- stats::update(object,
                              data = dat_mod)
  out <- stats::coef(lm_out_mod)
  stded <- names(var_a)[!is.na(var_a)]
  attr(out, "standardized") <- stded
  return(out)
}

# Copied from stdmod
#' Similar to sweep, but will skip columns that are not numeric.
#' It sweep only on the columns.
#' @param x The data frame.
#' @param values A named vector with length equal to the number of columns
#'        in data.
#' @param FUN The function to be applied.
#' @noRd

sweep2 <- function(x,
                   values,
                   FUN = "-") {
    FUN <- match.fun(FUN)
    k <- ncol(x)
    n <- nrow(x)
    fct <- function(x,
                    values,
                    FUN) {
        if(is.numeric(x)) {
            out <- FUN(x,
                       values)
          } else {
            out <- x
          }
        out
      }
    out <- mapply(fct,
                  x,
                  values,
                  MoreArgs = list(FUN = FUN),
                  SIMPLIFY = FALSE)
    data.frame(out,
               stringsAsFactors = FALSE)
  }

#' @noRd
# Copied from stdmod
# Return SD only for numeric vector
sd2 <- function(x) {
    if (!is.numeric(x)) {
        return(NA)
      } else {
        return(stats::sd(x))
      }
  }
