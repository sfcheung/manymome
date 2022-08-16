#' @title Create Dummy Variables
#'
#' @description Create dummy variables from a categorical
#'  variable.
#'
#' @details Its main use is for creating dummy variables
#'  (indicator variables) from a categorical variable,
#'  to be used in [lavaan::sem()].
#'
#' Optionally, the other contrasts can be used through
#' the argument `x_contrasts`.
#'
#' @return
#' It always returns a matrix with the number of rows
#' equal to the length of the vector (`x_value`). If
#' the categorical has only two categories and so
#' only one dummy variable is needed, the output is
#' still a one-column "matrix" in R.
#'
#' @param x_value The vector of categorical variable.
#' @param x_contrasts The contrast to be used. Default
#'                    is `"constr.treatment"`.
#' @param prefix The prefix to be added to the variables
#'               to be created. Default is `""`.
#' @param add_rownames Whether row names will be added
#'                     to the output. Default is `TRUE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' dat <- data_mod_cat
#' dat <- data.frame(dat,
#'                   factor2var(dat$w, prefix = "gp", add_rownames = FALSE))
#' head(dat[, c("w", "gpgroup2", "gpgroup3")], 15)
#'
#' @export
#'

factor2var <- function(x_value,
                       x_contrasts = "contr.treatment",
                       prefix = "",
                       add_rownames = TRUE) {
    x_fac <- factor(x_value)
    stats::contrasts(x_fac) <- x_contrasts
    m <- do.call(x_contrasts, list(n = levels(x_fac)))
    mj <- ncol(m)
    mna <- rep(NA, mj)
    out <- t(sapply(as.character(x_value), function(x) {
                                if (is.na(x)) {
                                    xx <- mna
                                  } else {
                                    xx <- m[x, ]
                                  }
                                xx
                              }))
    if (!is.matrix(out)) {
        out <- matrix(out, ncol = 1)
      }
    colnames(out) <- colnames(m)
    colnames(out) <- paste0(prefix, colnames(out))
    if (!add_rownames) rownames(out) <- NULL
    out
  }