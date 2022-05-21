#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param x_value The vector of categorical variable.
#' @param x_contrasts The contrast to be used. Default
#'                    is `constr.treatment`.
#' @param prefix The prefix to be added to the variables
#'               to be created. Default is `""`.
#' @param add_rownames Whether row names will be added
#'                     to the output. Default is `FALSE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' dat <- modmed_x1m3w4y1
#' dat <- data.frame(dat,
#'                   factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
#' head(dat[, c(10, 12, 13)], 15)
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
    out <- t(sapply(x_value, function(x) m[x, ]))
    if (!is.matrix(out)) {
        out <- matrix(out, ncol = 1)
      }
    colnames(out) <- paste0(prefix, colnames(out))
    if (!add_rownames) rownames(out) <- NULL
    out
  }