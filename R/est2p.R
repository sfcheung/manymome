#' @title Asymmetric Bootstrap p-Value
#'
#' @description Compute the asymmetric
#' bootstrap *p*-value from a vector of
#' bootstrap estimates.
#'
#' @details It computes the *p*-value
#' based on the method presented in
#' Asparouhov and Muthén (2021).
#'
#' @return
#' Numeric. The *p*-value. `NA` if
#' it cannot be computed.
#'
#' @param x A numeric vector. The
#' bootstrap estimates.
#'
#' @param h0 The value for the null
#' hypothesis. Default is zero.
#'
#' @param min_size Integer. The
#' bootstrap *p*-value will be computed
#' only if `x` has at least `min_size`
#' valid values.
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' @examples
#' x1 <- rnorm(n, 2, 4)
#' est2p(x1)
#'
#' @noRd

est2p <- function(x,
                  h0 = 0,
                  min_size = 100) {
    # Based on the method in
    # https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    if (length(x) < min_size) {
        warning(paste("Bootstrap p-value not computed. Less than ",
                      min_size,
                      "bootstrap estimates."))
        return(NA)
      }
    b <- length(x)
    m0 <- sum((x < h0))
    out <- 2 * min(m0 / b, 1 - m0 / b)
    out
  }
