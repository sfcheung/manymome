#' @title Sample Dataset: A Latent
#' Variable Moderated Mediation Model With 4
#' Factors
#'
#' @description This data set is for
#' testing functions for moderated
#' mediation among latent variables.
#'
#' @format A data frame with 500 rows
#' and 16 variables:
#' \describe{
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{w1}{Indicator. Numeric.}
#'   \item{w2}{Indicator. Numeric.}
#'   \item{w3}{Indicator. Numeric.}
#'   \item{w4}{Indicator. Numeric.}
#'   \item{m1}{Indicator. Numeric.}
#'   \item{m2}{Indicator. Numeric.}
#'   \item{m3}{Indicator. Numeric.}
#'   \item{m4}{Indicator. Numeric.}
#'   \item{y1}{Indicator. Numeric.}
#'   \item{y2}{Indicator. Numeric.}
#'   \item{y3}{Indicator. Numeric.}
#'   \item{y4}{Indicator. Numeric.}
#' }
#'
#'
#' @examples
#' data(data_sem_mome)
#' mod <-
#' "
#' fx =~ x1 + x2 + x3 + x4
#' fw =~ w1 + w2 + w3 + w4
#' fm =~ m1 + m2 + m3 + m4
#' fy =~ y1 + y2 + y3 + y4
#' fm ~ fx + fw + fx:fw
#' fy ~ fm + fx
#' "
#' library(lavaan)
#' fit <- sam(model = mod, data = data_sem_mome)
#' summary(fit)
#'
"data_sem_mome"
