#' @title Sample Dataset: A Latent
#' Variable Mediation Model With 4
#' Factors
#'
#' @description This data set is for
#' testing functions in a four-factor
#' structural model.
#'
#' @format A data frame with 200 rows
#' and 14 variables:
#' \describe{
#'   \item{x01}{Indicator. Numeric.}
#'   \item{x02}{Indicator. Numeric.}
#'   \item{x03}{Indicator. Numeric.}
#'   \item{x04}{Indicator. Numeric.}
#'   \item{x05}{Indicator. Numeric.}
#'   \item{x06}{Indicator. Numeric.}
#'   \item{x07}{Indicator. Numeric.}
#'   \item{x08}{Indicator. Numeric.}
#'   \item{x09}{Indicator. Numeric.}
#'   \item{x10}{Indicator. Numeric.}
#'   \item{x11}{Indicator. Numeric.}
#'   \item{x12}{Indicator. Numeric.}
#'   \item{x13}{Indicator. Numeric.}
#'   \item{x14}{Indicator. Numeric.}
#' }
#'
#'
#' @examples
#' data(data_sem)
#' dat <- data_med_mod_b_mod
#' mod <-
#'   'f1 =~ x01 + x02 + x03
#'    f2 =~ x04 + x05 + x06 + x07
#'    f3 =~ x08 + x09 + x10
#'    f4 =~ x11 + x12 + x13 + x14
#'    f3 ~  a1*f1 + a2*f2
#'    f4 ~  b1*f1 + b3*f3
#'    a1b3 := a1 * b3
#'    a2b3 := a2 * b3
#'   '
#' fit <- lavaan::sem(model = mod, data = data_sem)
#' summary(fit)
#'
"data_sem"
