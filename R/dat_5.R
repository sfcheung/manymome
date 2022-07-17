#' @title A Complicated Mediation Model: Sample Dataset
#'
#' @description A mediation model with two predictors, two pathways,
#'
#' @format A data frame with 300 rows and 5 variables:
#' \describe{
#'   \item{x1}{Predictor 1. Numeric.}
#'   \item{x2}{Predictor 2. Numeric.}
#'   \item{m11}{Mediator 1 in Path 1. Numeric.}
#'   \item{m12}{Mediator 2 in Path 1. Numeric.}
#'   \item{m2}{Mediator in Path 2. Numeric.}
#'   \item{y1}{Outcome variable 1. Numeric.}
#'   \item{y2}{Outcome variable 2. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(data_med_complicated)
#' dat <- data_med_complicated
#' summary(lm_m11 <- lm(m11 ~ x1 + x1 + x2 + c1 + c2, dat))
#' summary(lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, dat))
#' summary(lm_y1 <- lm(y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat))
#' summary(lm_y2 <- lm(y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat))
"data_med_complicated"
