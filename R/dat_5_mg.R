#' @title Sample Dataset: A Complicated
#' Mediation Model With Two Groups
#'
#' @description A mediation model with
#' two predictors, two pathways, and
#' two groups.
#'
#' @format A data frame with 300 rows
#' and 5 variables:
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
#'   \item{group}{Group variable. Character. 'Group A' or 'Group B'}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_complicated_mg)
#' dat <- data_med_complicated_mg
#' mod <-
#' "
#' m11 ~ x1 + x2 + c1 + c2
#' m12 ~ m11 + c1 + c2
#' m2 ~ x1 + x2 + c1 + c2
#' y1 ~ m11 + m12 + x1 + x2 + c1 + c2
#' y2 ~ m2 + x1 + x2 + c1 + c2
#' "
#' fit <- sem(mod, dat, group = "group")
#' summary(fit)
"data_med_complicated_mg"
