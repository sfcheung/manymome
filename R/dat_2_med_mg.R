#' @title Sample Dataset: Simple
#' Mediation With Two Groups
#'
#' @description A simple mediation
#' model with two groups.
#'
#' @format A data frame with 100 rows
#' and 5 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#'   \item{group}{Group variable. Character. "Group A" or "Group B"}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mg)
#' mod <-
#' "
#' m ~ c(a1, a2) * x + c1 + c2
#' y ~ c(b1, b2) * m + x + c1 + c2
#' a1b1 := a1 * b1
#' a2b2 := a2 * b2
#' abdiff := a2b2 - a1b1
#' "
#' fit <- sem(mod, data_med_mg, fixed.x = FALSE,
#'            group = "group")
#' parameterEstimates(fit)
"data_med_mg"
