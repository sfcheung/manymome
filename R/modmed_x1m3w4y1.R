#' @title Sample Dataset: Moderated Serial Mediation
#'
#' @description Generated from a serial mediation model with
#'   one predictor, three mediators, and one outcome
#'   variable, with one moderator in each stage.
#'
#' @format A data frame with 200 rows and 11 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{w3}{Moderator 3. Numeric.}
#'   \item{w4}{Moderator 4. Numeric.}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{m3}{Mediator 3. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{gp}{Three values: "one", "two", "three". String.}
#'   \item{city}{Four values: "alpha", "beta", "gamma", "sigma". String.}
#' }
#'
"modmed_x1m3w4y1"