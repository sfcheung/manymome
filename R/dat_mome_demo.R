#' @title A Complicated Moderated-Mediation Model for Demonstration
#'
#' @description Generated from a complicated moderated-mediation model for
#'   demonstration.
#'
#' @details
#'
#' The model:
#'
#' ```
#' # w1x1 <- x1 * w1
#' # w2m2 <- w2 * m2
#' m1 ~ x1 + w1 + w1x1 + x2 + c1 + c2
#' m2 ~ m1 + c1 + c2
#' m3 ~ x2 + x1 + c1 + c2
#' y1 ~ m2 + w2 + w2m2 + x1 + x2 + m3 + c1 + c2
#' y2 ~ m3 + x2 + x1 + m2 + c1 + c2
#' # Covariances excluded for brevity
#' ```
#'
#' @format A data frame with 200 rows and 11 variables:
#' \describe{
#'   \item{x1}{Predictor 1. Numeric.}
#'   \item{x2}{Predictor 2. Numeric.}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{m3}{Mediator 3. Numeric.}
#'   \item{y1}{Outcome Variable 1. Numeric.}
#'   \item{y2}{Outcome Variable 2. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 21. Numeric.}
#'   \item{c1}{Control Variable 1. Numeric.}
#'   \item{c2}{Control Variable 2. Numeric.}
#' }
#'
"data_mome_demo"