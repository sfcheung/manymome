#' @title Sample Dataset: A Simple
#' Mediation Model with b-Path
#' Moderated-Moderation
#'
#' @description A simple mediation model
#' with moderated-mediation on the
#' b-path.
#'
#' @format A data frame with 100 rows
#' and 5 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator on b-path. Numeric.}
#'   \item{w2}{Moderator on the moderating effect of w1. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(data_med_mod_b_mod)
#' dat <- data_med_mod_b_mod
#' summary(lm_m <- lm(m ~ x + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m*w1*w2 + x + c1 + c2, dat))
"data_med_mod_b_mod"
