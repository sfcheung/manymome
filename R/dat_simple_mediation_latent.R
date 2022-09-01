#' @title Sample Dataset: A Simple
#' Latent Mediation Model
#'
#' @description Generated from a simple
#' mediation model among xthree latent
#' factors, `fx`, `fm`, and `fy`, xeach
#' has three indicators.
#'
#' @details
#'
#' The model:
#'
#' ```
#' fx =~ x1 + x2 + x3
#' fm =~ m1 + m2 + m3
#' fy =~ y1 + y2 + y3
#' fm ~ a * fx
#' fy ~ b * fm + cp * fx
#' indirect := a * b
#' ```
#'
#' @format A data frame with 200 rows
#' and 11 variables:
#' \describe{
#'   \item{x1}{Indicator of `fx`. Numeric.}
#'   \item{x2}{Indicator of `fx`. Numeric.}
#'   \item{x3}{Indicator of `fx`. Numeric.}
#'   \item{m1}{Indicator of `fm`. Numeric.}
#'   \item{m2}{Indicator of `fm`. Numeric.}
#'   \item{m3}{Indicator of `fm`. Numeric.}
#'   \item{y1}{Indicator of `fy`. Numeric.}
#'   \item{y2}{Indicator of `fy`. Numeric.}
#'   \item{y3}{Indicator of `fy`. Numeric.}
#' }
#'
"simple_mediation_latent"