#' @title Sample Dataset: With Reverse Items
#'
#' @description Generated from a complicated
#' mediation model among several latent
#' factors, `fx`, `fm1`, `fm2`, `fm3`,
#' and `fy`, along with two control variables,
#' `c1` and `c2`, each
#' has four indicators, and some are
#' reverse items.
#'
#' @details
#'
#' The model:
#'
#' \preformatted{fx =~ x_1 + x_2 + x_3 + x_4
#' fm1 =~ m1_1 + m1_2 + m1_3 + m1_4
#' fm2 =~ m2_1 + m2_2 + m2_3 + m2_4
#' fm3 =~ m3_1 + m3_2 + m3_3 + m3_4
#' fy =~ y_1 + y_2 + y_3 + y_4
#' fc1 =~ c1_1 + c1_2 + c1_3 + c1_4
#' fc2 =~ c2_1 + c2_2 + c2_3 + c2_4
#' fm1 ~ a1 * fx + fc1 + fc2
#' fm3 ~ b1 * fm1 + fc1 + fc2
#' fm2 ~ a2 * fx + fc1 + fc2
#' fy ~ b3 * fm3 + b2 * fm2 + fc1 + fc2
#' ab13 := a1 * b1 * b3
#' ab2 := a2 * b2}
#'
#' @format A data frame with 500 rows
#' and 11 variables:
#' \describe{
#'   \item{x_1}{Indicator of `fx`. Numeric.}
#'   \item{x_2}{Indicator of `fx`. Numeric.}
#'   \item{x_3}{Indicator of `fx`. Numeric.}
#'   \item{x_4}{Indicator of `fx`. Numeric. A reverse item.}
#'   \item{m1_1}{Indicator of `fm1`. Numeric.}
#'   \item{m1_2}{Indicator of `fm1`. Numeric.}
#'   \item{m1_3}{Indicator of `fm1`. Numeric.}
#'   \item{m1_4}{Indicator of `fm1`. Numeric.}
#'   \item{m2_1}{Indicator of `fm2`. Numeric.}
#'   \item{m2_2}{Indicator of `fm2`. Numeric.}
#'   \item{m2_3}{Indicator of `fm2`. Numeric.}
#'   \item{m2_4}{Indicator of `fm2`. Numeric.}
#'   \item{m3_1}{Indicator of `fm3`. Numeric.}
#'   \item{m3_2}{Indicator of `fm3`. Numeric.}
#'   \item{m3_3}{Indicator of `fm3`. Numeric.}
#'   \item{m3_4}{Indicator of `fm3`. Numeric.}
#'   \item{y_1}{Indicator of `fy`. Numeric.}
#'   \item{y_2}{Indicator of `fy`. Numeric.}
#'   \item{y_3}{Indicator of `fy`. Numeric.}
#'   \item{y_4}{Indicator of `fy`. Numeric. A reverse item.}
#'   \item{c1_1}{Indicator of `fc1`. Numeric.}
#'   \item{c1_2}{Indicator of `fc1`. Numeric.}
#'   \item{c1_3}{Indicator of `fc1`. Numeric.}
#'   \item{c1_4}{Indicator of `fc1`. Numeric.}
#'   \item{c2_1}{Indicator of `fc2`. Numeric.}
#'   \item{c2_2}{Indicator of `fc2`. Numeric.}
#'   \item{c2_3}{Indicator of `fc2`. Numeric.}
#'   \item{c2_4}{Indicator of `fc2`. Numeric.}
#'   \item{x}{Mean of the indicators of `x`. Numeric.}
#'   \item{m1}{Mean of the indicators of `m1`. Numeric.}
#'   \item{m2}{Mean of the indicators of `m2`. Numeric.}
#'   \item{m3}{Mean of the indicators of `m3`. Numeric.}
#'   \item{y}{Mean of the indicators of `y`. Numeric.}
#'   \item{c1}{Mean of the indicators of `c1`. Numeric.}
#'   \item{c2}{Mean of the indicators of `c2`. Numeric.}
#'
#' }
#'
"data_indicators"