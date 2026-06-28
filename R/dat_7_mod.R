#' @title Sample Dataset: Two Moderators (Version 2)
#'
#' @description A two-moderator model,
#' with variation in the standard deviation
#' of `x` related to one moderator.
#'
#' @format A data frame with 200 rows
#' and 6 variables:
#' \describe{
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_mod_2w)
#' lm_out <- lm(y ~ x*w1 + x*w2 + c1 + c2, data_mod_2w)
#' out <- cond_effects(
#'   wlevels = c("w1", "w2"),
#'   x = "x",
#'   fit = lm_out
#' )
#' out
#' plot(out, facet_grid_cols = "w1", graph_type = "tumble")
#' plot(out, facet_grid_cols = "w2", graph_type = "tumble")
"data_mod_2w"
