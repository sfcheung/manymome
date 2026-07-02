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

#' @title Sample Dataset: One Moderator on Two Predictors
#'
#' @description A one-moderator model,
#' with moderator affecting the effects
#' of two predictors.
#'
#' @format A data frame with 200 rows
#' and 6 variables:
#' \describe{
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{x1}{Predictor. Numeric.}
#'   \item{x2}{Predictor. Numeric.}
#'   \item{w}{Moderator. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_mod_2x1w)
#' lm_out <- lm(y ~ x1*w + x2*w + c1 + c2, data_mod_2x1w)
#' out1 <- cond_effects(
#'   wlevels = "w",
#'   x = "x1",
#'   fit = lm_out
#' )
#' out1
#' plot(out1, graph_type = "tumble")
#'
#' out2 <- cond_effects(
#'   wlevels = "w",
#'   x = "x2",
#'   fit = lm_out
#' )
#' out2
#' plot(out2, graph_type = "tumble")
"data_mod_2x1w"

#' @title Sample Dataset: Two Moderators on Two Predictors
#'
#' @description A two-moderator model,
#' with the moderators affecting the
#' effects of different predictors.
#'
#' @format A data frame with 200 rows
#' and 6 variables:
#' \describe{
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{x1}{Predictor 1. Numeric.}
#'   \item{x2}{Predictor 2. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_mod_2x2w)
#' lm_out <- lm(y ~ x1*w1 + x2*w2 + c1 + c2, data_mod_2x2w)
#' out1 <- cond_effects(
#'   wlevels = "w1",
#'   x = "x1",
#'   fit = lm_out
#' )
#' out1
#' plot(out1, graph_type = "tumble")
#'
#' out2 <- cond_effects(
#'   wlevels = "w2",
#'   x = "x2",
#'   fit = lm_out
#' )
#' out2
#' plot(out2, graph_type = "tumble")
"data_mod_2x2w"