#' @title Sample Dataset: Mixed Moderators
#'
#' @description A two-moderator model,
#' one categorical and one numerical.
#'
#' @format A data frame with 600 rows
#' and 6 variables:
#' \describe{
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{x}{Predictor. Numeric.}
#'   \item{w}{Moderator 2. Numeric.}
#'   \item{city}{Moderator 1. String: "City A" and "City B".}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(data_mod_cat_num_2w)
#' lm_out <- lm(y ~ x*city*w + c1 + c2, data_mod_cat_num_2w)
#' out <- cond_effects(
#'   wlevels = c("city", "w"),
#'   x = "x",
#'   fit = lm_out
#' )
#' out
#' plot(out, facet_grid_cols = "city", graph_type = "tumble")
"data_mod_cat_num_2w"
