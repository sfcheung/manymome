#' @title Sample Dataset: Two Categorical Moderators
#'
#' @description A two-moderator model,
#' both categorical.
#'
#' @format A data frame with 600 rows
#' and 6 variables:
#' \describe{
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{x}{Predictor. Numeric.}
#'   \item{gp}{Moderator 1. String: "Control" and "Treatment".}
#'   \item{site}{Moderator 2. String: "Site 1", "Site 2", and "Site 3".}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(data_mod_cat_2w)
#' lm_out <- lm(y ~ x*gp + x*site + c1 + c2, data_mod_cat_2w)
#' out <- cond_effects(
#'   wlevels = c("site", "gp"),
#'   x = "x",
#'   fit = lm_out
#' )
#' out
#' plot(out, facet_grid_cols = "site", graph_type = "tumble")
"data_mod_cat_2w"
