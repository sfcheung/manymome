#' @title Get The Conditional Indirect Effect for One Row of
#' 'cond_indirect_effects' Output
#'
#' @description Return the conditional indirect effect of one row of the output of
#'        [cond_indirect_effects()].
#'
#' @details It just extracts the corresponding output of [cond_indirect()]
#'  from the requested row.
#'
#' @param object The output of [cond_indirect_effects()].
#' @param row The row number of the row to be retrieved.
#'
#' @return An `indirect`-class object, similar to the output of
#'  [indirect_effect()] and [cond_indirect()].
#'  See [indirect_effect)] and [cond_indirect()] for details
#'  on these classes.
#'
#'
#' @seealso [cond_indirect_effects]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ x  + w1 + x:w1
#' m2 ~ m1
#' y  ~ m2 + x + w4 + m2:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for cond_indirect():
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the default levels
#' out1 <- cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = c("w1", "w4"), fit = fit)
#' get_one_cond_indirect_effect(out1, 3)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the levels
#' out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
#'                       wlevels = c("w1", "w4"), fit = fit)
#' get_one_cond_indirect_effect(out2, 4)
#'
#' @export

get_one_cond_indirect_effect <- function(object, row) {
    full_output <- attr(object, "full_output")
    out <- full_output[[row]]
    out
  }

#' @describeIn get_one_cond_indirect_effect An alias to [get_one_cond_indirect_effect()]
#' @export

get_one_cond_effect <- get_one_cond_indirect_effect