#' @title Extract the Indirect Effects
#' from a 'indirect_list' Object
#'
#' @description Return the estimates of
#' the indirect effects in the output of
#' [many_indirect_effects()].
#'
#' @details It extracts the estimates
#' in each 'indirect'-class object
#' in the list.
#'
#' If standardized effect is requested
#' when calling [many_indirect_effects()],
#' the effects
#' returned are also standardized.
#'
#' @return A numeric vector of the
#' indirect effects.
#'
#' @param object The output of
#' [many_indirect_effects()].
#'
#' @param ...  Optional arguments.
#' Ignored by the function.
#'
#'
#' @seealso [indirect_effect()] and
#' [cond_indirect()].
#'
#' @examples
#'
#' library(lavaan)
#' data(data_serial_parallel)
#' mod <-
#' "
#' m11 ~ x + c1 + c2
#' m12 ~ m11 + x + c1 + c2
#' m2 ~ x + c1 + c2
#' y ~ m12 + m2 + m11 + x + c1 + c2
#' "
#' fit <- sem(mod, data_serial_parallel,
#'            fixed.x = FALSE)
#' # All indirect paths from x to y
#' paths <- all_indirect_paths(fit,
#'                            x = "x",
#'                            y = "y")
#' paths
#' # Indirect effect estimates
#' out <- many_indirect_effects(paths,
#'                              fit = fit)
#' out
#' coef(out)
#'
#'
#' @export

coef.indirect_list <- function(object, ...) {
    out <- sapply(object, stats::coef)
    names(out) <- names(object)
    out
  }
