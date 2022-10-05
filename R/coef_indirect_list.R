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
#' # TODO
#'
#' @export

coef.indirect_list <- function(object, ...) {
    out <- sapply(object, stats::coef)
    names(out) <- names(object)
    out
  }
