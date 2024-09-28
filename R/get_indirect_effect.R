#' @title Get The Conditional Indirect
#' Effect for One Row of
#' 'cond_indirect_effects' Output
#'
#' @description Return the conditional
#' indirect effect of one row of the
#' output of [cond_indirect_effects()].
#'
#' @details
#' [get_one_cond_indirect_effect()]
#' extracts the
#' corresponding output of
#' [cond_indirect()] from the requested
#' row.
#'
#' @param object The output of
#' [cond_indirect_effects()].
#'
#' @param row The row number of the row
#' to be retrieved.
#'
#' @return
#' [get_one_cond_indirect_effect()]
#' returns an `indirect`-class object,
#' similar to the output of
#' [indirect_effect()] and
#' [cond_indirect()]. See
#' [indirect_effect()] and
#' [cond_indirect()] for details on
#' these classes.
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
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for cond_indirect():
#'
#' # Conditional effects from x to m1
#' # when w1 is equal to each of the default levels
#' out1 <- cond_indirect_effects(x = "x", y = "m1",
#'                               wlevels = c("w1", "w4"), fit = fit)
#' get_one_cond_indirect_effect(out1, 3)
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the levels
#' out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
#'                               wlevels = c("w1", "w4"), fit = fit)
#' get_one_cond_indirect_effect(out2, 4)
#'
#' @rdname get_one_cond_indirect_effect
#'
#' @export

get_one_cond_indirect_effect <- function(object, row) {
    full_output <- attr(object, "full_output")
    out <- full_output[[row]]
    out
  }

#' @rdname get_one_cond_indirect_effect
#'
#' @details
#' [get_one_cond_effect()] is an
#' alias of [get_one_cond_indirect_effect()].
#'
#' @export

get_one_cond_effect <- get_one_cond_indirect_effect

#' @param ... Optional arguments to be
#' passed to teh `print` method of
#' the output of
#' [indirect_effect()] and
#' [cond_indirect()]
#'
#' @return
#' [print_all_cond_indirect_effects()]
#' returns the object invisibly. Called
#' for its side effect.
#'
#' @examples
#'
#' print_all_cond_indirect_effects(out2, digits = 2)
#'
#' @rdname get_one_cond_indirect_effect
#'
#' @details
#' [print_all_cond_indirect_effects()] loops over
#' the conditional effects and print all
#' of them.
#'
#' @export

print_all_cond_indirect_effects <- function(object, ...) {
    full_output <- attr(object, "full_output")
    wlevels <- attr(object, "wlevels")
    wlevels_names <- rownames(wlevels)
    section_sep <- paste0(rep("-",
                          round(getOption("width") * .80)),
                          collapse = "")
    for (xx in seq_along(full_output)) {
        cat(section_sep, "\n")
        cat("Moderator Level:",
            wlevels_names[xx], "\n")
        print(full_output[[xx]], ...)
      }
    return(invisible(object))
  }

#' @rdname get_one_cond_indirect_effect
#'
#' @details
#' [print_all_cond_effects()] is an
#' alias of [print_all_cond_indirect_effects()].
#'
#' @export
print_all_cond_effects <- print_all_cond_indirect_effects