#' @title Total Indirect Effect Between
#' Two Variables
#'
#' @description Compute the total
#' indirect effect between two
#' variables in the paths estimated by
#' [many_indirect_effects()].
#'
#' @details It extracts the
#' `indirect`-class objects
#' of relevant paths and then add
#' the indirect effects together
#' using the `+` operator.
#'
#' @return An `indirect`-class
#' object.
#'
#' @param object The output of
#' [many_indirect_effects()], or a list
#' of `indirect`-class objects.
#'
#' @param x Character. The name of the `x` variable.
#' All paths start from `x` will be
#' included.
#'
#' @param y Character. The name of the `y` variable.
#' All paths end at `y` will be included.
#'
#'
#' @seealso [many_indirect_effects()]
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
#'
#' # All indirect paths, control variables excluded
#' paths <- all_indirect_paths(fit,
#'                             exclude = c("c1", "c2"))
#' paths
#'
#' # Indirect effect estimates
#' out <- many_indirect_effects(paths,
#'                              fit = fit)
#' out
#'
#' # Total indirect effect from x to y
#' total_indirect_effect(out,
#'                       x = "x",
#'                       y = "y")
#'
#'
#' @export

total_indirect_effect <- function(object,
                                  x,
                                  y) {
    if (missing(x) || missing(y)) {
        stop("Both 'x' and 'y' must be supplied")
      }
    if (!is.list(object)) {
        stop("object is not a list")
      }
    has_groups <- indirect_list_has_groups(object)
    if (has_groups) {
        tmp <- group_labels_and_numbers_list(object)
        group_labels <- tmp$label
        group_numbers <- tmp$number
        i1 <- sapply(object, function(xx) xx$group_label)
        i2 <- split(seq_along(object), i1)
        out0 <- lapply(i2, function(xx) {
                    object_i <- object[xx]
                    total_indirect_effect_i(object_i,
                                            x = x,
                                            y = y)
                  })
        out0 <- out0[group_labels]
        tmp <- sapply(out0, function(xx) !identical(xx, NA))
        if (!any(tmp)) {
            stop("In all groups, no paths from ", x, " to ", y, ".")
          }
        out0 <- out0[tmp]
      } else {
        # ix <- sapply(object, function(z) z$x)
        # iy <- sapply(object, function(z) z$y)
        # i0 <- (ix %in% x) & (iy %in% y)
        # if (isFALSE(any(i0))) {
        #     return(NA)
        #   }
        # p1 <- object[i0]
        out0 <- total_indirect_effect_i(object = object,
                                        x = x,
                                        y = y)
        if (identical(out0, NA)) {
            stop("No paths from ", x, " to ", y, ".")
          }
      }
    out0
  }

#' @noRd

total_indirect_effect_i <- function(object,
                                    x,
                                    y) {
    ix <- sapply(object, function(z) z$x)
    iy <- sapply(object, function(z) z$y)
    i0 <- (ix %in% x) & (iy %in% y)
    if (isFALSE(any(i0))) {
        return(NA)
      }
    p1 <- object[i0]
    out <- Reduce(`+`, p1)
    out
  }