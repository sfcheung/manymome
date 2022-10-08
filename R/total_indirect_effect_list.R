#' @title Total Indirect Effect Between
#' Two Variables
#'
#' @description Compute the total
#' indirect effect between two
#' variables in the paths estimated by
#' [many_indirect_effects()].
#'
#' @details It extracts the estimates
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
#' @param x The name of the `x` variable.
#' All paths start from `x` will be
#' included.
#'
#' @param y The name of the `y` variable.
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
#' # All indirect paths, control variables excluded
#' paths <- all_indirect_paths(fit,
#'                             exclude = c("c1", "c2"))
#' paths
#' # Indirect effect estimates
#' out <- many_indirect_effects(paths,
#'                              fit = fit)
#' out
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
    ix <- sapply(object, function(z) z$x)
    iy <- sapply(object, function(z) z$y)
    i0 <- (ix %in% x) & (iy %in% y)
    if (isFALSE(any(i0))) {
        stop("No valid path was found.")
      }
    p1 <- object[i0]
    out <- Reduce(`+`, p1)
    out
  }
