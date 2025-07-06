#' @title Plot Method for the Output
#' of 'q_mediation' Family
#'
#' @description Plot the path model
#' fitted by the family of 'q_mediation'
#' functions.
#'
#' @details
#' (TODO)
#'
#' @return A `qgraph` object generated
#' by [semPlot::semPaths()], which is
#' plotted by default unless `DoNotPlot`
#' is set to `TRUE`. It can be further
#' modified by other functions that
#' work on a `qgraph` object, such as
#' those from [semptools].
#'
#' @param x The output of
#' [q_mediation()],
#' [q_simple_mediation()],
#' [q_serial_mediation()], and
#' [q_parallel_mediation()].
#' (Named `x`
#' because it is required in the naming
#' of arguments of the `plot` generic
#' function.)
#'
#'
#' @export
plot.q_mediation <- function(
                      x,
                      ...
                    ) {
  if (!requireNamespace("semptools", quietly = TRUE)) {
    stop("Please install 'semptools' first.")
  }
}
