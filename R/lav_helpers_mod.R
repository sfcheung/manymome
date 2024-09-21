#' @title Residual Degrees of Freedom of a 'lavaan'-Object
#'
#' @description Set the df to `Inf`.
#'
#' @details
#' - Support only `lavaan`-like objects.
#'
#' @param object A `lavaan`-like object.
#'
#' @return
#' A named numeric vector of the
#' residual dfs.
#'
#' @noRd

lav_df_residual <- function(object) {
    est <- lav_est(object)
    dvs <- lavaan_get_dvs(est)
    out <- rep(Inf, length(dvs))
    names(out) <- dvs
    out
  }
