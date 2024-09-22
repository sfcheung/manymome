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

#' @title A 'lavaan'-VCOV List
#'
#' @description Convert a 'lavaan'-VCOV
#' to a list of VOCV matrices.
#'
#' @noRd

est_vcov_list <- function(est_vcov,
                          est) {
    if (is.list(est_vcov)) {
        if (!inherits(est_vcov[[1]], "lavaan.matrix.symmetric")) {
            # Assume it is a vcov from lm
            return(est_vcov)
          }
      }
    # TODO: Handle multiple-group models
    est$est_labels <- lavaan::lav_partable_labels(est)
    vcov_names <- colnames(est_vcov)
    # TODO: Support intercept
    tmpfct <- function(yi) {
        esti <- est[(est$lhs == yi) & (est$op == "~"), ]
        i <- match(esti$est_labels, vcov_names)
        if (all(is.na(i))) return(matrix(numeric(0), 0, 0))
        vcovi <- est_vcov[i, i, drop = FALSE]
        colnames(vcovi) <- rownames(vcovi) <- esti$rhs
        vcovi
      }
    ys <- lavaan_get_dvs(est)
    out <- lapply(ys,
                  tmpfct)
    names(out) <- ys
    out
  }
