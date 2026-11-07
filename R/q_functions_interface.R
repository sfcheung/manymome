#' @noRd
# Extract boot_out or mc_out
get_ci_out_from_q <- function(
  q_out
) {
  chk_names1 <- c(
    "ind_out",
    "ind_total",
    "cond_ind_out",
    "dir_out"
  )
  chk_names2 <- c(
    "ustd",
    "stdx",
    "stdy",
    "stdxy"
  )
  for (x1 in chk_names1) {
    for (x2 in chk_names2) {
      tmp <- q_out[[x1]][[x2]]
      if (!is.null(tmp)) {
        tmp <- tmp[[1]]
        out0 <- attr(tmp, "boot_out")
        if (inherits(out0, "boot_out")) {
          return(out0)
        }
        out0 <- attr(tmp, "mc_out")
        if (inherits(out0, "mc_out")) {
          return(out0)
        }
      }
    }
  }
  return(NULL)
}
