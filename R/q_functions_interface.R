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

#' @noRd
# Extract cond_effects
get_cond_effects_from_q <- function(
  q_out,
  m = NULL,
  which = c("ustd", "stdx", "stdy", "stdxy")
) {
  which <- match.arg(which)
  out0 <- q_out$cond_ind_out[[which]]
  if (is.null(out0)) {
    stop("No conditional effects found.")
  }
  x <- q_out$x
  y <- q_out$y
  if (length(out0) == 1) {
    # Has only one path. Use it
    path_name <- names(out0)
  } else {
    if (is.null(m)) {
      stop("m must be specified because there are more than one path.")
    }
    path_name <- c(x, m, y)
    path_name <- paste0(path_name, collapse = " -> ")
  }
  i <- match(
      path_name,
      names(out0)
    )
  if (is.na(i)) {
    stop("The path ",
         path_name,
         " not in the output.")
  }
  out1 <- out0[[i]]
  if (inherits(out1, "cond_indirect_effects")) {
    return(out1)
  } else {
    stop("Cannot find a cond_indirect_effects object.")
  }
}
