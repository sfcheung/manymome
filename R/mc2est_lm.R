#' @noRd
mc2est_lm <- function(
  fit,
  progress = TRUE
) {
  mc_est0 <- attr(fit, "mc")
  ptable0 <- lm2ptable(fit)
  ptable <- ptable0$est
  ptable$label <- lavaan::lav_partable_labels(ptable)
  f <- function(
    i
  ) {
    mc_est_i <- mc_est0[i, , drop = TRUE]
    out_i <- ptable
    out_i$est <- mc_est_i[ptable$label]
    out_i$label <- NULL
    out_i
  }
  out0 <- pbapply::pblapply(
    seq_len(nrow(mc_est0)),
    f
  )
  out0
}