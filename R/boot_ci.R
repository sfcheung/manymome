#' @noRd

# Internal function for different types
# of bootstrap CI.
# Only work for one statistic.

boot_ci_internal <- function(t0,
                             t,
                             level = .95,
                             boot_ci_type = c("perc", "bc")) {
    boot_ci_type <- match.arg(boot_ci_type)
    out <- switch(boot_ci_type,
                  perc = boot_ci_perc(t0 = t0, t = t, level = level),
                  bc = boot_ci_bc(t0 = t0, t = t, level = level))
    # Must be a 2-element numeric vector
    out
  }

#' @noRd

boot_ci_perc <- function(t0,
                         t,
                         level = .95) {
    boot_tmp <- list(t0 = t0,
                     t = matrix(t, ncol = 1),
                     R = length(t))
    ci <- boot::boot.ci(boot_tmp,
                        type = "perc",
                        conf = level)
    ci$percent[4:5]
  }

#' @noRd

boot_ci_bc <- function(t0,
                       t,
                       level = .95) {

  }