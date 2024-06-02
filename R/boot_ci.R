#' @noRd

# Internal function for different types
# of bootstrap CI.
# Only work for one statistic.

boot_ci_internal <- function(t0,
                             t,
                             level = .95,
                             boot_ci_type = c("perc", "bc"),
                             add_names = TRUE) {
    boot_ci_type <- match.arg(boot_ci_type)
    out <- switch(boot_ci_type,
                  perc = boot_ci_perc(t0 = t0, t = t, level = level),
                  bc = boot_ci_bc(t0 = t0, t = t, level = level))
    # Must be a 2-element numeric vector
    if (add_names) {
        names(out) <- paste0(formatC(c(100 * (1 - level) / 2,
                                     100 * (1 - (1 - level) / 2)), 2,
                                     format = "f"), "%")
      }
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