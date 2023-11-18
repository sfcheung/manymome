#' @title Form Confidence Intervals
#'
#' @description An internal function to
#' form confidence intervals from
#' bootstrap or similar estimates.
#'
#' @details Use `boot:boot.ci()` to
#' do the work.
#'
#' @return
#' A numeric vector with the confidence
#' limits. `NA` if no stored estimates
#' found.
#'
#' @param object The object with elements
#' like a vector of bootstrap estimates
#'
#' @param est0 The name of the element
#' with the original estimate. Default
#' is `"indirect"`.
#'
#' @param where The name(s) of the element
#' with the vector. Can be a character
#' vector. The first one found will be
#' used.
#'
#' @param level The level of confidence.
#' Default is .95, for 95% confidence
#' intervals.
#'
#' @param ... Additional arguments. Not
#' used for now.
#'
#' @noRd
confint_int <- function(object,
                        est0 = "indirect",
                        where = c("boot_indirect",
                                  "mc_indirect"),
                        level = .95,
                        ...) {
    est <- object[[est0]]
    if (!is.numeric(est)) return(NA)
    est_name <- intersect(where, names(object))[1]
    if (!is.character(est_name)) return(NA)
    sim_est <- object[[est_name]]
    if (!is.numeric(sim_est)) return(NA)
    nboot <- length(sim_est)
    tmp <- list(t = matrix(sim_est, nrow = nboot, ncol = 1),
                t0 = est,
                R = nboot)
    boot_ci0 <- boot::boot.ci(tmp, conf = level, type = "perc")
    boot_ci1 <- boot_ci0$percent[4:5]
    names(boot_ci1) <- paste0(formatC(c(100 * (1 - level) / 2,
                                  100 * (1 - (1 - level) / 2)), 2,
                                  format = "f"), "%")
    return(boot_ci1)
  }