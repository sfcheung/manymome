#' @title (WIP) Monte Carlo Estimates for a
#' `lavaan` Output
#'
#' @description Generate Monte Carlo
#' estimates from the output of
#' [lavaan::sem()].
#'
#' @details This function is for
#' advanced users. [do_mc()] is a
#' function users should try first
#' because [do_mc()] has a general
#' interface for input-specific
#' functions like this one.
#'
#' [fit2mc_out()] can be used
#' to extract the stored Monte Carlo
#' estimates so that they can be reused
#' by [indirect_effect()],
#' [cond_indirect_effects()] and related
#' functions to form Monte Carlo
#' confidence intervals for effects such
#' as indirect effects and conditional
#' indirect effects.
#'
#' This approach removes the need to
#' repeat Monte Carlo simulation in each
#' call to
#' [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions. It also ensures
#' that the same set of Monte Carlo
#' estimates is used in all subsequent
#' analyses.
#'
#' @return A `mc_out`-class object
#' that can be used for the `mc_out`
#' argument of [indirect_effect()],
#' [cond_indirect_effects()], and
#' related functions for forming
#' Monte Carlo confidence intervals.
#'
#' The object is a list with the number
#' of elements equal to the number of
#' Monte Carlo replication. Each element
#' is a
#' list of the parameter estimates and
#' sample variances and covariances of
#' the variables in each Monte Carlo
#' replication.
#'
#' @param fit The fit object. This
#' function only supports a
#' [lavaan::lavaan-class] object.
#'
#' @seealso [do_mc()], the general
#' purpose function that users should
#' try first before using this function.
#'
#' @examples
#'
#' # TO PROCESS
#'
#' library(lavaan)
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' dat$"x:w" <- dat$x * dat$w
#' dat$"m:w" <- dat$m * dat$w
#' mod <-
#' "
#' m ~ x + w + x:w + c1 + c2
#' y ~ m + w + m:w + x + c1 + c2
#' "
#'
#' # Bootstrapping not requested in calling lavaan::sem()
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' fit_boot_out <- fit2boot_out_do_boot(fit = fit,
#'                                      R = 40,
#'                                      seed = 1234,
#'                                      progress = FALSE)
#' out <- cond_indirect_effects(wlevels = "w",
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = fit,
#'                              boot_ci = TRUE,
#'                              boot_out = fit_boot_out)
#' out
#'
#' @export

fit2mc_out <- function(fit) {
    mc_est <- mc2est(fit)
    mc_implied <- mc2implied(fit)
    out <- mapply(function(x, y) list(est = x,
                                      implied_stats = y),
                  x = mc_est,
                  y = mc_implied,
                  SIMPLIFY = FALSE)
    names(out) <- names(mc_est)
    class(out) <- "mc_out"
    out
  }

# Convert stored estimates to a list of
# parameter estimates tables. This is
# preferred because it is what users
# usually see.
#' @noRd

mc2est <- function(fit) {
    if (is.null(fit@external$manymome$mc)) {
        stop("Monte Carlo estimates not found. Please run do_mc() first.")
      }
    mc_est0 <- fit@external$manymome$mc
    ptable <- lavaan::parameterTable(fit)
    p_free <- ptable$free > 0
    mc_est <- split(mc_est0, row(mc_est0))
    # set_est_i() supports both mc and boot
    out_all <- lapply(mc_est, set_est_i,
                        fit = fit,
                        p_free = p_free)
    out_all
  }

# Get the implied statistics from stored parameter estimates
#' @noRd

mc2implied <- function(fit) {
    if (is.null(fit@external$manymome$mc)) {
        stop("Monte Carlo estimates not found. Please run do_mc() first.")
      }
    # NOTE: Support fixed.x = TRUE
    mc_est0 <- fit@external$manymome$mc
    mc_est <- split(mc_est0, row(mc_est0))
    # get_implied_i() supports both mc and boot
    out_all <- lapply(mc_est, get_implied_i,
                        fit = fit)
    out_all
  }