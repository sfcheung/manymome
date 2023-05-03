#' @title Monte Carlo Estimates for a
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
#' Monte Carlo replications. Each element
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
#' fit <- sem(model = mod, data = dat, fixed.x = FALSE,
#'            baseline = FALSE)
#' # In real research, R should be 5000 or even 10000.
#' fit <- gen_mc_est(fit, R = 100, seed = 453253)
#' fit_mc_out <- fit2mc_out(fit)
#' out <- cond_indirect_effects(wlevels = "w",
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = fit,
#'                              mc_ci = TRUE,
#'                              mc_out = fit_mc_out)
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
    ptable <- lav_ptable(fit)
    p_free <- ptable$free > 0
    mc_est <- split(mc_est0, row(mc_est0))
    # set_est_i() supports both mc and boot
    if (inherits(fit, "lavaan")) {
        est_df0 <- lav_est(fit,
                           se = FALSE,
                           ci = FALSE,
                           rsquare = FALSE)
      }
    if (inherits(fit, "lavaan.mi")) {
        est_df0 <- lav_est(fit,
                           se = FALSE,
                           ci = FALSE)
      }
    out_all <- lapply(mc_est, set_est_i,
                        fit = fit,
                        p_free = p_free,
                        est_df = est_df0)
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
    if (inherits(fit, "lavaan.mi")) {
        fit_tmp <- methods::new("lavaan",
                      version = as.character(utils::packageVersion("lavaan")))
        fit_tmp@Model <- fit@Model
        fit_tmp@Data <- fit@Data
        fit_tmp@ParTable <- fit@ParTableList[[1]]
        fit_tmp@pta <- fit@pta
        fit_tmp@Options <- fit@Options
      } else {
        fit_tmp <- NULL
      }
    # get_implied_i() supports both mc and boot
    out_all <- lapply(mc_est, get_implied_i,
                        fit = fit,
                        fit_tmp = fit_tmp)
    out_all
  }
