#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + c4 * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' check_path(x = "x", y = "m3", m = c("m1", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m1", "m2"), fit = fit)
#'
#' @noRd
#'
#'

fit2boot_out <- function(fit) {
    boot_est <- boot2est(fit)
    boot_implied <- boot2implied(fit)
    out <- mapply(function(x, y) list(est = x,
                                      implied_stats = y),
                  x = boot_est,
                  y = boot_implied,
                  SIMPLIFY = FALSE)
    names(out) <- names(boot_est)
    out
  }

boot2est <- function(fit) {
    opt <- lavaan::lavInspect(fit, "options")
    if (opt$se != "bootstrap") {
        stop("'se' not set to 'bootstrap' when fitting the model.")
      }
    boot_est0 <- lavaan::lavInspect(fit, "boot")
    ptable <- lavaan::parameterTable(fit)
    p_free <- ptable$free > 0
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- lapply(boot_est, set_est_i,
                        fit = fit,
                        p_free = p_free)
    out_all
  }

boot2implied <- function(fit) {
    opt <- lavaan::lavInspect(fit, "options")
    if (opt$se != "bootstrap") {
        stop("'se' not set to 'bootstrap' when fitting the model.")
      }
    if (opt$fixed.x) {
        stop("'fixed.x' set to TRUE is not supported.")
      }
    boot_est0 <- lavaan::lavInspect(fit, "boot")
    boot_est <- split(boot_est0, row(boot_est0))
    out_all <- lapply(boot_est, get_implied_i,
                        fit = fit)
    out_all
  }

set_est_i <- function(est0, fit, p_free) {
    fit@ParTable$est[p_free] <- unname(est0)
    est0 <- lavaan::parameterEstimates(fit,
                                       se = FALSE,
                                       zstat = FALSE,
                                       pvalue = FALSE,
                                       ci = FALSE,
                                       rsquare = TRUE,
                                       remove.eq = FALSE,
                                       remove.ineq = FALSE,
                                       remove.def = FALSE,
                                       remove.nonfree = FALSE,
                                       remove.step1 = FALSE)
    est0
  }

get_implied_i <- function(est0, fit) {
    # fit@ParTable$est[p_free] <- unname(est0)
    # fit@Model@GLIST <- lavaan::lav_model_set_parameters(fit@Model,
    #                                                     est0)@GLIST
    # implied <- lavaan::lavInspect(fit, "implied")
    # implied
    mod0 <- lavaan::lav_model_set_parameters(fit@Model, est0)
    implied <- lavaan::lav_model_implied(mod0,
                                         GLIST = NULL,
                                         delta = TRUE)
    out <- lavaan::lavInspect(fit, "implied")
    out_names <- names(out)
    out1 <- out
    for (x in out_names) {
        out1[[x]][] <- implied[[x]][[1]]
      }
    out1
  }