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
#' @param x Character. The name of predictor at the start of the pathway.
#' @param y Character. The name of the outcome variable at
#'          the end of the pathway.
#' @param m A vector of the variable names of the
#'          moderators. The pathway goes from the first
#'          mediator successively to the last mediator. If
#'          `NULL`, the default, the pathway goes from `x`
#'          to `y`.
#' @param fit The fit object. Can be a
#'            [lavaan::lavaan-class] object or a list of [lm()] outputs.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`, the
#'            default, it will be generated from `fit`. If supplied,
#'            `fit` will ge ignored.
#' @param implied_stats Implied means, variances, and
#'                covariances of observed variables, of the
#'                form of the output of
#'                [lavaan::lavInspect()] with `what` set to
#'                `"implied"`. The standard deviations are
#'                extracted from this object for
#'                standardization. Default is `NULL`, and
#'                implied statistics will be computed from `fit` if required.
#' @param wvalues A numeric vector of named elements. The names are the variable
#'                names of the moderators, and the values are the values to
#'                which the moderators will be set to. Default is `NULL`.
#' @param standardized_x Logical. Whether `x` will be standardized. Default is
#'                       `FALSE`.
#' @param standardized_y Logical. Whether `y` will be standardized. Default is
#'                       `FALSE`.
#' @param boot_ci Logical. Whether bootstrap confidence interval will be formed.
#'                Default is `FALSE`.
#' @param boot_out If `boot_ci` is `TRUE`, users can supply pregenerated
#'                 bootstrap results. This can be the output of [fit2boot_out()]
#'                 or [lm2boot_out()]. If not supplied, the function will try
#'                 to generate them from `fit`.
#' @param seed If `fit` is a list of [lm()] outputs, `boot_ci` is `TRUE`, and
#'             `boot_out` is `NULL`, this is the seed for the bootstrapping.
#'             Default is `NULL` and seed is not set.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
#' m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
#' m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
#' y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
#'
#' indirect_1 <- indirect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
#'                        wvalues = wvalues)
#' indirect_2 <- (est[est$label == "a1", "est"] +
#'                 wvalues["w1"] * est[est$label == "d1", "est"]) *
#'               (est[est$label == "a2", "est"] +
#'                 wvalues["w2"] * est[est$label == "d2", "est"]) *
#'               (est[est$label == "a3", "est"] +
#'                 wvalues["w3"] * est[est$label == "d3", "est"]) *
#'               (est[est$label == "a4", "est"] +
#'                 wvalues["w4"] * est[est$label == "d4", "est"])
#' indirect_1$indirect
#' indirect_2
#'
#' @export
#'
#'

cond_indirect <- function(x,
                     y,
                     m = NULL,
                     fit = NULL,
                     est = NULL,
                     implied_stats = NULL,
                     wvalues = NULL,
                     standardized_x = FALSE,
                     standardized_y = FALSE,
                     boot_ci = FALSE,
                     boot_out = NULL,
                     seed = NULL) {
    fit_type <- NA
    if (inherits(fit, "lavaan")) {
        fit_type <- "lavaan"
      }
    if (inherits(fit, "list")) {
        tmp <- sapply(fit, inherits, what = "lm")
        if (isTRUE(all(tmp))) {
            fit_type <- "lm"
          } else {
            stop("'fit' is a list but not all the elements are lm outputs.")
          }
      }
    if (is.na(fit_type)) {
        stop("'fit' is neither a lavaan object or a list of lm outputs.")
      }
    if (boot_ci) {
        if (fit_type == "lavaan") {
            opt <- lavaan::lavInspect(fit, "options")
            if (opt$se != "bootstrap") {
                stop("If 'boot_ci' is TRUE, 'se' needs to be 'bootstrap' in fit.")
              }
          }
        if (fit_type == "lm") {
            if (is.null(boot_out)) {
                # Do bootstrap here.
              }
          }
      }
  }
