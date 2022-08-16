#' @title Index of Moderated Mediation and Index of Moderated
#'   Moderated Mediation
#'
#' @description Compute the index of moderated mediation and
#'   the index of moderated moderated mediation.
#'
#' @details
#' The function [index_of_mome()] computes the index of moderated
#' mediation proposed by Hayes. It supports any path in a model,
#' with one component path moderated.
#'
#' The function [index_of_momome()] computes the index of
#' moderated mediation proposed by Hayes. It supports any path
#' in a model, with two component paths each moderated by
#' one moderator.
#'
#' @return
#' It returns an `cond_indirect_diff`-class object. This class has a
#' `print` method ([print.cond_indirect_diff()]), a `coef` method
#' ([coef.cond_indirect_diff()]), and a `confint` method
#' ([confint.cond_indirect_diff()]).
#'
#'
#' @param x Character. The name of the predictor at the start of the
#'    path.
#' @param y Character. The name of the outcome variable at the end of
#'    the path.
#' @param m A vector of the variable names of the moderators. The
#'    path goes from the first mediator successively to the last
#'    mediator. If `NULL`, the default, the path goes from `x` to
#'    `y`.
#' @param w Character. The name of the moderator.
#' @param z Character. The name of the second moderator, for computing
#'   the index of moderated moderated mediation.
#' @param fit The fit object. Can be a
#'    [lavaan::lavaan-class] object or a list of [lm()] outputs.
#' @param boot_ci Logical. Whether bootstrap confidence interval will
#'    be formed. Default is `FALSE`.
#' @param level The level of confidence for the bootstrap confidence
#'    interval. Default is .95.
#' @param boot_out If `boot_ci` is `TRUE`, users can supply
#'    pregenerated bootstrap estimates. This can be the output of
#'    [do_boot()]. For [cond_indirect_effects()], this can be
#'    the output of previous call to [cond_indirect_effects()],
#'    [indirect_effect()], or [cond_indirect()] with
#'    bootstrap estimates. These stored estimates will be reused such that
#'    there is no need to do bootstrapping again. If not supplied, the
#'    function will try to generate them from `fit`.
#' @param R Integer. If `boot_ci` is `TRUE`, `fit` is a list of [lm()]
#'    outputs, and `boot_out` is `NULL`, this function will do
#'    bootstrapping on `fit`. `R` is the number of bootstrap samples.
#'    Default is 100.
#' @param seed If `boot_ci` is `TRUE`, `fit` is a list of [lm()]
#'    outputs, and `boot_out` is `NULL`, this function will do
#'    bootstrapping on `fit`. This is the seed for the bootstrapping.
#'    Default is `NULL` and seed is not set.
#' @param progress Logical. Display progress or not. Default is `TRUE`.
#' @param ... Arguments to be passed to [cond_indirect_effects()]
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Hayes, A. F. (2015). An index and test of linear moderated mediation.
#' *Multivariate Behavioral Research, 50*(1), 1-22.
#' \doi{10.1080/00273171.2014.962683}
#'
#' Hayes, A. F. (2018). Partial, conditional, and moderated moderated
#' mediation: Quantification, inference, and interpretation.
#' *Communication Monographs, 85*(1), 4-40.
#' \doi{10.1080/03637751.2017.1352100}
#'
#' @examples
#'
#' \dontrun{
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' dat$xw1 <- dat$x * dat$w1
#' mod <-
#' "
#' m1 ~ a * x  + f * w1 + d * xw1
#' y  ~ b * m1 + cp * x
#' ind_mome := d * b
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # R should be at least 2000 or even 5000 in real research.
#' boot_out <- do_boot(fit, R = 500, seed = 4314, parallel = FALSE)
#' out_mome <- index_of_mome(x = "x", y = "y", m = "m1", w = "w1",
#'                           fit = fit,
#'                           boot_ci = TRUE, boot_out = boot_out)
#' out_mome
#' coef(out_mome)
#' # From lavaan
#' print(est[19, ], nd = 8)
#' confint(out_mome)
#' }
#'
#'
#' @export
#' @describeIn index_of_mome Compute the index of
#'   moderated mediation.
#' @order 1

index_of_mome <- function(x,
                          y,
                          m = NULL,
                          w = NULL,
                          fit = NULL,
                          boot_ci = FALSE,
                          level = .95,
                          boot_out = NULL,
                          R = 100,
                          seed = NULL,
                          progress = TRUE,
                          ...) {
    # if (is.null(m) || length(m) != 1) {
    #     stop("The path must have one and only one mediator (m).")
    #   }
    if (is.null(w) || length(w) != 1) {
        stop("The path must have exactly one moderator.")
      }
    mm_w <- mod_levels(w, fit = fit, values = c(0, 1))
    prods <- cond_indirect(wvalues = mm_w[1, ],
                            x = x,
                            y = y,
                            m = m,
                            fit = fit,
                            get_prods_only = TRUE)
    if (length(n_prods(prods)) != 1) {
        stop("The index cannot be computed when there are more than one moderators")
      }
    out <- cond_indirect_effects(wlevels = mm_w, x = x, y = y,
                                 m = m, fit = fit, level = level,
                                 boot_ci = boot_ci,
                                 R = R,
                                 seed = seed,
                                 progress = progress,
                                 boot_out = boot_out,
                                 ...)
    out <- cond_indirect_diff(out, from = 2, to = 1, level = level)
    out$type <- "index_of_mome"
    out
  }

#' @examples
#'
#' \dontrun{
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' dat$xw1 <- dat$x * dat$w1
#' dat$m1w4 <- dat$m1 * dat$w4
#' mod <-
#' "
#' m1 ~ a * x  + f1 * w1 + d1 * xw1
#' y  ~ b * m1 + f4 * w4 + d4 * m1w4 + cp * x
#' ind_momome := d1 * d4
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # R should be at least 2000 or even 5000 in real research.
#' boot_out <- do_boot(fit, R = 500, seed = 4314, parallel = FALSE)
#' out_momome <- index_of_momome(x = "x", y = "y", m = "m1",
#'                           w = "w1", z = "w4",
#'                           fit = fit,
#'                           boot_ci = TRUE, boot_out = boot_out)
#' out_momome
#' coef(out_momome)
#' print(est[32, ], nd = 8)
#' confint(out_momome)
#' }
#'
#' @export
#' @describeIn index_of_mome Compute the index of
#'   moderated moderated mediation.
#' @order 2

index_of_momome <- function(x,
                            y,
                            m = NULL,
                            w = NULL,
                            z = NULL,
                            fit = NULL,
                            boot_ci = FALSE,
                            level = .95,
                            boot_out = NULL,
                            R = 100,
                            seed = NULL,
                            progress = TRUE,
                            ...) {
    # if (is.null(m) || length(m) != 1) {
    #     stop("The path must have one and only one mediator (m).")
    #   }
    if (is.null(w) || is.null(z) ||
        length(w) != 1 || length(z) != 1) {
        stop("The path must have exactly two moderators.")
      }
    mm_w <- mod_levels(w, fit = fit, values = c(0, 1))
    mm_z <- mod_levels(z, fit = fit, values = c(0, 1))
    mm <- merge_mod_levels(mm_w, mm_z)
    prods <- cond_indirect(wvalues = mm[1, ],
                            x = x,
                            y = y,
                            m = m,
                            fit = fit,
                            get_prods_only = TRUE)
    if (!all.equal(n_prods(prods), c(1, 1), check.attributes = FALSE)) {
        stop("The index can be computed only when there are exactly two moderators.")
      }
    out <- cond_indirect_effects(wlevels = mm, x = x, y = y,
                                 m = m, fit = fit, level = level,
                                 boot_ci = boot_ci,
                                 R = R,
                                 seed = seed,
                                 progress = progress,
                                 boot_out = boot_out,
                                 ...)
    i0 <- cond_indirect_diff(out, from = 4, to = 3, level = level)
    i1 <- cond_indirect_diff(out, from = 2, to = 1, level = level)
    ind <- stats::coef(i1) - stats::coef(i0)
    ind_boot <- i1$boot_diff - i0$boot_diff
    levels0 <- c((1 - level) / 2, 1 - (1 - level) / 2)
    ind_ci <- stats::quantile(ind_boot, probs = levels0)
    out <- list(index = ind,
                ci = ind_ci,
                level = level,
                from = i0$from,
                to = i0$to,
                from1 = i1$from,
                to1 = i1$to,
                output = out,
                boot_diff = ind_boot,
                type = "index_of_momome")
    class(out) <- c("cond_indirect_diff", class(out))
    out
  }

# Check the number of product terms
#' @noRd

n_prods <- function(prods) {
    isna <- sapply(prods, function(x) all(is.na(x)))
    if (all(isna)) {
        return(0)
      }
    prod1 <- prods[!isna]
    np <- sapply(prod1, function(x) length(unlist(x$prod)))
    return(np)
  }
