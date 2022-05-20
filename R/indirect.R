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
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
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

indirect <- function(x,
                     y,
                     m = NULL,
                     fit = NULL,
                     est = NULL,
                     implied_stats = NULL,
                     wvalues = NULL,
                     standardized_x = FALSE,
                     standardized_y = FALSE) {
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    # if (is.null(m)) {
    #     out <- get_b(x, y, fit)
    #     return(out)
    #   }
    y0 <- y
    p <- length(m) + 1
    bs <- rep(NA, p)
    xs <- c(x, m)
    ys <- c(m, y)
    bs_names <- paste0(ys, "~", xs)
    bs <- mapply(get_b,
                 x = xs,
                 y = ys,
                 MoreArgs = list(est = est))
    bs_org <- bs
    names(bs_org) <- bs_names
    prods <- mapply(get_prod,
                    x = xs,
                    y = ys,
                    MoreArgs = list(est = est),
                    SIMPLIFY = FALSE)
    names(prods) <- ys
    if (!is.null(wvalues)) {
        tmpfct <- function(xi) {
            if (all(is.na(xi))) return(0)
            if (is.null(xi$prod)) return(0)
            b_i <- xi$b
            w_i <- xi$w
            wvalues_i <- wvalues[w_i]
            sum(b_i * wvalues_i)
          }
        b_cond <- sapply(prods, tmpfct)
        bs <- bs + b_cond
      } else {
        b_cond <- rep(NA, length(bs))
      }
    names(bs) <- bs_names
    b_all <- prod(bs)
    scale_x <- 1
    scale_y <- 1
    if (standardized_x || standardized_y) {
        if (is.null(implied_stats)) {
            implied_stats <- lavaan::lavInspect(fit, "implied")
          }
        if (standardized_x) {
            scale_x <- sqrt(diag(implied_stats$cov)[x])
          }
        if (standardized_y) {
            scale_y <- sqrt(diag(implied_stats$cov)[y])
          }
      }
    b_all_final <- b_all * scale_x / scale_y
    out <- list(indirect = b_all_final,
                indirect_raw = b_all,
                components = bs_org,
                components_conditional = bs,
                call = match.call(),
                scale_x = scale_x,
                scale_y = scale_y,
                standardized_x = standardized_x,
                standardized_y = standardized_y,
                wvalues = wvalues,
                x = x,
                y = y,
                m = m)
    class(out) <- "indirect"
    return(out)
  }
