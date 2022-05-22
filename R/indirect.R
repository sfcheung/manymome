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
#' @param computation_digits The number of digits in storing the computation
#'                           in text. Default is 3.
#' @param warn If `TRUE`, the default, the function will warn against possible
#'             misspecification, such as not setting the value of a moderator
#'             which moderate one of the component path. Set this to `FALSE`
#'             will suppress these warnings. Suppress them only when the
#'             moderators are omitted intentionally.
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
                     standardized_y = FALSE,
                     computation_digits = 5,
                     warn = TRUE) {
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    chkpath <- check_path(x = x, y = y, m = m, fit = fit, est = est)
    if (!chkpath) {
        msg <- paste0("No path from ", sQuote(x), " to ", sQuote(y), ".",
                      "Please check the arguments x, y, and m.")
        stop(msg)
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
            wv_na <- is.na(wvalues_i)
            if (isTRUE(any(wv_na))) {
                wvalues_i[wv_na] <- 0
                names(wvalues_i) <- w_i
              }
            sum(b_i * wvalues_i)
          }
        b_cond <- sapply(prods, tmpfct)
        bs <- bs + b_cond
      } else {
        b_cond <- rep(NA, length(bs))
        # b_all_str0 <- "(Not yet available)"
        # b_all_str1 <- "(Not yet available)"
      }
    b_cond_str <- mapply(gen_computation, xi = prods, yi = bs_org,
                          yiname = names(bs_org),
                          MoreArgs = list(digits = computation_digits,
                                          y = y,
                                          wvalues = wvalues,
                                          warn = warn),
                          USE.NAMES = TRUE,
                          SIMPLIFY = FALSE)
    b_all_str0 <- paste0("(", b_cond_str, ")", collapse = "*")
    b_all_str1 <- paste0("(", sapply(b_cond_str, names), ")", collapse = "*")
    names(bs) <- bs_names
    b_all <- prod(bs)
    scale_x <- 1
    scale_y <- 1
    if (standardized_x || standardized_y) {
        if (is.null(implied_stats)) {
            # implied_stats <- lavaan::lavInspect(fit, "implied")
            implied_stats <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                                  mean = lavaan::lavInspect(fit, "mean.ov"),
                                  mean_lv = lavaan::lavInspect(fit, "mean.lv"))
          }
        if (standardized_x) {
            scale_x <- sqrt(diag(implied_stats$cov)[x])
            b_all_str0 <- paste0(b_all_str0, "*(",
                                 formatC(scale_x, digits = computation_digits,
                                         format = "f"), ")")
            b_all_str1 <- paste0(b_all_str1, "*",
                                 "sd_", names(scale_x))
          }
        if (standardized_y) {
            scale_y <- sqrt(diag(implied_stats$cov)[y])
            b_all_str0 <- paste0(b_all_str0, "/(",
                                 formatC(scale_y, digits = computation_digits,
                                         format = "f"), ")")
            b_all_str1 <- paste0(b_all_str1, "/",
                                 "sd_", names(scale_y))
          }
      }
    b_all_final <- b_all * scale_x / scale_y
    out <- list(indirect = unname(b_all_final),
                indirect_raw = unname(b_all),
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
                m = m,
                computation_values = b_all_str0,
                computation_symbols = b_all_str1)
    class(out) <- "indirect"
    return(out)
  }

gen_computation <- function(xi, yi, yiname, digits = 3, y, wvalues = NULL,
                            warn = TRUE) {
    yiname_old <- yiname
    yiname <- paste0("b.", yiname)
    if (all(is.na(xi)) || is.null(xi$prod)) {
        out <- formatC(yi, digits = digits, format = "f")
        names(out) <- yiname
        return(out)
      }
    b_i <- xi$b
    b_i0 <- paste0("b.", names(b_i))
    w_i <- xi$w
    if (is.null(wvalues)) {
        wvalues_i <- rep(0, length(w_i))
        tmp <- paste0(paste0(w_i, collapse = ", "),
                      " modelled as moderator(s) for the path ",
                      "from ", yiname_old, " to ", y,
                      " but not included in ", sQuote("wvalues"), ". ",
                      "This is equivalent to setting wvalues to zero ",
                      "in computing the effect, ",
                      "which may not be meaningful. Please check.")
        if (warn) warning(tmp)
      } else {
        wvalues_i <- wvalues[w_i]
        wv_na <- is.na(wvalues_i)
        if (isTRUE(any(wv_na))) {
            wvalues_i[wv_na] <- 0
            names(wvalues_i) <- w_i
            tmp0 <- w_i[!w_i %in% names(wvalues)]
            tmp <- paste0(paste0(tmp0, collapse = ", "),
                          " modelled as moderator(s) for the path ",
                          "from ", yiname_old, " to ", y,
                          " but not included in ", sQuote("wvalues"), ". ",
                          "They will be set to zero ",
                          "in computing the conditional effect, ",
                          "which may not be meaningful. Please check.")
            if (warn) warning(tmp)
          }
      }
    y0 <- yiname
    out1 <- paste0(y0, " + ",
                    paste0("(", b_i0, ")*(", w_i, ")",
                          collapse = " + "))
    out2 <- paste0("(", formatC(yi, digits = digits, format = "f"),
                    ") + ",
                    paste0("(",
                          formatC(b_i, digits = digits, format = "f"),
                          ")*(",
                          formatC(wvalues_i, digits = digits, format = "f"),
                          ")",
                          collapse = " + "))
    names(out2) <- out1
    out2
  }