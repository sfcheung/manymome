#' @title Indirect Effect (No Bootstrapping)
#'
#' @description It computes an indirect effect, optionally conditional on
#'   the value(s) of moderator(s) if present.
#'
#' @details This function is a low-level function called by
#'   [indirect_effect()], [cond_indirect_effects()], and
#'   [cond_indirect()], which call this function multiple times if
#'   bootstrap confidence interval is requested.
#'
#' This function usually should not be used directly. It is exported for
#'   advanced users and developers
#'
#' @return It returns an `indirect`-class object. This class has the
#'   following methods: [coef.indirect()], [print.indirect()]. The
#'   [confint.indirect()] method is used only when called by
#'   [cond_indirect()] or [cond_indirect_effects()].
#'
#' @param x Character. The name of the predictor at the start of the
#'   path.
#' @param y Character. The name of the outcome variable at the end of
#'   the path.
#' @param m A vector of the variable names of the mediator(s). The path
#'   goes from the first mediator successively to the last mediator.
#'   If `NULL`, the default, the path goes from `x` to `y`.
#' @param fit The fit object. Currently only supports
#'   [lavaan::lavaan-class] objects. Support for lists of [lm()]
#'   output is implemented by high level functions such as
#'   [indirect_effect()] and [cond_indirect_effects()].
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`,
#'   the default, it will be generated from `fit`. If supplied, `fit`
#'   will be ignored.
#' @param implied_stats Implied means, variances, and covariances of
#'   observed variables and latent variables (if any), of the form of
#'   the output of [lavaan::lavInspect()] with `what` set to
#'   `"implied"`, but with means extracted with `what` set to
#'   `"mean.ov"` and `"mean.lv"`. The standard deviations are
#'   extracted from this object for standardization. Default is
#'   `NULL`, and implied statistics will be computed from `fit` if
#'   required.
#' @param wvalues A numeric vector of named elements. The names are
#'   the variable names of the moderators, and the values are the
#'   values to which the moderators will be set to. Default is `NULL`.
#' @param standardized_x Logical. Whether `x` will be standardized.
#'   Default is `FALSE`.
#' @param standardized_y Logical. Whether `y` will be standardized.
#'   Default is `FALSE`.
#' @param computation_digits The number of digits in storing the
#'   computation in text. Default is 3.
#' @param prods The product terms found. For internal use.
#' @param get_prods_only IF `TRUE`, will quit early and return the
#'   product terms found. The results can be passed to the `prod`
#'   argument when calling this function. Default is `FALSE`.
#'   For internal use.
#' @param data Data frame (optional). If supplied, it will be used to
#'   identify the product terms. For internal use.
#' @param expand Whether products of more than two terms will be
#'   searched. `TRUE` by default. For internal use.
#' @param warn If `TRUE`, the default, the function will warn against
#'   possible misspecification, such as not setting the value of a
#'   moderator which moderate one of the component path. Set this to
#'   `FALSE` will suppress these warnings. Suppress them only when the
#'   moderators are omitted intentionally.
#'
#'
#' @seealso [indirect_effect()], [cond_indirect_effects()], and
#'   [cond_indirect()], the high level functions that should usually
#'   be used.
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
#' fit <- sem(mod, dat, meanstructure = TRUE,
#'            fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
#'
#' # Compute the conditional indirect effect by indirect_i()
#' indirect_1 <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
#'                        wvalues = wvalues)
#'
#' # Manually compute the conditional indirect effect
#' indirect_2 <- (est[est$label == "a1", "est"] +
#'                 wvalues["w1"] * est[est$label == "d1", "est"]) *
#'               (est[est$label == "a2", "est"] +
#'                 wvalues["w2"] * est[est$label == "d2", "est"]) *
#'               (est[est$label == "a3", "est"] +
#'                 wvalues["w3"] * est[est$label == "d3", "est"]) *
#'               (est[est$label == "a4", "est"] +
#'                 wvalues["w4"] * est[est$label == "d4", "est"])
#' # They should be the same
#' coef(indirect_1)
#' indirect_2
#'
#' @export
#'
#'

indirect_i <- function(x,
                     y,
                     m = NULL,
                     fit = NULL,
                     est = NULL,
                     implied_stats = NULL,
                     wvalues = NULL,
                     standardized_x = FALSE,
                     standardized_y = FALSE,
                     computation_digits = 5,
                     prods = NULL,
                     get_prods_only = FALSE,
                     data = NULL,
                     expand = TRUE,
                     warn = TRUE) {
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    chkpath <- check_path(x = x, y = y, m = m, fit = fit, est = est)
    if (!chkpath) {
        msg <- paste0("No path from ", sQuote(x), " to ", sQuote(y),
                      ifelse(is.null(m), "",
                                         paste0(" through ",
                                                paste0(sQuote(m), collapse = ", "))),
                      ". ",
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
    chk_lv <- unique(c(xs, ys)) %in% check_lv_in_est(est)
    if (isTRUE(any(chk_lv)) && !isTRUE(all(chk_lv))) {
        stop("Does not support paths with both latent and observed variables")
      }
    if (is.null(prods)) {
        if (isTRUE(all(chk_lv))) {
            prods <- mapply(get_prod,
                            x = xs,
                            y = ys,
                            operator = "_x_",
                            MoreArgs = list(est = est),
                            SIMPLIFY = FALSE)
          } else {
            if (is.null(data)) {
                # Try to get the data from fit
                if (!is.null(fit)) {
                    fit_type <- cond_indirect_check_fit(fit)
                    data <- switch(fit_type,
                                  lavaan = lavaan::lavInspect(fit, "data"),
                                  lm = lm2ptable(fit)$data)
                  }
              }
            if (!is.null(fit)) {
                prods <- mapply(get_prod,
                                x = xs,
                                y = ys,
                                MoreArgs = list(fit = fit,
                                                data = data,
                                                expand = expand),
                                SIMPLIFY = FALSE)
              } else {
                prods <- mapply(get_prod,
                                x = xs,
                                y = ys,
                                MoreArgs = list(est = est,
                                                data = data,
                                                expand = expand),
                                SIMPLIFY = FALSE)
              }
          }
      } else {
        # prods is supplied.
        # Need to update the estimates
        prods <- update_prods(prods, est)
      }
    if (get_prods_only) return(prods)
    names(prods) <- ys
    if (!is.null(wvalues)) {
        tmpfct <- function(xi) {
            if (all(is.na(xi))) return(0)
            if (is.null(xi$prod)) return(0)
            b_i <- xi$b
            w_i <- xi$w
            if (is.list(w_i)) {
                w_i0 <- sapply(w_i, paste0, collapse = ":")
              } else {
                w_i0 <- w_i
              }
            wvalues_i <- mapply(function(b1, w1, wvalues) {
                              prod(wvalues[w1])
                            },
                            b1 = b_i,
                            w1 = w_i,
                            MoreArgs = list(wvalues = wvalues))
            # wvalues_i <- wvalues[w_i0]
            wv_na <- is.na(wvalues_i)
            if (isTRUE(any(wv_na))) {
                wvalues_i[wv_na] <- 0
                names(wvalues_i) <- w_i0
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
            implied_stats <- lav_implied_all(fit)
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

#' @noRd

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
    if (is.list(w_i)) {
        w_i0 <- sapply(w_i, paste0, collapse = ":")
        w_i1 <- unique(unlist(w_i))
        w_i2 <- sapply(w_i, paste0, collapse = "*")
      } else {
        w_i0 <- w_i
        w_i1 <- w_i
        w_i2 <- w_i
      }
    wvalues_i0 <- mapply(function(w1, wvalues) {
                      paste0(formatC(wvalues[w1], digits = digits, format = "f"),
                             collapse = "*")
                    },
                    w1 = w_i,
                    MoreArgs = list(wvalues = wvalues))
    if (is.null(wvalues)) {
        wvalues_i <- rep(0, length(w_i))
        wvalues_i0 <- "(0)"
        tmp <- paste0(paste0(w_i1, collapse = ", "),
                      " modelled as moderator(s) for the path ",
                      "from ", yiname_old, " to ", y,
                      " but not included in ", sQuote("wvalues"), ". ",
                      "This is equivalent to setting wvalues to zero ",
                      "in computing the effect, ",
                      "which may not be meaningful. Please check.")
        if (warn) warning(tmp)
      } else {
        # wvalues_i <- wvalues[w_i]
        wvalues_i <- mapply(function(b1, w1, wvalues) {
                          b1 * prod(wvalues[w1])
                        },
                        b1 = b_i,
                        w1 = w_i,
                        MoreArgs = list(wvalues = wvalues))
        wv_na <- is.na(wvalues_i)
        if (isTRUE(any(wv_na))) {
            wvalues_i[wv_na] <- 0
            wvalues_i0[wv_na] <- "0"
            names(wvalues_i) <- w_i0
            # tmp0 <- w_i[!(w_i %in% names(wvalues))]
            tmp0 <- unique(unlist(w_i[wv_na]))
            tmp0 <- tmp0[!(tmp0 %in% names(wvalues))]
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
                    paste0("(", b_i0, ")*(", w_i2, ")",
                          collapse = " + "))

    out2 <- paste0("(", formatC(yi, digits = digits, format = "f"),
                    ") + ",
                    paste0("(",
                          formatC(b_i, digits = digits, format = "f"),
                          ")*(",
                          wvalues_i0,
                          ")",
                          collapse = " + "))
    names(out2) <- out1
    out2
  }

#' @noRd

check_lv_in_est <- function(est) {
    unique(est$lhs[est$op == "=~"] )
  }

#' @noRd

update_prods <- function(prods, est) {
    pout <- prods
    tmpfct <- function(prods_i) {
        pout_i <- prods_i
        if (all(is.na(prods_i))) {
            return(pout_i)
          } else {
            est_i <- est[(est$lhs == prods_i$y) &
                        (est$op == "~") &
                        (est$rhs %in% prods_i$prod), "est"]
            pout_i$b <- est_i
            names(pout_i$b) <- prods_i$prod
            return(pout_i)
          }
      }
    pout <- sapply(prods, tmpfct, simplify = FALSE)
    pout
  }