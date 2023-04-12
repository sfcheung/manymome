#' @title Math Operators for
#' 'indirect'-Class Objects
#'
#' @description Mathematic operators for
#' 'indirect'-class object, the output
#' of [indirect_effect()] and
#' [cond_indirect()].
#'
#' @details For now, only `+` operator
#' and `-` operator are supported. These
#' operators can be used to estimate and
#' test a function of effects between
#' the same pair of variables but along
#' different paths.
#'
#' For example, they can be used to
#' compute and test the total effects
#' along different paths. They can also
#' be used to compute and test the
#' difference between the effects along
#' two paths.
#'
#' The operators will check whether an
#' operation is valid. An operation is
#' not valid if
#'
#' 1. the two paths do not start from
#' the same variable,
#'
#' 2. the two paths do not end at the
#' same variable, (c) a path appears in
#' both objects,
#'
#' 3. moderators are involved but they
#' are not set to the same values in
#' both objects, and
#'
#' 4. bootstrap estimates stored in
#' `boot_out`, if any, are not identical.
#'
#' 5. Monte Carlo simulated
#' estimates stored in
#' `mc_out`, if any, are not identical.
#'
#' @return An 'indirect'-class object
#' with a list of effects stored. See
#' [indirect_effect()] on details for
#' this class.
#'
#' @param e1 An 'indirect'-class object.
#'
#' @param e2 An 'indirect'-class object.
#'
#'
#' @seealso [indirect_effect()] and
#' [cond_indirect()]
#'
#' @name math_indirect
NULL

#' @rdname math_indirect
#'
#' @examples
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x  + d1 * w1 + e1 * x:w1
#' m2 ~ m1 + a2 * x
#' y  ~ b1 * m1 + b2 * m2 + cp * x
#' "
#' fit <- sem(mod, dat,
#'            meanstructure = TRUE, fixed.x = FALSE,
#'            se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#' hi_w1 <- mean(dat$w1) + sd(dat$w1)
#'
#' # Examples for cond_indirect():
#'
#' # Conditional effect from x to m1 when w1 is 1 SD above mean
#' out1 <- cond_indirect(x = "x", y = "y", m = c("m1", "m2"),
#'               wvalues = c(w1 = hi_w1), fit = fit)
#' out2 <- cond_indirect(x = "x", y = "y", m = c("m2"),
#'               wvalues = c(w1 = hi_w1), fit = fit)
#' out3 <- cond_indirect(x = "x", y = "y",
#'               wvalues = c(w1 = hi_w1), fit = fit)
#'
#' out12 <- out1 + out2
#' out12
#' out123 <- out1 + out2 + out3
#' out123
#' coef(out1) + coef(out2) + coef(out3)
#'
#' @export
`+.indirect` <- function(e1, e2) {
    plusminus(e1, e2, op = "+")
  }

#' @rdname math_indirect
#' @export
`-.indirect` <- function(e1, e2) {
    plusminus(e1, e2, op = "-")
  }

#' @noRd

plusminus <- function(e1, e2, op = c("+", "-")) {
    op <- match.arg(op, c("+", "-"))
    check_xy(e1, e2)
    cp1 <- if (is.list(e1$components)) e1$components else list(e1$components)
    cp2 <- if (is.list(e2$components)) e2$components else list(e2$components)
    cp0 <- c(cp1, cp2)
    cpc1 <- if (is.list(e1$components_conditional)) e1$components_conditional else list(e1$components_conditional)
    cpc2 <- if (is.list(e2$components_conditional)) e2$components_conditional else list(e2$components_conditional)
    cpc0 <- c(cpc1, cpc2)
    m1 <- if (is.list(e1$m)) e1$m else list(e1$m)
    m2 <- if (is.list(e2$m)) e2$m else list(e2$m)
    m0 <- c(m1, m2)
    cv1 <- if (is.list(e1$computation_values)) e1$computation_values else list(e1$computation_values)
    cv2 <- if (is.list(e2$computation_values)) e2$computation_values else list(e2$computation_values)
    cv0 <- c(cv1, cv2)
    cs1 <- if (is.list(e1$computation_symbols)) e1$computation_symbols else list(e1$computation_symbols)
    cs2 <- if (is.list(e2$computation_symbols)) e2$computation_symbols else list(e2$computation_symbols)
    cs0 <- c(cs1, cs2)
    ca1 <- if (is.list(e1$call)) e1$call else list(e1$call)
    ca2 <- if (is.list(e2$call)) e2$call else list(e2$call)
    ca0 <- c(ca1, ca2)
    est0 <- switch(op,
                   "+" = e1$indirect + e2$indirect,
                   "-" = e1$indirect - e2$indirect)
    level0 <- e1$level
    has_ci <- FALSE
    ci_type <- NULL
    if (!is.null(e1$boot_indirect) && !is.null(e2$boot_indirect)) {
        has_ci <- TRUE
        ci_type <- "boot"
        ind_name <- "boot_indirect"
      }
    if (!is.null(e1$mc_indirect) && !is.null(e2$mc_indirect)) {
        has_ci <- TRUE
        ci_type <- "mc"
        ind_name <- "mc_indirect"
      }
    if (has_ci) {
        if (op == "+") {
            bind0 <- e1[[ind_name]] + e2[[ind_name]]
          }
        if (op == "-") {
            bind0 <- e1[[ind_name]] - e2[[ind_name]]
          }
      } else {
        bind0 <- NULL
      }
    if (!is.null(bind0)) {
        nboot <- length(bind0)
        tmp <- list(t = matrix(bind0, nrow = nboot, ncol = 1),
                    t0 = est0,
                    R = nboot)
        boot_ci0 <- boot::boot.ci(tmp, conf = level0, type = "perc")
        boot_ci1 <- boot_ci0$percent[4:5]
        names(boot_ci1) <- paste0(formatC(c(100 * (1 - level0) / 2,
                                      100 * (1 - (1 - level0) / 2)), 2,
                                      format = "f"), "%")
        bci0 <- boot_ci1
        bp0 <- est2p(est0)
      } else {
        bci0 <- NULL
        bp0 <- NULL
      }
    op1 <- e1$op
    op2 <- e2$op
    if (is.null(op1)) {
        if (is.null(e1$m)) {
            op1 <- paste0(e1$x, "->", e1$y)
          } else {
            op1 <- paste0(e1$x, "->",
                            paste(eval(e1$m), collapse = "->"),
                            "->", e1$y)
          }
      }
    if (is.null(op2)) {
        if (is.null(e2$m)) {
            op2 <- paste0(e2$x, "->", e2$y)
          } else {
            op2 <- paste0(e2$x, "->",
                            paste(eval(e2$m), collapse = "->"),
                            "->", e2$y)
          }
      }
    op0 <- paste0("(", op1, ")",
                  "\n", op, "(", op2, ")")
    bind0_boot <- NULL
    bci0_boot <- NULL
    bp0_boot <- NULL
    bind0_mc <- NULL
    bci0_mc <- NULL
    if (has_ci) {
        if (ci_type == "boot") {
            bind0_boot <- bind0
            bci0_boot <- bci0
            bp0_boot <- bp0
          }
        if (ci_type == "mc") {
            bind0_mc <- bind0
            bci0_mc <- bci0
          }
      }
    out <- list(indirect = est0,
                indirect_raw = e1$indirect_raw + e2$indirect_raw,
                components = cp0,
                components_conditional = cpc0,
                scale_x = e1$scale_x,
                scale_y = e1$scale_y,
                standardized_x = e1$standardized_x,
                standardized_y = e1$standardized_y,
                wvalues = join_wvalues(e1, e2),
                x = e1$x,
                y = e1$y,
                m = m0,
                computation_values = cv0,
                computation_symbols = cs0,
                call = ca0,
                op = op0,
                boot_indirect = bind0_boot,
                boot_ci = bci0_boot,
                boot_p = bp0_boot,
                mc_indirect = bind0_mc,
                mc_ci = bci0_mc,
                level = level0,
                boot_out = e1$boot_out,
                mc_out = e1$mc_out
                )
    class(out) <- c("indirect", class(out))
    out
  }

# Check if the two objects are eligible for "+" and "-" operations
#' @noRd

check_xy <- function(e1, e2) {
    x1 <- e1$x
    x2 <- e2$x
    y1 <- e1$y
    y2 <- e2$y
    m1 <- e1$m
    m2 <- e2$m
    stdx1 <- e1$standardized_x
    stdy1 <- e1$standardized_y
    stdx2 <- e2$standardized_x
    stdy2 <- e2$standardized_y
    wv1 <- e1$wvalues
    wv2 <- e2$wvalues
    scx1 <- e1$scale_x
    scy1 <- e1$scale_y
    scx2 <- e2$scale_x
    scy2 <- e2$scale_y
    if (!identical(x1, x2)) {
        stop("The objects to be added do not have the same 'x'.")
      }
    if (!identical(y1, y2)) {
        stop("The objects to be added do not have the same 'y'.")
      }
    if (!is.list(m1)) {
        m1 <- list(m1)
      }
    if (!is.list(m2)) {
        m2 <- list(m2)
      }
    m1m2_chk <- intersect(m1, m2)
    if (length(m1m2_chk) != 0) {
        stop("The objects have one or more paths in common.")
      }
    if (!identical(stdx1, stdx2)) {
        stop("x is standardized in one object but not in the other.")
      }
    if (!identical(stdy1, stdy2)) {
        stop("y is standardized in one object but not in the other.")
      }
    wvnames <- union(names(wv1), names(wv2))
    if (!is.null(wv1) && !is.null(wv2)) {
        for (i in wvnames) {
            i1 <- wv1[i]
            i2 <- wv2[i]
            if (!is.na(i1) && !is.na(i2)) {
                if (!identical(i1, i2)) {
                    stop("Some of the wvalues are not identical in both objects.")
                  }
              }
          }
      }
    if (!identical(scx1, scx2)) {
        stop("x is not scaled by the same factor (SD) in the two objects.")
      }
    if (!identical(scy1, scy2)) {
        stop("y is not scaled by the same factor (SD) in the two objects.")
      }
    if (!identical(e1$level, e2$level)) {
        stop("The two objects do not have the same level for confidence interval.")
      }
    if (!identical(e1$boot_out, e2$boot_out)) {
        stop("The two objects do not have the same object in boot_out.")
      }
    if (!identical(e1$mc_out, e2$mc_out)) {
        stop("The two objects do not have the same object in mc_out.")
      }
    return(TRUE)
  }

# Join the wvalues in the two elements
#' @noRd

join_wvalues <- function(e1, e2) {
    wv1 <- e1$wvalues
    wv2 <- e2$wvalues
    if (is.null(wv1) && is.null(wv2)) {
        return(NULL)
      }
    if (is.null(wv1)) {
        return(wv2)
      }
    if (is.null(wv2)) {
        return(wv1)
      }
    wvnames <- setdiff(names(wv2), names(wv1))
    if (length(wvnames) == 0) {
        return(wv1)
      }
    wv0 <- c(wv1, wv2[wvnames])
    return(wv0)
  }