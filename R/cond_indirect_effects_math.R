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
#' the same pair of variables.
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
#' same variable,
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
#' ## Multigroup Models
#'
#' Since Version 0.1.14.2, support for
#' multigroup models has been added for models
#' fitted by `lavaan`. Both bootstrapping
#' and Monte Carlo confidence intervals
#' are supported. These operators can
#' be used to compute and test the
#' difference of an indirect effect
#' between two groups. This can also
#' be used to compute and test the
#' difference between a function of
#' effects between groups, for example,
#' the total indirect effects between
#' two groups.
#'
#' The operators are flexible and allow
#' users to do many possible computations.
#' Therefore, users need to make sure
#' that the function of effects is
#' meaningful.
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
#' # Multigroup model with indirect effects
#'
#' dat <- data_med_mg
#' mod <-
#' "
#' m ~ x + c1 + c2
#' y ~ m + x + c1 + c2
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE,
#'            group = "group")
#'
#' # If a model has more than one group,
#' # the argument 'group' must be set.
#' ind1 <- indirect_effect(x = "x",
#'                         y = "y",
#'                         m = "m",
#'                         fit = fit,
#'                         group = "Group A")
#' ind1
#' ind2 <- indirect_effect(x = "x",
#'                         y = "y",
#'                         m = "m",
#'                         fit = fit,
#'                         group = 2)
#' ind2
#'
#' # Compute the difference in indirect effects between groups
#' ind2 - ind1
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
    # group_number and group_label can be vectors
    group_number_1 <- e1$group_number
    group_number_2 <- e2$group_number
    group_label_1 <- e1$group_label
    group_label_2 <- e2$group_label
    if (is.numeric(group_number_1) && is.numeric(group_number_2)) {
        has_group <- TRUE
        group_labels <- c(group_label_1, group_label_2)
      } else {
        has_group <- FALSE
      }
    cp1 <- if (is.list(e1$components)) e1$components else list(e1$components)
    cp2 <- if (is.list(e2$components)) e2$components else list(e2$components)
    cp0 <- c(cp1, cp2)
    if (has_group) names(cp0) <- group_labels
    cpc1 <- if (is.list(e1$components_conditional)) e1$components_conditional else list(e1$components_conditional)
    cpc2 <- if (is.list(e2$components_conditional)) e2$components_conditional else list(e2$components_conditional)
    cpc0 <- c(cpc1, cpc2)
    if (has_group) names(cpc0) <- group_labels
    m1 <- if (is.list(e1$m)) e1$m else list(e1$m)
    m2 <- if (is.list(e2$m)) e2$m else list(e2$m)
    m0 <- c(m1, m2)
    if (has_group) names(m0) <- group_labels
    cv1 <- if (is.list(e1$computation_values)) e1$computation_values else list(e1$computation_values)
    cv2 <- if (is.list(e2$computation_values)) e2$computation_values else list(e2$computation_values)
    cv0 <- c(cv1, cv2)
    if (has_group) names(cv0) <- group_labels
    cs1 <- if (is.list(e1$computation_symbols)) e1$computation_symbols else list(e1$computation_symbols)
    cs2 <- if (is.list(e2$computation_symbols)) e2$computation_symbols else list(e2$computation_symbols)
    cs0 <- c(cs1, cs2)
    if (has_group) names(cs0) <- group_labels
    ca1 <- if (is.list(e1$call)) e1$call else list(e1$call)
    ca2 <- if (is.list(e2$call)) e2$call else list(e2$call)
    ca0 <- c(ca1, ca2)
    if (has_group) names(ca0) <- group_labels
    gnumber0 <- c(group_number_1, group_number_2)
    glabel0 <- c(group_label_1, group_label_2)
    est0 <- switch(op,
                   "+" = e1$indirect + e2$indirect,
                   "-" = e1$indirect - e2$indirect)
    est0_raw <- switch(op,
                   "+" = e1$indirect_raw + e2$indirect_raw,
                   "-" = e1$indirect_raw - e2$indirect_raw)
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
        boot_ci1 <- boot_ci_internal(t0 = est0,
                            t = bind0,
                            level = level0,
                            boot_ci_type = "perc")
        bci0 <- boot_ci1
        bp0 <- est2p(bind0)
        bse0 <- stats::sd(bind0, na.rm = TRUE)
      } else {
        bci0 <- NULL
        bp0 <- NULL
        bse0 <- NULL
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
        if (has_group && (length(group_label_1) == 1)) {
            op1 <- paste0(group_label_1, "[",
                          group_number_1, "]: ", op1)
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
        if (has_group && (length(group_label_2) == 1)) {
            op2 <- paste0(group_label_2, "[",
                          group_number_2, "]: ", op2)
          }
      }
    op0 <- paste0("(", op1, ")",
                  "\n", op, "(", op2, ")")
    bind0_boot <- NULL
    bci0_boot <- NULL
    bp0_boot <- NULL
    bind0_mc <- NULL
    bci0_mc <- NULL
    bse0_boot <- NULL
    bse0_mc <- NULL
    if (has_ci) {
        if (ci_type == "boot") {
            bind0_boot <- bind0
            bci0_boot <- bci0
            bp0_boot <- bp0
            bse0_boot <- bse0
          }
        if (ci_type == "mc") {
            bind0_mc <- bind0
            bci0_mc <- bci0
            bse0_mc <- bse0
          }
      }
    if (has_group) {
        scale_x_out <- c(e1$scale_x, e2$scale_x)
        scale_y_out <- c(e1$scale_y, e2$scale_y)
      } else {
        scale_x_out <- e1$scale_x
        scale_y_out <- e1$scale_y
      }
    if (has_group) {
        out_boot_scale_x <- join_sim_scale_factor(e1 = e1,
                                                  e2 = e2,
                                                  scaled = "x",
                                                  prefix = "boot")
        out_boot_scale_y <- join_sim_scale_factor(e1 = e1,
                                                  e2 = e2,
                                                  scaled = "y",
                                                  prefix = "boot")
        out_mc_scale_x <- join_sim_scale_factor(e1 = e1,
                                                  e2 = e2,
                                                  scaled = "x",
                                                  prefix = "mc")
        out_mc_scale_y <- join_sim_scale_factor(e1 = e1,
                                                  e2 = e2,
                                                  scaled = "y",
                                                  prefix = "mc")
      } else {
        out_boot_scale_x <- e1$boot_scale_x
        out_boot_scale_y <- e1$boot_scale_y
        out_mc_scale_x <- e1$mc_scale_x
        out_mc_scale_y <- e1$mc_scale_y
      }
    # TODO (BC): Store boot_ci_type
    out <- list(indirect = est0,
                indirect_raw = est0_raw,
                components = cp0,
                components_conditional = cpc0,
                scale_x = scale_x_out,
                scale_y = scale_y_out,
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
                boot_se = bse0_boot,
                boot_scale_x = out_boot_scale_x,
                boot_scale_y = out_boot_scale_y,
                mc_indirect = bind0_mc,
                mc_ci = bci0_mc,
                mc_se = bse0_mc,
                mc_scale_x = out_mc_scale_x,
                mc_scale_y = out_mc_scale_y,
                level = level0,
                boot_out = e1$boot_out,
                mc_out = e1$mc_out,
                group_number = gnumber0,
                group_label = glabel0
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
    group1 <- e1$group_number
    group2 <- e2$group_number
    if (is.numeric(group1) && is.numeric(group2)) {
        has_group <- TRUE
      } else {
        has_group <- FALSE
      }
    if ((is.null(group1) && is.numeric(group2)) ||
        (is.null(group2) && is.numeric(group1))) {
        stop("The objects do not agree in the number of groups.")
      }
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
    # Disable this test.
    # - The two effects can be two conditional effects.
    # - The two effects can be from two different groups.
    # if (length(m1m2_chk) != 0) {
    #     stop("The objects have one or more paths in common.")
    #   }
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
    if (!has_group) {
        if (!identical(scx1, scx2)) {
            stop("x is not scaled by the same factor (SD) in the two objects.")
          }
        if (!identical(scy1, scy2)) {
            stop("y is not scaled by the same factor (SD) in the two objects.")
          }
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

#' @noRd

join_sim_scale_factor <- function(e1, e2,
                                  scaled = c("x", "y"),
                                  prefix = c("boot", "mc")) {
    scaled <- match.arg(scaled)
    prefix <- match.arg(prefix)
    scaled_name <- paste0(prefix, "_scale_", scaled)
    # Assume e1 and e2 are consistent in standardization
    if (!is.numeric(e1[scaled_name]) &&
        !is.list(e1[scaled_name])) {
        return(e1[scaled_name])
      }
    if (!is.list(e1[scaled_name])) {
        e1_sim_scale <- list(e1[scaled_name])
      } else {
        e1_sim_scale <- e1[scaled_name]
      }
    if (!is.list(e2[scaled_name])) {
        e2_sim_scale <- list(e2[scaled_name])
      } else {
        e2_sim_scale <- e2[scaled_name]
      }
    out_sim_scale <- c(e1_sim_scale, e2_sim_scale)
    out_sim_scale
  }