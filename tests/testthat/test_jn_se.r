skip("WIP")
skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Test

dat <- data_med_mod_a
dat$wx <- dat$x * dat$w
dat$w2 <- -1 * dat$w
dat$w2x <- dat$x * dat$w2
mod <-
"
m ~ x + w + wx
y  ~ m + x
"
mod2 <-
"
m ~ x + w2 + w2x
y  ~ m + x
"
fit1 <- lm2list(lm(m ~ x*w + c1 + c2, dat))
fit2 <- lm2list(lm(m ~ x*w2 + c1 + c2, dat))

# Create levels of w1, the moderators
wlevels1 <- mod_levels("w", fit = fit1, sd_from_mean = c(-5, 0, 5))
wlevels2 <- mod_levels("w2", fit = fit2, sd_from_mean = c(-5, 0, 5))

out1 <- cond_indirect_effects(x = "m",
                              y = "y",
                              wlevels = wlevels1,
                              fit = fit1)

johnson_neyman <- function(object = NULL,
                           w_lower = NULL,
                           w_upper = NULL,
                           optimize_method = c("uniroot", "optimize"),
                           extendInt = c("no", "yes", "downX", "upX"),
                           tol = .Machine$double.eps^0.25) {
    optimize_method <- match.arg(optimize_method)
    if (inherits(object, "cond_indirect_effects")) {
        if (cond_indirect_effects_has_groups(object)) {
            stop("Multigroup models are not supported.")
          }
        # Do not use update() because it is not reliable in this context
        x <- attr(object, "x", exact = TRUE)
        y <- attr(object, "y", exact = TRUE)
        m <- attr(object, "m", exact = TRUE)
        wlevels0 <- attr(object, "wlevels", exact = TRUE)
        w <- colnames(wlevels0)[1]
        fit <- attr(object, "fit", exact = TRUE)
        full_output_i <- attr(object, "full_output", exact = TRUE)[[1]]
        out_call <- attr(object, "call", exact = TRUE)
        boot_out <- attr(object, "boot_out", exact = TRUE)
        mc_out <- attr(object, "mc_out", exact = TRUE)
        has_boot_out <- !is.null(boot_out)
        has_mc_out <- !is.null(mc_out)
        ci_type <- ifelse(has_boot_out, "boot",
                          ifelse(has_mc_out, "mc",
                                 NA))
        boot_ci <- has_boot_out
        mc_ci <- has_mc_out
        standardized_x <- full_output_i$standardized_x
        standardized_y <- full_output_i$standardized_y
        if (ncol(wlevels0) != 1) {
            stop("Support only one moderator.")
          }
        if (!is.numeric(wlevels0[, 1])) {
            stop("Support only numeric moderator.")
          }
        if (!has_boot_out && !has_mc_out) {
            stop("Confidence intervals not in 'object'.")
          }
      } else {
        stop("'object' needs to be an output of cond_indirect_effects().")
      }
    w_tmp <- mod_levels(w = w,
                        fit = fit,
                        w_method = "sd",
                        sd_from_mean = c(-10, 10))
    w_tmp <- range(w_tmp[, w])
    if (is.null(w_lower)) {
        w_lower <- w_tmp[1]
      }
    if (is.null(w_upper)) {
        w_upper <- w_tmp[2]
      }
    wlevel_interval <- c(w_lower, w_upper)
    w_lower_ci <- pseudo_johnson_neyman_one_bound(w0 = w_lower,
                                                  object = object,
                                                  type = "ci")
    w_upper_ci <- pseudo_johnson_neyman_one_bound(w0 = w_upper,
                                                  object = object,
                                                  type = "ci")
    w_increasing <- (w_upper_ci[1, 2] > w_lower_ci[1, 2])
    if (w_increasing) {
        w_lb_0 <- list(w = w_lower, objective = w_lower_ci[1, 2])
        w_ub_0 <- list(w = w_upper, objective = w_upper_ci[1, 1])
      } else {
        w_lb_0 <- list(w = w_upper, objective = w_upper_ci[1, 2])
        w_ub_0 <- list(w = w_lower, objective = w_lower_ci[1, 1])
      }
    if (optimize_method == "uniroot") {
        w_lb <- tryCatch(pseudo_johnson_neyman_uniroot(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "lower",
                            extendInt = extendInt,
                            tol = tol),
                         error = function(e) e)
        if (inherits(w_lb, "error")) {
            w_lb <- w_lb_0
            names(w_lb) <- c("root", "f.root")
          }
        w_ub <- tryCatch(pseudo_johnson_neyman_uniroot(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "upper",
                            extendInt = extendInt,
                            tol = tol),
                         error = function(e) e)
        if (inherits(w_ub, "error")) {
            w_ub <- w_ub_0
            names(w_ub) <- c("root", "f.root")
          }
        w_df <- rbind(data.frame(w_lb[c("root", "f.root")]),
                      data.frame(w_ub[c("root", "f.root")]))
      }
    if (optimize_method == "optimize") {
        w_lb <- tryCatch(pseudo_johnson_neyman_optimize(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "lower",
                            tol = tol),
                         error = function(e) e)
        w_ub <- tryCatch(pseudo_johnson_neyman_optimize(object = object,
                            wlevel_interval = wlevel_interval,
                            which = "upper",
                            tol = tol),
                         error = function(e) e)
        if (inherits(w_lb, "error") || inherits(w_ub, "error")) {
            stop("The search failed. Try setting optimize_method to 'uniroot'.")
          }
        w_df <- rbind(data.frame(w_lb),
                      data.frame(w_ub))
      }
    colnames(w_df) <- c("w", "fx")
    w_df <- w_df[order(w_df$w, decreasing = TRUE), ]
    w_min <- min(w_df$w)
    w_max <- max(w_df$w)
    w_min_valid <- isTRUE(all.equal(w_df[2, "fx"], 0, tolerance = 1e-5))
    w_max_valid <- isTRUE(all.equal(w_df[1, "fx"], 0, tolerance = 1e-5))
    w_tmp <- mod_levels(w = w,
                        fit = fit,
                        values = c(Low = w_min, High = w_max))
    w_lower <- min(w_min, w_lower)
    w_upper <- max(w_max, w_upper)
    out_cond <- cond_indirect_effects(wlevels = w_tmp,
                                      x = x,
                                      y = y,
                                      m = m,
                                      fit = fit,
                                      boot_ci = boot_ci,
                                      boot_out = boot_out,
                                      mc_ci = mc_ci,
                                      mc_out = mc_out,
                                      standardized_x = standardized_x,
                                      standardized_y = standardized_y)
    out <- list(cond_effects = out_cond,
                w_min_valid = w_min_valid,
                w_max_valid = w_max_valid,
                w_range_lb = w_min,
                w_range_ub = w_max,
                w_lower = w_lower,
                w_upper = w_upper)
    class(out) <- c("pseudo_johnson_neyman", class(out))
    out
  }

johnson_neyman(out)
