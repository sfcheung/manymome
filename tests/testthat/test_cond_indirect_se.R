skip("WIP")
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Get the SE

print_indirect_se <- function(indirect_out,
                            digits = 3,
                            ...) {
    # NOTE:
    # This is a tentative workaround,
    # not a long term solution.
    # Standard errors will be officially added in the
    # next release.

    has_ci <- FALSE
    ci_type <- NULL
    if (isTRUE(!is.null(indirect_out$boot_ci))) {
        has_ci <- TRUE
        ci_type <- "boot"
        est_name <- "boot_indirect"
        print_name <- "Bootstrap standard error:"
      }
    if (isTRUE(!is.null(indirect_out$mc_ci))) {
        has_ci <- TRUE
        ci_type <- "mc"
        est_name <- "mc_indirect"
        print_name <- "Monte Carlo standard error:"
      }
    print(indirect_out, digits = digits, ...)
    if (has_ci) {
        indirect_se <- sd(indirect_out[[est_name]],
                          na.rm = TRUE)
        cat(print_name, " ",
            round(indirect_se, digits),
            "\n",
            sep = "")
        return(invisible(indirect_se))
      } else {
        return(invisible(NA))
      }
  }

print_cond_indirect_effects_se <- function(cond_indirect_effects_out) {
    # NOTE:
    # This is a tentative workaround,
    # not a long term solution.
    # Standard errors will be officially added in the
    # next release.

    full_output <- attr(cond_indirect_effects_out, "full_output")
    if (is.null(full_output)) {
        stop("SEs cannot be computed because full outputs are not saved.")
      }
    tmp_fct <- function(x) {
        has_ci <- FALSE
        if (isTRUE(!is.null(x$boot_ci))) {
            has_ci <- TRUE
            est_name <- "boot_indirect"
            se_name <- "boot_se"
          }
        if (isTRUE(!is.null(x$mc_ci))) {
            has_ci <- TRUE
            est_name <- "mc_indirect"
            se_name <- "mc_se"
          }
        if (has_ci) {
            out <- sd(x[[est_name]],
                      na.rm = TRUE)
            return(out)
          } else {
            return(NA)
          }
      }
    se_all <- sapply(full_output, tmp_fct)
    cond_indirect_effects_out[, "SE"] <- se_all
    return(cond_indirect_effects_out)
  }

indirect_se <- function(indirect_out) {
    has_ci <- FALSE
    ci_type <- NULL
    if (isTRUE(!is.null(indirect_out$boot_ci))) {
        has_ci <- TRUE
        ci_type <- "boot"
        ci_name <- "boot_ci"
        est_name <- "boot_indirect"
        se_name <- "boot_se"
        R <- length(indirect_out$boot_indirect)
      }
    if (isTRUE(!is.null(indirect_out$mc_ci))) {
        has_ci <- TRUE
        ci_type <- "mc"
        ci_name <- "mc_ci"
        est_name <- "mc_indirect"
        se_name <- "mc_se"
        R <- length(indirect_out$mc_indirect)
      }
    if (has_ci) {
        indirect_out$boot_se <- sd(indirect_out[[est_name]], na.rm = TRUE)
      }
    return(indirect_out)
  }

cond_indirect_effects_se <- function(cond_indirect_effects_out) {
    full_output <- attr(cond_indirect_effects_out, "full_output")
    if (is.null(full_output)) {
        return(cond_indirect_effects_out)
      }
    out <- lapply(full_output, indirect_se)
    attr(cond_indirect_effects_out, "full_output") <- out
    return(cond_indirect_effects_out)
  }

# From vignette "manymome"

dat <- data_med_mod_ab

# Form the product terms
dat$w1x <- dat$w1 * dat$x
dat$w2m <- dat$w2 * dat$m
mod <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + w2 + w2m + x + c1 + c2
# Covariances of the error term of m with w2m and w2
m ~~ w2m + w2
# Covariance between other variables
# They need to be added due to the covariances added above
# See Kwan and Chan (2018) and Miles et al. (2015)
w2m ~~ w2 + x + w1 + w1x + c1 + c2
w2  ~~ x + w1 + w1x + c1 + c2
x   ~~ w1 + w1x + c1 + c2
w1  ~~ w1x + c1 + c2
w1x ~~ c1 + c2
c1  ~~ c2
"
fit <- sem(model = mod,
           data = dat,
           fixed.x = FALSE,
           estimator = "MLR")

fit_boot <- do_boot(fit = fit,
                    R = 50,
                    seed = 53253,
                    parallel = FALSE,
                    progress = FALSE)

fit_mc <- do_mc(fit = fit,
                R = 100,
                seed = 53253,
                progress = FALSE)

out_cond_boot <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                       x = "x",
                                       y = "y",
                                       m = "m",
                                       fit = fit,
                                       boot_ci = TRUE,
                                       boot_out = fit_boot)

out_cond_mc <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                     x = "x",
                                     y = "y",
                                     m = "m",
                                     fit = fit,
                                     mc_ci = TRUE,
                                     mc_out = fit_mc)

out_cond_stdxy_boot <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                        x = "x",
                                        y = "y",
                                        m = "m",
                                        fit = fit,
                                        boot_ci = TRUE,
                                        boot_out = fit_boot,
                                        standardized_x = TRUE,
                                        standardized_y = TRUE)

out_cond_stdxy_mc <- cond_indirect_effects(wlevels =c("w1", "w2"),
                                        x = "x",
                                        y = "y",
                                        m = "m",
                                        fit = fit,
                                        mc_ci = TRUE,
                                        mc_out = fit_mc,
                                        standardized_x = TRUE,
                                        standardized_y = TRUE)

out_momome_boot <- index_of_momome(x = "x",
                              y = "y",
                              m = "m",
                              w = "w1",
                              z = "w2",
                              fit = fit,
                              boot_ci = TRUE,
                              boot_out = fit_boot)

out_momome_mc <- index_of_momome(x = "x",
                              y = "y",
                              m = "m",
                              w = "w1",
                              z = "w2",
                              fit = fit,
                              mc_ci = TRUE,
                              mc_out = fit_mc)

print_cond_indirect_effects_se(out_cond_boot)
print_cond_indirect_effects_se(out_cond_mc)
print_cond_indirect_effects_se(out_cond_stdxy_boot)
print_cond_indirect_effects_se(out_cond_stdxy_mc)

# Moderation Mediation

dat$w1x <- dat$w1 * dat$x
mod2 <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + x + c1 + c2
"
fit2 <- sem(model = mod2,
           data = dat,
           fixed.x = FALSE,
           estimator = "MLR")

fit2_boot <- do_boot(fit = fit2,
                    R = 50,
                    seed = 53253,
                    parallel = FALSE,
                    progress = FALSE)

fit2_mc <- do_mc(fit = fit2,
                    R = 100,
                    seed = 53253,
                    parallel = FALSE,
                    progress = FALSE)

out_mome_boot <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w1",
                          fit = fit2,
                          boot_ci = TRUE,
                          boot_out = fit2_boot)

out_mome_mc <- index_of_mome(x = "x",
                          y = "y",
                          m = "m",
                          w = "w1",
                          fit = fit2,
                          mc_ci = TRUE,
                          mc_out = fit2_mc)

# Mediation Only

dat <- data_serial
print(head(dat), digits = 3)

mod_med <- "
m1 ~ x + c1 + c2
m2 ~ m1 + x + c1 + c2
y ~ m2 + m1 + x + c1 + c2
"
fit_med <- sem(model = mod_med,
               data = dat,
               fixed.x = TRUE)

out_med_boot <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           boot_ci = TRUE,
                           R = 50,
                           seed = 43143,
                           parallel = FALSE,
                           progress = FALSE)

out_med_mc <- indirect_effect(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           fit = fit_med,
                           mc_ci = TRUE,
                           R = 100,
                           seed = 43143,
                           progress = FALSE)

out_med_stdxy_boot <- indirect_effect(x = "x",
                                 y = "y",
                                 m = c("m1", "m2"),
                                 fit = fit_med,
                                 boot_ci = TRUE,
                                 boot_out = out_med_boot,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE)
out_med_stdxy_mc <- indirect_effect(x = "x",
                                 y = "y",
                                 m = c("m1", "m2"),
                                 fit = fit_med,
                                 mc_ci = TRUE,
                                 mc_out = out_med_mc,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE)

out_x_m2_y_boot <- indirect_effect(x = "x",
                              y = "y",
                              m = "m2",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = out_med_boot)
out_x_m2_y_mc <- indirect_effect(x = "x",
                              y = "y",
                              m = "m2",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = out_med_mc)

out_x_m1_y_boot <- indirect_effect(x = "x",
                              y = "y",
                              m = "m1",
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = out_med_boot)
out_x_m1_y_mc <- indirect_effect(x = "x",
                              y = "y",
                              m = "m1",
                              fit = fit_med,
                              mc_ci = TRUE,
                              mc_out = out_med_mc)

total_ind_boot <- out_med_boot + out_x_m1_y_boot + out_x_m2_y_boot
total_ind_mc <- out_med_mc + out_x_m1_y_mc + out_x_m2_y_mc

out_x_direct_boot <- indirect_effect(x = "x",
                                y = "y",
                                fit = fit_med,
                                boot_ci = TRUE,
                                boot_out = out_med_boot)
out_x_direct_mc <- indirect_effect(x = "x",
                                y = "y",
                                fit = fit_med,
                                mc_ci = TRUE,
                                mc_out = out_med_mc)

total_effect_boot <- out_med_boot + out_x_m1_y_boot + out_x_m2_y_boot + out_x_direct_boot
total_effect_mc <- out_med_mc + out_x_m1_y_mc + out_x_m2_y_mc + out_x_direct_mc

print_indirect_se(out_med_boot)
print_indirect_se(out_med_mc)
print_indirect_se(out_med_stdxy_boot)
print_indirect_se(out_med_stdxy_mc)
print_indirect_se(total_effect_boot)
print_indirect_se(total_effect_mc)
