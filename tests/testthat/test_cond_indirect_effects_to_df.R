skip("WIP")

library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 6:7]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

# Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

outmo_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit)

outmo_1_std <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                 standardized_x = TRUE)

outmo_1_boot <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                      boot_ci = TRUE,
                                      R = 100,
                                      progress = FALSE,
                                      parallel = FALSE,
                                      seed = 1234)

outmo_1_std <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                 standardized_x = TRUE)

outmo_1_std_boot <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                 boot_ci = TRUE,
                                 R = 100,
                                 progress = FALSE,
                                 parallel = FALSE,
                                 standardized_x = TRUE)



# Target output
#        ind      CI.lo    CI.hi pvalue        SE
# 1 3.060021  2.1677888 4.039487   0.00 0.4596262
# 2 2.136191  1.4073608 2.925047   0.00 0.3583436
# 3 1.212360 -0.2880764 2.564034   0.08 0.5921044
#
#          std       CI.lo     CI.hi pvalue        SE
# 1 0.06344079 -0.20196663 0.3030855   0.64 0.1237579
# 2 0.12925647 -0.06795164 0.3469891   0.30 0.1058540
# 3 0.01399145 -0.19169346 0.2860562   0.76 0.1300183

cond_indirect_effects_to_data_frame <- function(x,
                                                pvalue = NULL,
                                                se = NULL,
                                                level = .95,
                                                se_ci = TRUE) {
    out <- as.data.frame(x)
    full_output <- attr(x, "full_output")
    x_i <- full_output[[1]]
    has_ci <- FALSE
    ci_type <- NULL
    boot_type <- NULL
    has_groups <- ("group" %in% tolower(colnames(x)))
    if (has_groups) {
        group_labels <- unique(x$Group)
        group_numbers <- unique(x$Group_ID)
      } else {
        group_labels <- NULL
        group_numbers <- NULL
      }
    has_wlevels <- !is.null(attr(x, "wlevels"))
    if (!is.null(x_i$boot_ci)) {
        has_ci <- TRUE
        ci_type <- "boot"
        ind_name <- "boot_indirect"
        se_name <- "boot_se"
        boot_type <- x_i$boot_type
        if (is.null(boot_type)) boot_type <- "perc"
      }
    if (!is.null(x_i$mc_ci)) {
        has_ci <- TRUE
        ci_type <- "mc"
        ind_name <- "mc_indirect"
        se_name <- "mc_se"
      }
    standardized_x <- x_i$standardized_x
    standardized_y <- x_i$standardized_y
    has_m <- isTRUE(!is.null(x_i$m))

    # Default to OLS or Wald SE
    se_out <- cond_effects_original_se(x,
                                      level = level,
                                      append = FALSE)
    has_original_se <- !is.null(se_out)
    print_original_se <- FALSE
    if (!has_ci &&
        !has_m &&
        !has_groups &&
        has_wlevels &&
        !standardized_x &&
        !standardized_y &&
        has_original_se) {
        print_original_se <- TRUE
        if (is.null(pvalue)) {
            pvalue <- TRUE
          }
        if (is.null(se)) {
            se <- TRUE
          }
      } else {
        if (is.null(pvalue)) {
            pvalue <- FALSE
          }
        if (is.null(se)) {
            se <- FALSE
          }
        level <- x_i$level
      }

    out <- as.data.frame(x)
    if (has_ci) {
        j <- length(out)
        if ((ci_type == "boot") && pvalue) {
            boot_p <- sapply(attr(x, "full_output"), function(x) x$boot_p)
            boot_p <- unname(boot_p)
            i <- which(names(out) == "CI.hi")
            j <- length(out)
            out <- c(out[1:i], list(pvalue = boot_p), out[(i + 1):j])
          }
        if (se) {
            ind_se <- sapply(attr(x, "full_output"), function(x) x[[se_name]])
            ind_se <- unname(ind_se)
            i <- which(names(out) == "pvalue")
            if (length(i) == 0) i <- which(names(out) == "CI.hi")
            j <- length(out)
            out <- c(out[1:i], list(SE = ind_se), out[(i + 1):j])
          }
      }

    if (!has_ci &&
        !has_m &&
        !has_groups &&
        has_wlevels &&
        !standardized_x &&
        !standardized_y &&
        has_original_se) {
        # OLS or Wald SE
        # Moderation only
        print_original_se <- TRUE
        # t or Wald SE, CI, and p-values
        # TODO: Support multiple-group models
        out_original <- list()
        if (se_ci) {
            out_cilo <- unname(se_out$cilo)
            out_cihi <- unname(se_out$cihi)

            out_original <- c(out_original,
                              list(`CI.lo` = out_cilo,
                                  `CI.hi` = out_cihi))
          }
        if (pvalue) {
            # out_stat <- unname(se_out$stat)
            # out_original <- c(out_original,
            #                   list(Stat = unname(out_stat),
            #                        pvalue = unname(out_p)))
            out_p <- unname(se_out$p)
            out_original <- c(out_original,
                              list(pvalue = unname(out_p)))
          }
        if (se) {
            out_se <- unname(se_out$se)
            out_original <- c(out_original,
                              list(SE = out_se))
          }
        rownames(out_original) <- NULL
        i <- which(names(out) == "ind")
        j <- length(out)
        out <- c(out[1:i], out_original, out[(i + 1):j])
      }

    if (!has_m) {
        i <- which(names(out) %in% names(x_i$components))
        if (length(i) > 0) {
            out <- out[-i]
          }
      }
    out1 <- data.frame(out, check.names = FALSE)
    return(out1)
  }

cond_indirect_effects_to_data_frame(outmo_1)
cond_indirect_effects_to_data_frame(outmo_1_std)
cond_indirect_effects_to_data_frame(outmo_1_boot, pvalue = TRUE)
cond_indirect_effects_to_data_frame(outmo_1_boot, pvalue = TRUE, se = TRUE)
cond_indirect_effects_to_data_frame(outmo_1_std_boot, pvalue = TRUE)
cond_indirect_effects_to_data_frame(outmo_1_std_boot, pvalue = TRUE, se = TRUE)
