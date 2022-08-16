suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
dat$m3w4 <- dat$m3 * dat$w4
mod <-
"
m1 ~ x + w1 + xw1
m2 ~ m1
m3 ~ m2
y ~ m3 + x + w4 + m3w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
set.seed(4456)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 100,
                warn = FALSE)

out_mm_1 <- mod_levels_list("w1", "w4", fit = fit)
out_mm_2 <- mod_levels("w1", fit = fit)

# Suppress warnings due to small number of bootstrap samples.
# suppressWarnings(out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y",
#                                 m = c("m1", "m2", "m3"), fit = fit))
# suppressWarnings(out_2 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m3",
#                                 m = c("m1", "m2"), fit = fit))
# suppressWarnings(out_4 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m3",
#                                m = c("m1", "m2"), fit = fit,
#                                standardized_x = TRUE, standardized_y = TRUE))

suppressWarnings(out_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y",
                                m = c("m1", "m2", "m3"), fit = fit_boot,
                                boot_ci = TRUE))
suppressWarnings(out_6 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m3",
                                m = c("m1", "m2"), fit = fit_boot,
                                boot_ci = TRUE))
suppressWarnings(out_8 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m3",
                               m = c("m1", "m2"), fit = fit_boot,
                               standardized_x = TRUE, standardized_y = TRUE,
                               boot_ci = TRUE))

suppressWarnings(out_5_nb <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y",
                                m = c("m1", "m2", "m3"), fit = fit_boot,
                                boot_ci = FALSE))


index_ind_2w <- cond_indirect_diff(out_5, from = 2, to = 1)
index_ind <- cond_indirect_diff(out_6, from = 2, to = 1)
index_std <- cond_indirect_diff(out_8, from = 2, to = 1)

out_5_full <- attr(out_5, "full_output")
index_ind_2w_ans <- out_5[1, "ind"] - out_5[2, "ind"]
index_ind_2w_ci_ans <- quantile(out_5_full[[1]]$boot_i - out_5_full[[2]]$boot_i, c(.025, .975))

index_ind_2w_nb <- cond_indirect_diff(out_5_nb, from = 2, to = 1)


out_6_full <- attr(out_6, "full_output")
index_ind_ans <- out_6[1, "ind"] - out_6[2, "ind"]
index_ind_ci_ans <- quantile(out_6_full[[1]]$boot_i - out_6_full[[2]]$boot_i, c(.025, .975))

out_8_full <- attr(out_8, "full_output")
index_std_ans <- out_8[1, "std"] - out_8[2, "std"]
index_std_ci_ans <- quantile(out_8_full[[1]]$boot_i - out_8_full[[2]]$boot_i, c(.025, .975))

test_that("cond_indirect_diff", {
    expect_equal(
        coef(index_ind_2w),
        index_ind_2w_ans,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(index_ind_2w)),
        index_ind_2w_ci_ans,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(index_ind),
        index_ind_ans,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(index_ind)),
        index_ind_ci_ans,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(index_std),
        index_std_ans,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(index_std)),
        index_std_ci_ans,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(index_ind_2w_nb)),
        c(NA, NA),
        ignore_attr = TRUE
      )
  })
