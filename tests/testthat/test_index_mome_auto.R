skip("WIP")
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
pest <- parameterEstimates(fit)
set.seed(4456)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 100,
                warn = FALSE)

mm_w1 <- mod_levels("w1", fit = fit, values = c(-1, 0, 1))
mm_w4 <- mod_levels("w4", fit = fit, values = c(-1, 0, 1))
out_mm <- merge_mod_levels(mm_w1, mm_w4)

suppressWarnings(out_momome <- cond_indirect_effects(wlevels = out_mm, x = "x", y = "y",
                                m = c("m1", "m2", "m3"), fit = fit_boot,
                                boot_ci = TRUE))
suppressWarnings(out_mome <- cond_indirect_effects(wlevels = mm_w1, x = "x", y = "m3",
                                m = c("m1", "m2"), fit = fit_boot,
                                boot_ci = TRUE))

ind_mome_1 <- index_of_mome(out_mome, from = 3, to = 2)
ind_mome_2 <- index_of_mome(out_mome, from = 2, to = 1)
coef(ind_mome_1)
coef(ind_mome_2)
prod(pest$est[c(3, 4, 5)])

ind_momome_1a <- index_of_mome(out_momome, from = 9, to = 8)
ind_momome_1b <- index_of_mome(out_momome, from = 6, to = 5)
ind_momome_2a <- index_of_mome(out_momome, from = 5, to = 4)
ind_momome_2b <- index_of_mome(out_momome, from = 2, to = 1)
ind_momome_3a <- index_of_mome(out_momome, from = 8, to = 7)
ind_momome_3b <- index_of_mome(out_momome, from = 6, to = 5)
coef(ind_momome_1b) - coef(ind_momome_1a)
coef(ind_momome_2b) - coef(ind_momome_2a)
coef(ind_momome_3b) - coef(ind_momome_3a)
prod(pest$est[c(3, 4, 5, 9)])

index_of_mome_h <- function(x, y, m, w, fit, level = .95,
                            ...) {
    mm_w <- mod_levels(w, fit = fit, values = c(0, 1))
    prods <- cond_indirect(wvalues = mm_w[1, ],
                            x = x,
                            y = y,
                            m = m,
                            fit = fit,
                            get_prods_only = TRUE,
                            ...)
    if (n_prods(prods) != 1) {
        stop("The index cannot be computed when there is more than one moderators")
      }
    out <- cond_indirect_effects(wlevels = mm_w, x = x, y = y,
                                 m = m, fit = fit, level = level,
                                 boot_ci = TRUE, ...)
    index_of_mome(out, from = 2, to = 1, level = level)
  }
tmp <- index_of_mome_h(x = "x", y = "m2", m = "m1", w = "w1", fit = fit_boot)
tmp

index_of_momome_h <- function(x, y, m, w, z, fit, level = .95,
                              ...) {
    mm_w <- mod_levels(w, fit = fit, values = c(0, 1))
    mm_z <- mod_levels(z, fit = fit, values = c(0, 1))
    mm <- merge_mod_levels(mm_w, mm_z)
    prods <- cond_indirect(wvalues = mm[1, ],
                            x = x,
                            y = y,
                            m = m,
                            fit = fit,
                            get_prods_only = TRUE,
                            ...)
    if (identical(n_prods(prods), c(1, 1))) {
        stop("The index cannot be computed when there is more than one moderators")
      }
    out <- cond_indirect_effects(wlevels = mm, x = x, y = y,
                                 m = m, fit = fit, level = level,
                                 boot_ci = TRUE, ...)
    i0 <- index_of_mome(out, from = 4, to = 3, level = level)
    i1 <- index_of_mome(out, from = 2, to = 1, level = level)
    ind <- coef(i1) - coef(i0)
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
                boot_diff = ind_boot)
    class(out) <- c("index_of_mome", class(out))
    out
  }
tmp <- index_of_momome_h(x = "x", y = "y", m = c("m1", "m2", "m3"),
                         w = "w1", z = "w4", fit = fit_boot)
tmp

# test_that("index_of_mome", {
#     expect_equal(
#         coef(index_ind_2w),
#         index_ind_2w_ans,
#         ignore_attr = TRUE
#       )
#     expect_equal(
#         unlist(confint(index_ind_2w)),
#         index_ind_2w_ci_ans,
#         ignore_attr = TRUE
#       )
#     expect_equal(
#         coef(index_ind),
#         index_ind_ans,
#         ignore_attr = TRUE
#       )
#     expect_equal(
#         unlist(confint(index_ind)),
#         index_ind_ci_ans,
#         ignore_attr = TRUE
#       )
#     expect_equal(
#         coef(index_std),
#         index_std_ans,
#         ignore_attr = TRUE
#       )
#     expect_equal(
#         unlist(confint(index_std)),
#         index_std_ci_ans,
#         ignore_attr = TRUE
#       )
#   })
