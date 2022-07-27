library(stdmodsem)

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
fit <- lm2list(lm_m, lm_y)
lm2fit <- lm2ptable(fit)

boot_out <- lm2boot_out(fit, R = 100, seed = 418751)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

wv <- c(w1 = 5, w2 = -4)

# Moderated mediation
out <- cond_indirect(x = "x", y = "y",
                     m = "m",
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect_i(x = "x", y = "y",
                    m = "m",
                    est = lm2fit$est,
                    data = lm2fit$data,
                    wvalues = wv)

# Moderated mediation with bootstrap CI, precomputed bootstrapping
out_boot <- cond_indirect(x = "x", y = "y",
                     m = "m",
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out)
prods <- cond_indirect(x = "x", y = "y", m = "m",
                       fit = fit, get_prods_only = TRUE)
out_boot_chk <- mapply(indirect_i,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = "m",
                                       wvalues = wv,
                                       prods = prods),
                       SIMPLIFY = FALSE)

test_that("cond_indirect: lm, 3-way", {
    expect_identical(out$indirect, out_chk$indirect)
    expect_identical(out_boot$boot_indirect,
                     sapply(out_boot_chk, function(x) x$indirect))
  })

test_that("confint for indirect: lm, 3-way", {
    expect_warning(confint(out))
    expect_equal(coef(out),  out_boot$indirect, ignore_attr = TRUE)
    expect_equal(confint(out_boot),  out_boot$boot_ci, ignore_attr = TRUE)
  })

