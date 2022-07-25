
library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)
lm2fit <- lm2ptable(fit)
boot_out <- lm2boot_out(fit, R = 100, seed = 418751)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

wv <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

# Moderated mediation
out <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect_i(x = "x", y = "y",
                    m = c("m1", "m2", "m3"),
                    est = lm2fit$est,
                    wvalues = wv)

# Moderated mediation with bootstrap CI, precomputed bootstrapping
out_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     boot_out = boot_out)
out_boot_chk <- mapply(indirect_i,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1", "m2", "m3"),
                                       wvalues = wv),
                       SIMPLIFY = FALSE)

# Moderated mediation with bootstrap CI, new bootstrapping
out_boot2 <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     boot_ci = TRUE,
                     R = 100,
                     seed = 418751)

tmp <- capture.output(print(out))
tmp <- capture.output(print(out_boot))
tmp <- capture.output(print(out_boot2))

# Mediation only

dat <- modmed_x1m3w4y1
lm_m1m <- lm(m1 ~ x, dat)
lm_m2m <- lm(m2 ~ m1, dat)
lm_m3m <- lm(m3 ~ m2, dat)
lm_ym <- lm(y ~ m3 + x, dat)
fitm <- lm2list(lm_m1m, lm_m2m, lm_m3m, lm_ym)
lm2mfit <- lm2ptable(fitm)
boot_outm <- lm2boot_out(fitm, R = 100, seed = 418751)
boot_estm <- lapply(boot_outm, function(x) x$est)
boot_implied_statsm <- lapply(boot_outm, function(x) x$implied_stats)

outm <- cond_indirect(x = "x", y = "y",
                      m = c("m1", "m2", "m3"),
                      fit = fitm)
outm_chk <- indirect_i(x = "x", y = "y",
                    m = c("m1", "m2", "m3"),
                    est = lm2mfit$est)

# Mediation only with bootstrap CI, precomputed bootstrapping
outm_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)
outm_boot_chk <- mapply(indirect_i,
                       est = boot_estm,
                       implied_stats = boot_implied_statsm,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1", "m2", "m3")),
                       SIMPLIFY = FALSE)
outi <- indirect_effect(x = "x", y = "y",
                      m = c("m1", "m2", "m3"),
                      fit = fitm)
outi_boot <- indirect_effect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     boot_ci = TRUE,
                     boot_out = boot_outm)

# Moderation only

dat <- modmed_x1m3w4y1
lm_m1mo <- lm(m1 ~ x, dat)
lm_m2mo <- lm(m2 ~ m1 * w1, dat)
lm_m3mo <- lm(m3 ~ m2, dat)
lm_ymo <- lm(y ~ m3 + x, dat)
fitmo <- lm2list(lm_m1mo, lm_m2mo, lm_m3mo, lm_ymo)
lm2mofit <- lm2ptable(fitmo)
boot_outmo <- lm2boot_out(fitmo, R = 100, seed = 418751)
boot_estmo <- lapply(boot_outmo, function(x) x$est)
boot_implied_statsmo <- lapply(boot_outmo, function(x) x$implied_stats)

outmo <- cond_indirect(x = "m1", y = "m2", wvalues = c(w1 = 4),
                      fit = fitmo)
outmo_chk <- indirect_i(x = "m1", y = "m2", wvalues = c(w1 = 4),
                    est = lm2mofit$est)

# Moderation only with bootstrap CI, precomputed bootstrapping
outmo_boot <- cond_indirect(x = "m1", y = "m2", wvalues = c(w1 = 4),
                     fit = fitmo,
                     boot_ci = TRUE,
                     boot_out = boot_outmo)
outmo_boot_chk <- mapply(indirect_i,
                       est = boot_estmo,
                       implied_stats = boot_implied_statsmo,
                       MoreArgs = list(x = "m1", y = "m2", wvalues = c(w1 = 4)),
                       SIMPLIFY = FALSE)


test_that("cond_indirect: lavaan", {
    expect_identical(out$indirect, out_chk$indirect)
    # expect_identical(out_boot$boot_indirect,
    #                  sapply(out_boot_chk, function(x) x$indirect))
  })

test_that("confint for indirect", {
    expect_warning(confint(out))
    expect_equal(coef(out),  out_boot$indirect, ignore_attr = TRUE)
    expect_equal(confint(out_boot),  out_boot$boot_ci, ignore_attr = TRUE)
  })

test_that("cond_indirect: lavaan, mediation only", {
    expect_identical(outm$indirect, outm_chk$indirect)
    expect_identical(outm_boot$boot_indirect,
                     sapply(outm_boot_chk, function(x) x$indirect))
    expect_identical(outm[-which(names(outm) == "cond_indirect_call")],
                     outi[-which(names(outi) == "cond_indirect_call")])
  })

test_that("confint for indirect, mediation only", {
    expect_warning(confint(outm))
    expect_equal(coef(outm),  outm_boot$indirect, ignore_attr = TRUE)
    expect_equal(confint(outm_boot),  outm_boot$boot_ci, ignore_attr = TRUE)
    expect_equal(confint(outi_boot),  outm_boot$boot_ci, ignore_attr = TRUE)
  })

test_that("cond_indirect: lavaan, moderation only", {
    expect_identical(outmo$indirect, outmo_chk$indirect)
    # expect_identical(outmo_boot$boot_indirect,
    #                  sapply(outmo_boot_chk, function(x) x$indirect))
  })

test_that("confint for indirect, moderation only", {
    expect_warning(confint(outmo))
    expect_equal(coef(outmo),  outmo_boot$indirect, ignore_attr = TRUE)
    expect_equal(confint(outmo_boot),  outmo_boot$boot_ci, ignore_attr = TRUE)
  })