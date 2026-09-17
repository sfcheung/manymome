skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)
lm2fit <- lm2ptable(fit)
mc_out <- do_mc(fit, R = 100, seed = 418751, progress = FALSE)
mc_est <- lapply(mc_out, function(x) x$est)

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
out_mc <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     mc_ci = TRUE,
                     mc_out = mc_out)
out_mc_chk <- mapply(indirect_i,
                       est = mc_est,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1", "m2", "m3"),
                                       wvalues = wv),
                       SIMPLIFY = FALSE)

# Moderated mediation with bootstrap CI, new bootstrapping
out_mc2 <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     wvalues = wv,
                     mc_ci = TRUE,
                     R = 100,
                     parallel = FALSE,
                     progress = FALSE,
                     seed = 418751)

tmp <- capture.output(print(out))
tmp <- capture.output(print(out_mc))
tmp <- capture.output(print(out_mc2))

# Mediation only

dat <- modmed_x1m3w4y1
lm_m1m <- lm(m1 ~ x, dat)
lm_m2m <- lm(m2 ~ m1, dat)
lm_m3m <- lm(m3 ~ m2, dat)
lm_ym <- lm(y ~ m3 + x, dat)
fitm <- lm2list(lm_m1m, lm_m2m, lm_m3m, lm_ym)
lm2mfit <- lm2ptable(fitm)
mc_outm <- do_mc(fitm, R = 40, seed = 418751, progress = FALSE)
mc_estm <- lapply(mc_outm, function(x) x$est)

outm <- cond_indirect(x = "x", y = "y",
                      m = c("m1", "m2", "m3"),
                      fit = fitm)
outm_chk <- indirect_i(x = "x", y = "y",
                    m = c("m1", "m2", "m3"),
                    est = lm2mfit$est)

# Mediation only with bootstrap CI, precomputed bootstrapping
outm_mc <- cond_indirect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     mc_ci = TRUE,
                     mc_out = mc_outm)
outm_mc_chk <- mapply(indirect_i,
                       est = mc_estm,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1", "m2", "m3")),
                       SIMPLIFY = FALSE)
outi <- indirect_effect(x = "x", y = "y",
                      m = c("m1", "m2", "m3"),
                      fit = fitm)
outi_mc <- indirect_effect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     mc_ci = TRUE,
                     mc_out = mc_outm)

# Moderation only

dat <- modmed_x1m3w4y1
lm_m1mo <- lm(m1 ~ x, dat)
lm_m2mo <- lm(m2 ~ m1 * w1, dat)
lm_m3mo <- lm(m3 ~ m2, dat)
lm_ymo <- lm(y ~ m3 + x, dat)
fitmo <- lm2list(lm_m1mo, lm_m2mo, lm_m3mo, lm_ymo)
lm2mofit <- lm2ptable(fitmo)
mc_outmo <- do_mc(fitmo, R = 40, seed = 418751, progress = FALSE)
mc_estmo <- lapply(mc_outmo, function(x) x$est)

outmo <- cond_indirect(x = "m1", y = "m2", wvalues = c(w1 = 4),
                      fit = fitmo)
outmo_chk <- indirect_i(x = "m1", y = "m2", wvalues = c(w1 = 4),
                    est = lm2mofit$est)

# Moderation only with bootstrap CI, precomputed bootstrapping
outmo_mc <- cond_indirect(x = "m1", y = "m2", wvalues = c(w1 = 4),
                     fit = fitmo,
                     mc_ci = TRUE,
                     mc_out = mc_outmo)
outmo_mc_chk <- mapply(indirect_i,
                       est = mc_estmo,
                       MoreArgs = list(x = "m1", y = "m2", wvalues = c(w1 = 4)),
                       SIMPLIFY = FALSE)


test_that("cond_indirect: lm", {
    expect_equal(out$indirect, out_chk$indirect)
    # expect_identical(out_boot$boot_indirect,
    #                  sapply(out_boot_chk, function(x) x$indirect))
  })

test_that("confint for indirect: lm", {
    expect_warning(confint(out))
    expect_equal(coef(out),  out_mc$indirect, ignore_attr = TRUE)
    expect_equal(confint(out_mc),  out_mc$mc_ci, ignore_attr = TRUE)
  })

test_that("cond_indirect: lm, mediation only", {
    expect_equal(outm$indirect, outm_chk$indirect)
    expect_equal(outm_mc$mc_indirect,
                     sapply(outm_mc_chk, function(x) x$indirect))
    expect_identical(outm[-which(names(outm) == "cond_indirect_call")],
                     outi[-which(names(outi) == "cond_indirect_call")])
  })

test_that("confint for indirect, mediation only", {
    expect_warning(confint(outm))
    expect_equal(coef(outm),  outm_mc$indirect, ignore_attr = TRUE)
    expect_equal(confint(outm_mc),  outm_mc$mc_ci, ignore_attr = TRUE)
    expect_equal(confint(outi_mc),  outm_mc$mc_ci, ignore_attr = TRUE)
  })

test_that("cond_indirect: lm, moderation only", {
    expect_equal(outmo$indirect, outmo_chk$indirect)
    # expect_identical(outmo_boot$boot_indirect,
    #                  sapply(outmo_boot_chk, function(x) x$indirect))
  })

test_that("confint for indirect: lm, moderation only", {
    # SE-CI is valid and returned now
    # expect_warning(confint(outmo))
    expect_equal(coef(outmo),  outmo_mc$indirect, ignore_attr = TRUE)
    expect_equal(confint(outmo_mc),  outmo_mc$mc_ci, ignore_attr = TRUE)
  })

out_mc2_preboot <- cond_indirect(x = "x", y = "y",
                                    m = c("m1", "m2", "m3"),
                                    fit = fit,
                                    wvalues = wv,
                                    mc_ci = TRUE,
                                    mc_out = out_mc2)

test_that("cond_indirect: Use boot_out from previous run", {
    expect_equal(out_mc2_preboot$mc_ci, out_mc2$mc_ci, ignore_attr = TRUE)
  })

outm_mc2 <- indirect_effect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     mc_ci = TRUE,
                     R = 40,
                     parallel = FALSE,
                     progress = FALSE,
                     seed = 43175)
outm_mc2_preboot <- indirect_effect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fitm,
                     mc_ci = TRUE,
                     mc_out = outm_mc2)

test_that("indirect_effect: Use mc_out from previous run", {
    expect_equal(outm_mc2_preboot$mc_ci, outm_mc2$mc_ci, ignore_attr = TRUE)
  })
