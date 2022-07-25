library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
set.seed(85701)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
                se = "bootstrap", bootstrap = 50, baseline = FALSE,
                h1 = FALSE)
boot_out <- fit2boot_out(fit_boot)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

# Moderated mediation

wv <- c(w1 = 5, w4 = 3)

out <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect_i(x = "x", y = "y",
                    m = c("m1"),
                    est = parameterEstimates(fit),
                    wvalues = wv)

out_boot <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit_boot,
                     wvalues = wv,
                     boot_ci = TRUE)
out_boot_chk <- mapply(indirect_i,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m1"),
                                       wvalues = wv),
                       SIMPLIFY = FALSE)

# Mediation only

dat <- modmed_x1m3w4y1
modm <-
"
m1 ~ x
m2 ~ m1
m3 ~ m1 + m2
y  ~ m3 + x
"
fitm <- sem(modm, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
set.seed(85701)
fitm_boot <- sem(modm, dat, meanstructure = TRUE, fixed.x = FALSE,
                se = "bootstrap", bootstrap = 50, baseline = FALSE,
                h1 = FALSE)
bootm_out <- fit2boot_out(fitm_boot)
bootm_est <- lapply(bootm_out, function(x) x$est)
bootm_implied_stats <- lapply(bootm_out, function(x) x$implied_stats)

outm <- cond_indirect(x = "m1", y = "m3",
                     m = c("m2"),
                     fit = fitm)
outm_chk <- indirect_i(x = "m1", y = "m3",
                    m = c("m2"),
                    est = parameterEstimates(fitm))

outm_boot <- cond_indirect(x = "m1", y = "m3",
                     m = c("m2"),
                     fit = fitm_boot,
                     boot_ci = TRUE)
outm_boot_chk <- mapply(indirect_i,
                       est = bootm_est,
                       implied_stats = bootm_implied_stats,
                       MoreArgs = list(x = "m1",
                                       y = "m3",
                                       m = c("m2")),
                       SIMPLIFY = FALSE)

# Moderation only

dat <- modmed_x1m3w4y1
modmo <-
"
m1 ~ x
m2 ~ m1
m3 ~ m1 + m2 + m2:m1
y  ~ m3 + x
"
fitmo <- sem(modmo, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
set.seed(85701)
fitmo_boot <- sem(modmo, dat, meanstructure = TRUE, fixed.x = FALSE,
                se = "bootstrap", bootstrap = 50, baseline = FALSE,
                h1 = FALSE)
bootmo_out <- fit2boot_out(fitmo_boot)
bootmo_est <- lapply(bootmo_out, function(x) x$est)
bootmo_implied_stats <- lapply(bootmo_out, function(x) x$implied_stats)

outmo <- cond_indirect(x = "m2", y = "m3", wvalues = c(m1 = -5),
                     fit = fitmo)
outmo_chk <- indirect_i(x = "m2", y = "m3", wvalues = c(m1 = -5),
                    est = parameterEstimates(fitmo))

outmo_boot <- cond_indirect(x = "m2", y = "m3", wvalues = c(m1 = -5),
                     fit = fitmo_boot,
                     boot_ci = TRUE)
outmo_boot_chk <- mapply(indirect_i,
                       est = bootmo_est,
                       implied_stats = bootmo_implied_stats,
                       MoreArgs = list(x = "m2",
                                       y = "m3",
                                       wvalues = c(m1 = -5)),
                       SIMPLIFY = FALSE)

test_that("cond_indirect: lavaan", {
    expect_identical(out$indirect, out_chk$indirect)
    # expect_identical(out_boot$boot_indirect,
    #                  sapply(out_boot_chk, function(x) x$indirect))
  })

test_that("cond_indirect: lavaan, mediation only", {
    expect_identical(outm$indirect, outm_chk$indirect)
    # expect_identical(outm_boot$boot_indirect,
    #                  sapply(outm_boot_chk, function(x) x$indirect))
  })

test_that("cond_indirect: lavaan, moderation only", {
    expect_identical(outmo$indirect, outmo_chk$indirect)
    # expect_identical(outmo_boot$boot_indirect,
    #                  sapply(outmo_boot_chk, function(x) x$indirect))
  })
