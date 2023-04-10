library(testthat)
library(manymome)
suppressMessages(library(lavaan))

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

outi <- indirect_effect(x = "m1", y = "m3", m = c("m2"), fit = fitm)
outi_boot <- cond_indirect(x = "m1", y = "m3", m = c("m2"), fit = fitm_boot,
                     boot_ci = TRUE)

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
    expect_equal(out$indirect, out_chk$indirect)
    # expect_identical(out_boot$boot_indirect,
    #                  sapply(out_boot_chk, function(x) x$indirect))
  })

test_that("cond_indirect: lavaan, mediation only", {
    expect_equal(outm$indirect, outm_chk$indirect)
    # expect_identical(outm_boot$boot_indirect,
    #                  sapply(outm_boot_chk, function(x) x$indirect))
    expect_identical(outm[-which(names(outm) == "cond_indirect_call")],
                     outi[-which(names(outi) == "cond_indirect_call")])
  })

test_that("cond_indirect: lavaan, moderation only", {
    expect_equal(outmo$indirect, outmo_chk$indirect)
    # expect_identical(outmo_boot$boot_indirect,
    #                  sapply(outmo_boot_chk, function(x) x$indirect))
  })

# Monte Carlo

## Moderated mediation

fitml <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
out_mc <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fitml,
                     wvalues = wv,
                     mc_ci = TRUE,
                     R = 50,
                     seed = 532423)
# Emulate se = boot
fit_boot_mc <- gen_mc_est(fitml, R = 50, seed = 532423)
fit_boot_mc@boot <- fit_boot@boot
fit_boot_mc@boot$coef[] <- fit_boot_mc@external$manymome$mc
fit_boot_mc@Options$se <- "bootstrap"
fit_boot_mc_boot_out <- fit2boot_out(fit_boot_mc)
out_boot_mc <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit_boot_mc,
                     wvalues = wv,
                     boot_ci = TRUE)

test_that("cond_indirect: lavaan: mc", {
    expect_equal(out_mc$indirect, out_chk$indirect)
    expect_equal(out_mc$mc_ci, out_boot_mc$boot_ci)
  })

## Mediation Only

fitmml <- sem(modm, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
outm_mc <- cond_indirect(x = "m1", y = "m3",
                     m = c("m2"),
                     fit = fitmml,
                     mc_ci = TRUE,
                     R = 50,
                     seed = 89576)
# Emulate se = boot
fitm_boot_mc <- gen_mc_est(fitmml, R = 50, seed = 89576)
fitm_boot_mc@boot <- fitm_boot@boot
fitm_boot_mc@boot$coef[] <- fitm_boot_mc@external$manymome$mc
fitm_boot_mc@Options$se <- "bootstrap"
fitm_boot_mc_boot_out <- fit2boot_out(fitm_boot_mc)
outm_boot_mc <- cond_indirect(x = "m1", y = "m3",
                     m = c("m2"),
                     fit = fitm_boot_mc,
                     boot_ci = TRUE)

test_that("cond_indirect: lavaan, mediation only: mc", {
    expect_equal(outm_mc$indirect, outm_chk$indirect)
    expect_equal(outm_mc$mc_ci, outm_boot_mc$boot_ci)
  })


## Moderation Only

fitmoml <- sem(modmo, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
outmo_mc <- cond_indirect(x = "m2", y = "m3", wvalues = c(m1 = -5),
                     fit = fitmoml,
                     mc_ci = TRUE,
                     R = 50,
                     seed = 8536)
# Emulate se = boot
fitmo_boot_mc <- gen_mc_est(fitmoml, R = 50, seed = 8536)
fitmo_boot_mc@boot <- fitmo_boot@boot
fitmo_boot_mc@boot$coef[] <- fitmo_boot_mc@external$manymome$mc
fitmo_boot_mc@Options$se <- "bootstrap"
fitmo_boot_mc_boot_out <- fit2boot_out(fitmo_boot_mc)
outmo_boot_mc <- cond_indirect(x = "m2", y = "m3", wvalues = c(m1 = -5),
                     fit = fitmo_boot_mc,
                     boot_ci = TRUE)

test_that("cond_indirect: lavaan, moderation only: mc", {
    expect_equal(outmo_mc$indirect, outmo_chk$indirect)
    expect_equal(outmo_mc$mc_ci, outmo_boot_mc$boot_ci)
  })

# Test ci_out

## Moderated mediation

fitml <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
fit_mc_out <- do_mc(fitml, R = 50, seed = 532423)
out_mc2 <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fitml,
                     wvalues = wv,
                     ci_type = "mc",
                     ci_out = fit_mc_out)
out_boot2 <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit,
                     wvalues = wv,
                     ci_type = "boot",
                     ci_out = boot_out)

test_that("cond_indirect: lavaan: ci_type", {
    expect_equal(out_mc$mc_ci, out_mc2$mc_ci)
    expect_equal(out_boot$boot_ci, out_boot2$boot_ci)
  })


## Mediation only

fitmml <- sem(modm, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
fitm_mc_out <- do_mc(fitmml, R = 50, seed = 89576)
outm_mc2 <- cond_indirect(x = "m1", y = "m3",
                     m = c("m2"),
                     fit = fitmml,
                     ci_type = "mc",
                     ci_out = fitm_mc_out)
outm_boot2 <- cond_indirect(x = "m1", y = "m3",
                     m = c("m2"),
                     fit = fitm,
                     ci_type = "boot",
                     ci_out = bootm_out)

test_that("cond_indirect: lavaan, mediation only: ci_type", {
    expect_equal(outm_mc$mc_ci, outm_mc2$mc_ci)
    expect_equal(outm_boot$boot_ci, outm_boot2$boot_ci)
  })


## Moderation only

fitmoml <- sem(modmo, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
fitmo_mc_out <- do_mc(fitmoml, R = 50, seed = 8536)
outmo_mc2 <- cond_indirect(x = "m2", y = "m3", wvalues = c(m1 = -5),
                     fit = fitmoml,
                     ci_type = "mc",
                     ci_out = fitmo_mc_out)
outmo_boot2 <- cond_indirect(x = "m2", y = "m3", wvalues = c(m1 = -5),
                     fit = fitmo,
                     ci_type = "boot",
                     ci_out = bootmo_out)

test_that("cond_indirect: lavaan, moderation only: ci_type", {
    expect_equal(outmo_mc$mc_ci, outmo_mc2$mc_ci)
    expect_equal(outmo_boot$boot_ci, outmo_boot2$boot_ci)
  })
