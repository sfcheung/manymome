library(manymome)

suppressMessages(suppressMessages(library(lavaan)))

dat <- data_med_mod_b_mod
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit <- sem(mod, dat, fixed.x = FALSE, warn = FALSE)
est <- parameterEstimates(fit)

set.seed(857014)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
                se = "bootstrap", bootstrap = 10, baseline = FALSE,
                h1 = FALSE, warn = FALSE)
boot_out <- fit2boot_out(fit_boot)
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

# Moderated-moderated mediation

wv <- c(w1 = 5, w2 = -4)

out <- cond_indirect(x = "x", y = "y",
                     m = "m",
                     fit = fit,
                     wvalues = wv)
out_chk <- indirect_i(x = "x", y = "y",
                    m = "m",
                    est = parameterEstimates(fit),
                    data = lavInspect(fit, "data"),
                    wvalues = wv)
# Suppress the warnings for R = 10
out_boot <- suppressWarnings(cond_indirect(x = "x", y = "y",
                     m = c("m"),
                     fit = fit_boot,
                     wvalues = wv,
                     boot_ci = TRUE))
prods <- suppressWarnings(cond_indirect(x = "x", y = "y", m = "m", fit = fit_boot,
                       get_prods_only = TRUE))
out_boot_chk <- mapply(indirect_i,
                       est = boot_est,
                       implied_stats = boot_implied_stats,
                       MoreArgs = list(x = "x",
                                       y = "y",
                                       m = c("m"),
                                       wvalues = wv,
                                       prods = prods),
                       SIMPLIFY = FALSE)

test_that("cond_indirect: lavaan, 3-way", {
    expect_identical(out$indirect, out_chk$indirect)
    expect_identical(out_boot$boot_indirect,
                     sapply(out_boot_chk, function(x) x$indirect))
  })
