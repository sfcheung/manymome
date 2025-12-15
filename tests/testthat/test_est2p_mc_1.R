library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("MC p-value", {

# Mediation only

dat <- modmed_x1m3w4y1
lm_m1m <- lm(m1 ~ x, dat)
lm_m2m <- lm(m2 ~ m1, dat)
lm_m3m <- lm(m3 ~ m2, dat)
lm_ym <- lm(y ~ m3 + x, dat)
fitm <- lm2list(lm_m1m, lm_m2m, lm_m3m, lm_ym)
mod <-
"
m1 ~ x
m2 ~ m1
m3 ~ m2
y ~ m3 + x
"
fit <- sem(mod,
           dat)
mc_out <- do_mc(fit = fit,
                R = 40,
                parallel = FALSE,
                progress = FALSE,
                seed = 53253)

out_mc <- indirect_effect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     mc_ci = TRUE,
                     mc_out = mc_out)
expect_true(is.na(out_mc$mc_p))

out_mc2 <- indirect_effect(x = "x", y = "y",
                     m = c("m1", "m2", "m3"),
                     fit = fit,
                     mc_ci = TRUE,
                     mc_out = mc_out,
                     internal_options = list(pvalue_min_size = 39))
expect_false(is.na(out_mc2$mc_p))

mod <-
"
m1 ~ x
m2 ~ m1
m3 ~ x
y ~ m2 + m3
"
fit <- sem(mod,
           dat)
mc_out <- do_mc(fit = fit,
                R = 40,
                parallel = FALSE,
                progress = FALSE,
                seed = 53253)

paths <- all_indirect_paths(fit,
                            x = "x",
                            y = "y")
outs <- manymome::many_indirect_effects(paths = paths,
                                        fit = fit,
                                        mc_ci = TRUE,
                                        mc_out = mc_out,
                                        internal_options = list(pvalue_min_size = -Inf),
                                        progress = FALSE)
outs2 <- indirect_effects_from_list(outs,
                                    pvalue = TRUE)
expect_true(all(!is.na(outs2$pvalue)))

# Moderated mediation

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
y  ~ a4 * m1  + b4 * w4 + d4 * m1:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "standard", baseline = FALSE)
mc_out <- do_mc(fit = fit,
                R = 40,
                parallel = FALSE,
                progress = FALSE,
                seed = 53253)
wv <- c(w1 = 5, w4 = 3)
out <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit,
                     wvalues = wv,
                     mc_ci = TRUE,
                     mc_out = mc_out)
expect_true(is.na(out$mc_p))
out <- cond_indirect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit,
                     wvalues = wv,
                     mc_ci = TRUE,
                     mc_out = mc_out,
                     internal_options = list(pvalue_min_size = 39))
expect_false(is.na(out$mc_p))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
mc_out <- do_mc(fit = fit,
                R = 40,
                parallel = FALSE,
                progress = FALSE,
                seed = 53253)

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

# Suppress warnings due to small number of bootstrap samples.
suppressWarnings(out <- cond_indirect_effects(wlevels = out_mm_1,
                                                x = "x",
                                                y = "y",
                                                m = "m3",
                                                fit = fit,
                                                mc_ci = TRUE,
                                                mc_out = mc_out))
out_full <- attr(out, "full_output")
tmp <- sapply(out_full, function(x) x$mc_p)
expect_true(all(is.na(tmp)))

suppressWarnings(outb <- cond_indirect_effects(wlevels = out_mm_1,
                                                x = "x",
                                                y = "y",
                                                m = "m3",
                                                fit = fit,
                                                mc_ci = TRUE,
                                                mc_out = mc_out,
                                                internal_options = list(pvalue_min_size = 39)))
outb_full <- attr(outb, "full_output")
tmp <- sapply(outb_full, function(x) x$mc_p)
expect_true(all(!is.na(tmp)))

diff <- cond_indirect_diff(out, from = 1, to = 2)
expect_true(is.na(diff$pvalue))

diffb <- cond_indirect_diff(outb, from = 1, to = 2)
expect_true(!is.na(diffb$pvalue))

tmp <- capture.output(print(diffb))

# Should not print Monte Carlo asymmetric p-value for now
expect_true(all(!grepl("asymmetric", tmp)))

})
