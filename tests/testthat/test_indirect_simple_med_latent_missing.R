library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- simple_mediation_latent
dat <- add_na(dat, prop = .10, seed = 4589)
length(complete.cases(dat))
mod <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * fx
fy ~ b * fm + cp * fx
indirect := a * b
"
fit <- sem(mod, dat, se = "none", baseline = FALSE, missing = "fiml")
est <- parameterEstimates(fit)
std <- standardizedSolution(fit)
est[c(10:12, 25), ]
out <- indirect_i(x = "fx", y = "fy", m = c("fm"), fit = fit)
out
out_std <- indirect_i(x = "fx", y = "fy", m = c("fm"), fit = fit,
                    standardized_x = TRUE,
                    standardized_y = TRUE)
out_std
std[c(10:12, 25), ]

# Test against cond_indirect
out_cond <- cond_indirect(x = "fx", y = "fy", m = c("fm"), fit = fit)
out_cond$indirect
out$indirect

# Bootstrapping

set.seed(85701)
fit_boot <- sem(mod, dat,
                se = "bootstrap", bootstrap = 10, baseline = FALSE,
                h1 = FALSE, warn = FALSE,
                missing = "fiml")
# boot_out <- fit2boot_out(fit_boot)
boot_out <- suppressMessages(fit2boot_out_do_boot(fit, R = 10, seed = 85701))
boot_est <- lapply(boot_out, function(x) x$est)
boot_implied_stats <- lapply(boot_out, function(x) x$implied_stats)

suppressWarnings(out_cond_boot <- cond_indirect(x = "fx", y = "fy", m = c("fm"), fit = fit,
                               boot_ci = TRUE,
                               boot_out = boot_out))
out_cond_boot_chk <- mapply(indirect_i,
                            est = boot_est,
                            implied_stats = boot_implied_stats,
                            MoreArgs = list(x = "fx",
                                            y = "fy",
                                            m = "fm"),
                            SIMPLIFY = FALSE)
i <- which(est$lhs == "indirect")

test_that("indirect: latent variable", {
    expect_equal(out$indirect,
                 est[i, "est"])
    expect_equal(out_std$indirect,
                 std[i, "est.std"])
    expect_equal(out_cond$indirect,
                 out$indirect)
    expect_equal(out_cond_boot$boot_indirect,
                     sapply(out_cond_boot_chk, function(x) x$indirect))
  })
