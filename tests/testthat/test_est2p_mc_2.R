library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("MC p-value", {

# cond_effects_math

dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x
m2 ~ x
y ~ m1 + m2 + x
"
fit <- sem(mod,
           dat)
mc_out <- do_mc(fit = fit,
                R = 40,
                parallel = FALSE,
                progress = FALSE,
                seed = 53253)

out_mc1 <- indirect_effect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit,
                     mc_ci = TRUE,
                     mc_out = mc_out)
out_mc2 <- indirect_effect(x = "x", y = "y",
                     m = c("m2"),
                     fit = fit,
                     mc_ci = TRUE,
                     mc_out = mc_out)
out_mc12 <- out_mc1 + out_mc2
expect_true(is.na(out_mc12$mc_p))

outb_mc1 <- indirect_effect(x = "x", y = "y",
                     m = c("m1"),
                     fit = fit,
                     mc_ci = TRUE,
                     mc_out = mc_out,
                     internal_options = list(pvalue_min_size = 39))
outb_mc2 <- indirect_effect(x = "x", y = "y",
                     m = c("m2"),
                     fit = fit,
                     mc_ci = TRUE,
                     mc_out = mc_out,
                     internal_options = list(pvalue_min_size = 39))
outb_mc12 <- outb_mc1 + outb_mc2
expect_true(!is.na(outb_mc12$mc_p))

outc_mc12 <- out_mc1 + outb_mc2
expect_true(is.na(outc_mc12$mc_p))

})
