skip_on_cran()
skip_if_not_installed("semTools")
skip_if_not_installed("Amelia")
skip("To be examined in an interactive session")
# It works if run in an interactive mode.
# Something's about the methods in semTools
# Require semTools
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))

fn <- system.file("extdata", "mi_test_data.RData", package = "manymome")
load(fn)

# Moderated mediation

wv <- c(w2 = 5, w4 = 3)

out1_1_lav <- cond_indirect(x = "x", y = "y",
                         m = c("m1", "m2", "m3"),
                         fit = fit1_lav,
                         wvalues = wv)
out1_1_mi <- cond_indirect(x = "x", y = "y",
                        m = c("m1", "m2", "m3"),
                        fit = fit1_mi,
                        wvalues = wv)
est1_mi <- coef(fit1_mi)
out1_1_mi_check <- est1_mi["a1"] *
                   (est1_mi["a2"] + 5 * est1_mi["m2~m1:w2"]) *
                   est1_mi["a3"] *
                   (est1_mi["a4"] + 3 * est1_mi["y~m3:w4"])
test_that("Moderated mediation: MI", {
    expect_equal(unname(coef(out1_1_mi)),
                 unname(out1_1_mi_check))
  })

# Monte Carlo

# Moderated mediation

wv <- c(w2 = 5, w4 = 3)

suppressWarnings(out1_1_mi_mc <- cond_indirect(x = "x", y = "y",
                        m = c("m1", "m2", "m3"),
                        fit = fit1_mi,
                        wvalues = wv,
                        mc_ci = TRUE,
                        R = 10,
                        seed = 985723))

out1_1_mi_check <- est1_mi["a1"] *
                   (est1_mi["a2"] + 5 * est1_mi["m2~m1:w2"]) *
                   est1_mi["a3"] *
                   (est1_mi["a4"] + 3 * est1_mi["y~m3:w4"])
test_that("Moderated mediation: MI with MC", {
    expect_equal(unname(coef(out1_1_mi_mc)),
                 unname(out1_1_mi_check))
  })
