skip_on_cran()

library(testthat)
library(manymome)

dat <- data_med_mod_a
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m + x + c1 + c2, dat)
fit_lm <- lm2list(lm_m, lm_y)
cond_out <- cond_indirect_effects(wlevels = "w",
                                  x = "x",
                                  y = "y",
                                  fit = fit_lm)
test_that("No moderator", {
    # Updated in 0.2.3.8.
    # SEs are computed even for a direct path in
    # a y-model with no product term.
    expect_true(any(grepl("standard errors",
                          capture.output(print(cond_out)))))
    expect_false(is.null(cond_effects_original_se(cond_out)))
  })
