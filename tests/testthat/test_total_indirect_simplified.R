skip_on_cran()
# This test is long and should be tested locally.
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_med_complicated
mod <-
"
m11 ~ x1
m12 ~ m11
m2 ~ x1
y1 ~ m12 + m2 + x1
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

lm_m11 <- lm(m11 ~ x1, dat)
lm_m12 <- lm(m12 ~ m11 , dat)
lm_m2 <- lm(m2 ~ x1, dat)
lm_y1 <- lm(y1 ~ m12 + m2 + x1, dat)

fit_lm <- lm2list(lm_m11, lm_m12, lm_m2, lm_y1)

outla <- all_indirect_paths(fit, x = "x1", y = "y1")

outlm <- all_indirect_paths(fit_lm, x = "x1", y = "y1")

outla2 <- all_indirect_paths(fit)

outlm2 <- all_indirect_paths(fit_lm)

outla3 <- all_indirect_paths(fit, x = "x1")

outlm3 <- all_indirect_paths(fit_lm, x = "x1")

outla4 <- all_indirect_paths(fit, y = "y1")

outlm4 <- all_indirect_paths(fit_lm, y = "y1")

indla <- many_indirect_effects(outla,
                               fit = fit)
indlm <- many_indirect_effects(outlm,
                               fit = fit_lm)

indla2 <- many_indirect_effects(outla2,
                                fit = fit)
indlm2 <- many_indirect_effects(outlm2,
                                fit = fit_lm)
indla3 <- many_indirect_effects(outla3,
                                fit = fit)
indlm3 <- many_indirect_effects(outlm3,
                                fit = fit_lm)
indla4 <- many_indirect_effects(outla4,
                                fit = fit)
indlm4 <- many_indirect_effects(outlm4,
                                fit = fit_lm)

test_that("lavaan vs lm", {
  expect_equal(coef(indla),
               coef(indlm),
               tolerance = 1e-4)
})

test_that("auto x and y", {
  totla <- total_indirect_effect(indla)
  totlm <- total_indirect_effect(indlm)
  totla_chk <- total_indirect_effect(indla, x = "x1", y = "y1")
  totlm_chk <- total_indirect_effect(indlm, x = "x1", y = "y1")
  expect_equal(coef(totla),
               coef(totla_chk))
  expect_equal(coef(totlm),
               coef(totlm_chk))
})

test_that("auto x and y: error", {
  expect_error(total_indirect_effect(indla2), "'x' must be")
  expect_error(total_indirect_effect(indla3), "'y' must be")
  expect_error(total_indirect_effect(indla4))

  expect_error(total_indirect_effect(indlm2), "'x' must be")
  expect_error(total_indirect_effect(indlm3), "'y' must be")
  expect_error(total_indirect_effect(indlm4))

  expect_error(total_indirect_effect(indla3, x = "x1"))
  expect_error(total_indirect_effect(indlm3, x = "x1"))
  expect_error(total_indirect_effect(indla4, y = "y1"))
  expect_error(total_indirect_effect(indlm4, y = "y1"))

  expect_no_error(total_indirect_effect(indla3, y = "y1"))
  expect_no_error(total_indirect_effect(indlm3, y = "y1"))
  expect_no_error(total_indirect_effect(indla4, x = "x1"))
  expect_no_error(total_indirect_effect(indlm4, x = "x1"))

})

