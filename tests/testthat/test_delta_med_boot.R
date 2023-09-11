library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Simple Mediation

dat <- data_med
mod <-
"
m ~ a * x
y ~ b * m + cp * x
ab := a * b
"
fit <- sem(mod, dat, fixed.x = FALSE)
parameterEstimates(fit)

boot_out <- do_boot(fit,
                    R = 100,
                    seed = 879,
                    parallel = FALSE,
                    progress = FALSE)

dm_i <- delta_med(fit = fit,
                  x = "x",
                  y = "y",
                  m = "m")
dm_i
print(dm_i, full = TRUE)
print(dm_i, full = TRUE, digits = 5)
chk_dm_i_ci <- structure(c(NA, NA), dim = 1:2, dimnames = list("Delta_Med",
    c("2.5 %", "97.5 %")))

test_that("delta_med: CI", {
    expect_output(print(dm_i, full = TRUE),
                  "0.230")
    expect_equal(confint(dm_i),
                 chk_dm_i_ci)
  })

dm_boot <- delta_med(fit = fit,
                     x = "x",
                     y = "y",
                     m = "m",
                     boot_out = boot_out,
                     progress = FALSE)
dm_boot

print(dm_boot, full = TRUE)
print(dm_boot, full = TRUE, digits = 4)
print(dm_boot, level = .80, full = TRUE, digits = 4)
chk_dm_boot_ci_90 <- structure(c(0.126950157227903, 0.308364634253817), dim = 1:2, dimnames = list(
    "Delta_Med", c("5 %", "95 %")))

test_that("delta_med: CI", {
    expect_output(print(dm_boot, full = TRUE),
                  "0.230")
    expect_output(print(dm_boot, full = TRUE),
                  "95.0% Bootstrap confidence interval")
    expect_equal(confint(dm_boot),
                 dm_boot$boot_ci,
                 ignore_attr = TRUE)
    expect_equal(confint(dm_boot, level = .90),
                 chk_dm_boot_ci_90)
  })
