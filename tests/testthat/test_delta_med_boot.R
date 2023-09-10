skip("To be run in an interactive section")

library(testthat)
library(manymome)
suppressMessages(library(lavaan))
source("delta_med.R")

# Simple Mediation

dat <- data_med
mod <-
"
m ~ a * x + c1
y ~ b * m + cp * x + c2
ab := a * b
"
fit <- sem(mod, dat, fixed.x = FALSE)
parameterEstimates(fit)

boot_out <- do_boot(fit,
                    R = 100,
                    seed = 879,
                    ncores = 4)

dm_i <- delta_med(fit = fit,
                  x = "x",
                  y = "y",
                  m = "m")
dm_i
print(dm_i, full = TRUE)
print(dm_i, full = TRUE, digits = 5)
dm_boot <- delta_med(fit = fit,
                     x = "x",
                     y = "y",
                     m = "m",
                     boot_out = boot_out)
dm_boot
print(dm_boot, full = TRUE)
print(dm_boot, full = TRUE, digits = 4)
print(dm_boot, level = .80, full = TRUE, digits = 4)
