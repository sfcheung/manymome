skip_on_cran()
# A long test

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_serial

library(lavaan)
mod_med <- "
m1 ~ x
m2 ~ m1 + x
y ~ m2 + m1 + x
"
fit_med <- sem(model = mod_med,
               data = dat,
               fixed.x = TRUE)

all_paths <- all_indirect_paths(fit = fit_med,
                                x = "x",
                                y = "y")
all_paths

out_all <- many_indirect_effects(paths = all_paths,
                                 fit = fit_med,
                                 standardized_x = TRUE,
                                 standardized_y = TRUE,
                                 boot_ci = TRUE,
                                 R = 100,
                                 seed = 12345,
                                 parallel = FALSE,
                                 progress = FALSE)
out_all
