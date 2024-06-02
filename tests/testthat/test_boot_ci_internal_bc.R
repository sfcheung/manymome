skip_on_cran()

# Test BC formed by boot_ci_internal

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

data(data_serial)
mod <-
"
m1 ~ a * x + c1 + c2
m2 ~ b1 * m1 + x + c1 + c2
y ~ b2 * m2 + m1 + x + c1 + c2
indirect := a * b1 * b2
"
fit <- sem(mod,
           data = data_serial,
           fixed.x = FALSE,
           se = "boot",
           bootstrap = 100,
           iseed = 1234)

suppressWarnings(est_bc <- parameterEstimates(fit, boot.ci.type = "bca.simple", level = .90))

est0 <- coef(fit, type = "user")["indirect"]

fit_boot_est <- lavInspect(fit, "boot")
fit_boot_def <- fit_boot_est[, 1, drop = FALSE] *
                fit_boot_est[, 4, drop = FALSE] *
                fit_boot_est[, 8, drop = FALSE]

# Internal BC CI
suppressWarnings(bc_ci <- boot_ci_internal(t0 = est0,
                                           t = as.vector(fit_boot_def),
                                           boot_type = "bc",
                                           level = .90))

expect_equal(unname(unlist(as.data.frame(est_bc[22, c("ci.lower", "ci.upper")]))),
             unname(bc_ci))

