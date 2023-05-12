skip_on_cran()
# The tests are long because it needs to enumerate all paths.
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_med_complicated
# Make some path positive
dat$y1 <- -dat$y1
dat$x2 <- -dat$x2
dat$y2 <- dat$y2 + .2 * dat$x2
mod <-
"
m11 ~ x1 + x2 + c1 + c2
m12 ~ m11 + x1 + x2 + c1 + c2
m2 ~ x1 + x2 + c1 + c2
y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2
y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
est <- parameterEstimates(fit)

all_ind_x1_y1 <- all_indirect_paths(fit, x = "x1", y = "y1")

# Do not form CI for now.
# boot_out <- do_boot(fit, R = 5000, seed = 5145)
# mc_out <- do_mc(fit, R = 10000, seed = 5145)

out1 <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit)
out2 <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit)
out3 <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit)

# out1_boot <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit, boot_out = boot_out)
# out2_boot <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit, boot_out = boot_out)
# out3_boot <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit, boot_out = boot_out)

# out1_mc <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit, mc_out = mc_out)
# out2_mc <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit, mc_out = mc_out)
# out3_mc <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit, mc_out = mc_out)

# out1_boot$proportion
# out2_boot$proportion
# out3_boot$proportion
# out1_boot$boot_ci
# out2_boot$boot_ci
# out3_boot$boot_ci

# out1_mc$proportion
# out2_mc$proportion
# out3_mc$proportion
# out1_mc$mc_ci
# out2_mc$mc_ci
# out3_mc$mc_ci

test_that("indirect_proportion", {
    expect_error(indirect_proportion(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit))
    expect_error(indirect_proportion(x = "x1", y = "y2", m = c("m11", "m12"), fit = fit))
    expect_error(indirect_proportion(x = "x2", y = "y1", m = c("m11", "m12"), fit = fit))
    expect_equal(coef(out1),
                 .009894992,
                 tolerance = 1e-4,
                 ignore_attr = TRUE)
    expect_equal(coef(out2),
                 .4540117,
                 tolerance = 1e-4,
                 ignore_attr = TRUE)
    expect_equal(coef(out3),
                 .7176185,
                 tolerance = 1e-4,
                 ignore_attr = TRUE)
  })

# print(out2, digits = 5, annotation = FALSE)
