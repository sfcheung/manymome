library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
dat$m1w4 <- dat$m1 * dat$w4

modmo1 <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
y ~ b * m1 + cp * x
indmome := d1 * b
"
fitmo1 <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmo1_boot_out <- do_boot(fitmo1, R = 50, seed = 1234, parallel = FALSE)

modmo2 <-
"
m1 ~ a * x
y ~ b * m1 + g4 * w4 + d4 * m1w4 + cp * x
indmome := d4 * a
"
fitmo2 <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmo2_boot_out <- do_boot(fitmo2, R = 50, seed = 1234, parallel = FALSE)

modmomo <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
y ~ b * m1 + g4 * w4 + d4 * m1w4 + cp *x
indmome := d1 * d4
"
fitmomo <- sem(modmomo, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmomo_boot_out <- do_boot(fitmomo, R = 50, seed = 1234, parallel = FALSE)

set.seed(1234)
fitmo1_boot <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                warn = FALSE)
set.seed(1234)
fitmo2_boot <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                warn = FALSE)
set.seed(1234)
fitmomo_boot <- sem(modmomo, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 50,
                warn = FALSE)

ind_mome1 <- index_of_mome(x = "x", y = "y", m = "m1", w = "w1",
                           fit = fitmo1, boot_ci = TRUE, boot_out = fitmo1_boot_out)
ind_mome2 <- index_of_mome(x = "x", y = "y", m = "m1", w = "w4",
                           fit = fitmo2, boot_ci = TRUE, boot_out = fitmo2_boot_out)
ind_momome <- index_of_momome(x = "x", y = "y", m = "m1", w = "w1", z = "w4",
                           fit = fitmomo, boot_ci = TRUE, boot_out = fitmomo_boot_out)

coef(ind_mome1)
confint(ind_mome1)
print(parameterEstimates(fitmo1_boot)[19, c("est", "ci.lower", "ci.upper")], nd = 8)

coef(ind_mome2)
confint(ind_mome2)
print(parameterEstimates(fitmo2_boot)[19, c("est", "ci.lower", "ci.upper")], nd = 8)

coef(ind_momome)
confint(ind_momome)
print(parameterEstimates(fitmomo_boot)[32, c("est", "ci.lower", "ci.upper")], nd = 8)

test_that("index_of_mome and index_of_momome", {
    expect_equal(
        coef(ind_mome1),
        parameterEstimates(fitmo1_boot)[19, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ind_mome2),
        parameterEstimates(fitmo2_boot)[19, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ind_momome),
        parameterEstimates(fitmomo_boot)[32, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_mome1)),
        unlist(parameterEstimates(fitmo1_boot)[19, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_mome2)),
        unlist(parameterEstimates(fitmo2_boot)[19, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_momome)),
        unlist(parameterEstimates(fitmomo_boot)[32, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
  })
