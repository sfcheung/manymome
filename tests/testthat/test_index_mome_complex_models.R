library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
dat$m2w4 <- dat$m2 * dat$w4
dat$m1w2 <- dat$m1 * dat$w2

modmo1 <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
m2 ~ b1 * m1
y ~ b2 * m2 + cp * x
indmome := d1 * b1 * b2
"
fitmo1 <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmo1_boot_out <- do_boot(fitmo1, R = 40, seed = 1234, parallel = FALSE,
                           progress = FALSE)

modmo2 <-
"
m1 ~ a * x
m2 ~ b1 * m1 + g2 * w2 + d2 * m1w2
y ~ b2 * m2 + cp * x
indmome := d2 * b2 * a
"
fitmo2 <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmo2_boot_out <- do_boot(fitmo2, R = 40, seed = 1234, parallel = FALSE, progress = FALSE)

modmomo1 <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
m2 ~ b1 * m1
y ~ b2 * m2 + g4 * w4 + d4 * m2w4 + cp *x
indmomo := d1 * d4 * b1
"
fitmomo1 <- sem(modmomo1, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmomo1_boot_out <- do_boot(fitmomo1, R = 40, seed = 1234, parallel = FALSE, progress = FALSE)


modmomo2 <-
"
m1 ~ a * x + g1 * w1 + d1 * xw1
m2 ~ b1 * m1 + g2 * w2 + d2 * m1w2
y ~ b2 * m2 + cp *x
indmomo := d1 * d2 * b2
"
fitmomo2 <- sem(modmomo2, dat, meanstructure = TRUE, fixed.x = FALSE)
fitmomo2_boot_out <- do_boot(fitmomo2, R = 40, seed = 1234, parallel = FALSE, progress = FALSE)

set.seed(1234)
fitmo1_boot <- sem(modmo1, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 40,
                warn = FALSE)
set.seed(1234)
fitmo2_boot <- sem(modmo2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 40,
                warn = FALSE)
set.seed(1234)
fitmomo1_boot <- sem(modmomo1, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 40,
                warn = FALSE)
set.seed(1234)
fitmomo2_boot <- sem(modmomo2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 40,
                warn = FALSE)

ind_mome1 <- index_of_mome(x = "x", y = "y", m = c("m1", "m2"), w = "w1",
                           fit = fitmo1, boot_ci = TRUE, boot_out = fitmo1_boot_out)
ind_mome2 <- index_of_mome(x = "x", y = "y", m = c("m1", "m2"), w = "w2",
                           fit = fitmo2, boot_ci = TRUE, boot_out = fitmo2_boot_out)
ind_momome1 <- index_of_momome(x = "x", y = "y", m = c("m1", "m2"), w = "w1", z = "w4",
                           fit = fitmomo1, boot_ci = TRUE, boot_out = fitmomo1_boot_out)
ind_momome2 <- index_of_momome(x = "x", y = "y", m = c("m1", "m2"), w = "w1", z = "w2",
                           fit = fitmomo2, boot_ci = TRUE, boot_out = fitmomo2_boot_out)

coef(ind_mome1)
confint(ind_mome1)
print(parameterEstimates(fitmo1_boot)[22, c("est", "ci.lower", "ci.upper")], nd = 8)

coef(ind_mome2)
confint(ind_mome2)
print(parameterEstimates(fitmo2_boot)[22, c("est", "ci.lower", "ci.upper")], nd = 8)

coef(ind_momome1)
confint(ind_momome1)
print(parameterEstimates(fitmomo1_boot)[35, c("est", "ci.lower", "ci.upper")], nd = 8)

coef(ind_momome2)
confint(ind_momome2)
print(parameterEstimates(fitmomo2_boot)[35, c("est", "ci.lower", "ci.upper")], nd = 8)


test_that("index_of_mome and index_of_momome", {
    expect_equal(
        coef(ind_mome1),
        parameterEstimates(fitmo1_boot)[22, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ind_mome2),
        parameterEstimates(fitmo2_boot)[22, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ind_momome1),
        parameterEstimates(fitmomo1_boot)[35, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ind_momome2),
        parameterEstimates(fitmomo2_boot)[35, "est"],
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_mome1)),
        unlist(parameterEstimates(fitmo1_boot)[22, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_mome2)),
        unlist(parameterEstimates(fitmo2_boot)[22, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_momome1)),
        unlist(parameterEstimates(fitmomo1_boot)[35, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
    expect_equal(
        unlist(confint(ind_momome2)),
        unlist(parameterEstimates(fitmomo2_boot)[35, c("ci.lower", "ci.upper")]),
        tolerance = 1e-4,
        ignore_attr = TRUE
      )
  })
