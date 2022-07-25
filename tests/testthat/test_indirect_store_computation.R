library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b22 * w2 + b23 * w3 + d22 * m1:w2 + d23 * m1:w3
m3 ~ a3 * m2  + b3 * w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

ce_1b_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues, computation_digits = 10)
eval(parse(text = ce_1b_chk$computation_values))
ce_1b_chk$indirect
ce_1b_chk$computation_symbols

test_that("test stored computation", {
    expect_equal(eval(parse(text = ce_1b_chk$computation_values)),
                 ce_1b_chk$indirect,
                 ignore_attr = TRUE)
  })

test_that("Check warning", {
    expect_warning(ce_2_chk <- indirect_i(x = "x", y = "m1", fit = fit, computation_digits = 10))
    expect_equal(eval(parse(text = ce_2_chk$computation_values)),
                 ce_2_chk$indirect,
                 ignore_attr = TRUE)
  })

test_that("Check warning", {
    expect_warning(ce_3_chk <- indirect_i(x = "x", y = "m2", m = "m1",
                   wvalues = c(w1 = 2, w3 = 5),
                   fit = fit, computation_digits = 10))
    expect_equal(eval(parse(text = ce_3_chk$computation_values)),
                 ce_3_chk$indirect,
                 ignore_attr = TRUE)
  })

test_that("test stored computation, standardized", {
    ce_1b_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                          wvalues = wvalues, computation_digits = 10,
                          standardized_x = TRUE)
    expect_equal(eval(parse(text = ce_1b_chk$computation_values)),
                 ce_1b_chk$indirect,
                 ignore_attr = TRUE)
  })