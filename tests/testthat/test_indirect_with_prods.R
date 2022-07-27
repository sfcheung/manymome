library(stdmodsem)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

# Moderate mediation
ce_1b_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues)

# Get prods
ce_1b_prods <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues, get_prods_only = TRUE)
ce_1b_chk_prods <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues, prods = ce_1b_prods)

test_that("Check indirect with prods", {
    expect_equal(
        ce_1b_chk$indirect,
        ce_1b_chk_prods$indirect,
        ignore_attr = TRUE
      )
  })
