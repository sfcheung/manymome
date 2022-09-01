library(testthat)
library(manymome)
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
ce_1b_chk2 <- (est[est$label == "a1", "est"] +
                wvalues["w1"] * est[est$label == "d1", "est"]) *
              (est[est$label == "a2", "est"] +
                wvalues["w2"] * est[est$label == "d2", "est"]) *
              (est[est$label == "a3", "est"] +
                wvalues["w3"] * est[est$label == "d3", "est"]) *
              (est[est$label == "a4", "est"] +
                wvalues["w4"] * est[est$label == "d4", "est"])

# Moderation only.
ce_2_chk <- indirect_i(x = "x", y = "m1", fit = fit,
                      wvalues = wvalues)
ce_2_chk2 <- (est[est$label == "a1", "est"] +
                wvalues["w1"] * est[est$label == "d1", "est"])

mod2 <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit2 <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est2 <- parameterEstimates(fit2)

# No moderation and no mediation
ce_3_chk <- indirect_i(x = "m2", y = "m3", fit = fit2,
                      wvalues = wvalues)
ce_3_chk2 <- est2[est2$label == "a3", "est"]

# Moderated mediation with standardization
ce_1b_stdx_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues, standardized_x = TRUE)
ce_1b_stdy_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues, standardized_y = TRUE)
ce_1b_stdboth_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                      wvalues = wvalues, standardized_x = TRUE, standardized_y = TRUE)
fit_implied <- lavInspect(fit, "implied")
sd_x <- sqrt(fit_implied$cov["x", "x"])
sd_y <- sqrt(fit_implied$cov["y", "y"])
ce_1b_stdx_chk2 <- ce_1b_chk2 * sd_x
ce_1b_stdy_chk2 <- ce_1b_chk2 / sd_y
ce_1b_stdboth_chk2 <- ce_1b_chk2 * sd_x / sd_y

# A path without moderation nor mediation
ce_no_w <- indirect_i(x = "m2", y = "m3", fit = fit2)

tmp <- capture.output(print(ce_1b_chk))
tmp <- capture.output(print(ce_2_chk))
tmp <- capture.output(print(ce_3_chk))
tmp <- capture.output(print(ce_1b_stdx_chk))
tmp <- capture.output(print(ce_1b_stdy_chk))
tmp <- capture.output(print(ce_1b_stdboth_chk))
tmp <- capture.output(print(ce_no_w))

test_that("Check indirect", {
    expect_equal(
        ce_1b_chk$indirect,
        ce_1b_chk2,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_2_chk$indirect,
        ce_2_chk2,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_3_chk$indirect,
        ce_3_chk2,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b_stdx_chk$indirect,
        ce_1b_stdx_chk2,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b_stdy_chk$indirect,
        ce_1b_stdy_chk2,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b_stdboth_chk$indirect,
        ce_1b_stdboth_chk2,
        ignore_attr = TRUE
      )
  })
