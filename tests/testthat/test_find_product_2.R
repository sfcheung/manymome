library(stdmodsem)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4 + cp * x + b3 * w3 + d3 * x:w3 + b4x * x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

out_1 <- find_all_products(lavInspect(fit, "data"))
out_1_chk <- list(`m1:w2` = c("m1", "w2"), `m3:w4` = c("m3", "w4"), `x:w1` = c("x",
"w1"), `x:w3` = c("x", "w3"), `x:w4` = c("x", "w4"))


test_that("find_product", {
    expect_equal(
        out_1,
        out_1_chk
      )
  })