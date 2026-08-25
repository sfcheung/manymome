library(testthat)
library(manymome)

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(1234)
dat$w1 <- sample(0:1, n, replace = TRUE)
dat$w1_x <- dat$w1 * dat$x

out_1 <- find_all_products(dat[, c("y", "w1", "w1_x", "x")])

test_that("find_product: xw == x * xw", {
  expect_setequal(
    out_1[[1]],
    c("w1", "x"))
})
