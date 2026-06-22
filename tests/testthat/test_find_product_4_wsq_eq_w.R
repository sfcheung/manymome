skip("WIP")

library(testthat)
library(manymome)

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(1234)
dat$w1 <- sample(c(-1, 1), n, replace = TRUE)
dat$w2 <- sample(c(-1, 1), n, replace = TRUE)
xtabs(~ w1 + w2, dat)
dat$xw1 <- dat$x * dat$w1
dat$yw1 <- dat$y * dat$w1
dat$w1w2 <- dat$w1 * dat$w2

# Should not loop
out_1 <- find_all_products(dat[, c("y", "m1", "x", "w1", "w2", "xw1", "yw1")])
out_2 <- find_all_products(dat[, c("y", "m1", "x", "w1", "w2", "w1w2")])

dat2 <- modmed_x1m3w4y1
dat2$xw1 <- dat2$x * dat2$w1
dat2$xw2 <- dat2$x * dat2$w2
dat2$w1w2 <- dat2$w1 * dat2$w2
dat2$xw1w2 <- dat2$x * dat2$w1 * dat2$w2

# Should not loop
out_1 <- find_all_products(dat2[, c("y", "m1", "x", "w1", "w2", "xw1", "xw2", "xw1w2", "w1w2")])

test_that("find_product: xy == x", {
    expect_true(length(out_1) == 0)
  })
