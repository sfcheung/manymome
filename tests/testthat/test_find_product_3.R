library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(1234)
dat$w1 <- sample(0:1, n, replace = TRUE)
dat$w2 <- sample(0:1, n, replace = TRUE)
dat$w2 <- ifelse(dat$w1 == 0, 0, dat$w2)
xtabs(~ w1 + w2, dat)

lm_m <- lm(m1 ~ x + w1 + w2, dat)
lm_y <- lm(y ~ m1, dat)
lm_all <- lm2list(lm_m, lm_y)

# Should not loop
out_1 <- find_all_products(dat[, c("y", "m1", "x", "w1", "w2")])

test_that("find_product: xy == x", {
    expect_true(length(out_1) == 0)
  })
