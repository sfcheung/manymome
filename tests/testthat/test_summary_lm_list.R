library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)

out <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)
out_sum <- summary(out)
tmp <- capture.output(print(out_sum))
class(out_sum) <- "list"
out_sum_chk <- lapply(out, summary)

test_that("summary_lm_list", {
    expect_identical(out_sum, out_sum_chk, ignore_attr = FALSE)
  })