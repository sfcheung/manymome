
library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
lm_ext <- lm(y ~ x, dat)
lm_diffn <- lm(y ~ m2 + m3 + x * w4, dat[-c(1:10), ])

lm_m1b <- lm(m1 ~ x * w1, dat[-1, ])
lm_m2b <- lm(m2 ~ m1 + gp + city, dat[-2, ])
lm_m3b <- lm(m3 ~ m1 + x * gp, dat[-3, ])
lm_yb <- lm(y ~ m2 + m3 + x * w4, dat[-4, ])

out <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)
out_chk <- list(lm_m1, lm_m2, lm_m3, lm_y)
out_list <- out
class(out_list) <- "list"

test_that("lm2list", {
    expect_identical(out_list, out_chk, ignore_attr = FALSE)
    expect_error(lm2list(lm_m2, lm_ext))
    expect_error(lm2list(lm_m1, lm_m2, lm_m3, lm_diffn))
    expect_error(lm2list(lm_m1b, lm_m2b, lm_m3b, lm_yb))
  })