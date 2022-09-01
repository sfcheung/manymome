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
lm_ext <- lm(y ~ x, dat)
lm_diffn <- lm(y ~ m2 + m3 + x * w4, dat[-c(1:10), ])

lm_m1b <- lm(m1 ~ x * w1, dat[-1, ])
lm_m2b <- lm(m2 ~ m1 + gp + city, dat[-2, ])
lm_m3b <- lm(m3 ~ m1 + x * gp, dat[-3, ])
lm_yb <- lm(y ~ m2 + m3 + x * w4, dat[-4, ])

out <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)
tmp <- capture.output(print(out))
out_chk <- list(lm_m1, lm_m2, lm_m3, lm_y)

tmpdat <- dat[3:5, ]
pred_1 <- predict(out,
                  x = "x",
                  y = "y",
                  m = c("m1", "m2"),
                  newdata = tmpdat)
tmpdat_i <- tmpdat
pred_1_m1 <- predict(lm_m1, newdata = tmpdat_i)
tmpdat_i$m1 <- pred_1_m1
pred_1_m2 <- predict(lm_m2, newdata = tmpdat_i)
tmpdat_i$m2 <- pred_1_m2
pred_1_y <- predict(lm_y, newdata = tmpdat_i)

test_that("lm_list: predict", {
    expect_equal(pred_1,
                 pred_1_y,
                 ignore_attr = TRUE,
                 tolerance = 1e-6)
  })