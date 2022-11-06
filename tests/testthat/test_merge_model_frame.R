library(testthat)
library(manymome)

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4 + x * w1 + m1 + gp + city + x * gp, dat)

out1 <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)
out2 <- lm2list(lm_y, lm_m3, lm_m2, lm_m1)

chk1 <- merge_model_frame(out1)
chk2 <- merge_model_frame(out2)


test_that("merge_model_frame: Order reversed", {
    expect_true(suppressWarnings(all.equal(apply(chk1, 1, mean),
                                           apply(chk2, 1, mean))))
  })
