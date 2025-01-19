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
    expect_equal(out_sum, out_sum_chk, ignore_attr = FALSE)
  })

test_that("summary_lm_list: ci", {
out_sum <- summary(out,
                   ci = TRUE,
                   level = .90)
out_sum_ci_chk <- confint(out[[3]], level = .90)
out_sum_ci <- out_sum[[3]]$coefficients[, 2:3]
expect_equal(out_sum_ci[, 1],
             out_sum_ci_chk[, 1],
             ignore_attr = TRUE)
expect_equal(out_sum_ci[, 2],
             out_sum_ci_chk[, 2],
             ignore_attr = TRUE)
})

test_that("summary_lm_list: beta", {
out_sum <- summary(out,
                   betaselect = TRUE)
datz2 <- dat
datz2$m2 <- scale(datz2$m2)[, 1]
datz2$m1 <- scale(datz2$m1)[, 1]
lm_m2_std <- lm(m2 ~ m1 + gp + city, datz2)

datz3 <- dat
datz3$m3 <- scale(datz3$m3)[, 1]
datz3$m1 <- scale(datz3$m1)[, 1]
datz3$x <- scale(datz3$x)[, 1]
lm_m3_std <- lm(m3 ~ m1 + x * gp, datz3)

datzy <- dat
datzy$y <- scale(datzy$y)[, 1]
datzy$m2 <- scale(datzy$m2)[, 1]
datzy$m3 <- scale(datzy$m3)[, 1]
datzy$x <- scale(datzy$x)[, 1]
datzy$w4 <- scale(datzy$w4)[, 1]
lm_y_std <- lm(y ~ m2 + m3 + x * w4, datzy)

expect_equal(out_sum[[2]]$coefficients$betaS,
             coef(lm_m2_std),
             ignore_attr = TRUE)
expect_equal(out_sum[[3]]$coefficients$betaS,
             coef(lm_m3_std),
             ignore_attr = TRUE)
expect_equal(out_sum[[4]]$coefficients$betaS,
             coef(lm_y_std),
             ignore_attr = TRUE)
})
