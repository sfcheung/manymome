library(testthat)
library(manymome)
suppressMessages(suppressMessages(library(lavaan)))

dat <- data_med_mod_b_mod
dat$m2 <- dat$c2
dat$w3 <- dat$c1
dat$m1 <- dat$m
dat$w1x <- dat$w1 * dat$x
dat$w2m2 <- dat$w2 * dat$m2
dat$w3m2 <- dat$w3 * dat$m2
dat$w2w3 <- dat$w2 * dat$w3
dat$w3m2w2 <- dat$w3 * dat$m2 * dat$w2
mod <-
"
m1 ~ a * x + af1 * w1 + ad1 * w1x
m2 ~ b1 * m1
y ~ b2 * m2 + cp * x + bf2 * w2 + bf3 * w3 +
    bd2 * w2m2 + bd3 * w3m2 + bd23 * w2w3 + be23 * w3m2w2
# Covariances not added. Not necessary for this test.
"
fit <- sem(mod, dat, fixed.x = FALSE, warn = FALSE)
fit_m <- sem(mod, dat, fixed.x = FALSE, warn = FALSE, meanstructure = TRUE)
est <- parameterEstimates(fit)

# Fit by lm()
lm_m1 <- lm(m1 ~ x*w1, dat)
lm_m2 <- lm(m2 ~ m1, dat)
lm_y <- lm(y ~ m2*w2*w3 + x, dat)
lm_list <- lm2list(lm_m1, lm_m2, lm_y)

# Check model specification
coef(lm_m1)[-1]
est[(est$lhs == "m1") & (est$op == "~"), "est"]
coef(lm_m2)[-1]
est[(est$lhs == "m2") & (est$op == "~"), "est"]
coef(lm_y)[-1]
est[(est$lhs == "y") & (est$op == "~"), ][c(1, 3, 4, 2, 5, 6, 7, 8), "est"]

# lm_list
fit_list <- lm_from_lavaan_list(fit)
fit_m_list <- lm_from_lavaan_list(fit_m)

# coef
coef(fit_list[[1]])
coef(fit_m_list[[1]])

# predict

test_that("lm_from_lavaan: predict", {
  # 2026-01-04:
  # The tests can be more liberal, to allow for numerical differences
  # due to the libraries used.
    expect_equal(predict(fit_list[["y"]], dat[3:5, ]),
                 predict(lm_y, dat[3:5, ]) - coef(lm_y)[1],
                 ignore_attr = TRUE,
                 tolerance = 1e-4)
    expect_equal(predict(fit_m_list[["y"]], dat[3:5, ]),
                 predict(lm_y, dat[3:5, ]),
                 ignore_attr = TRUE,
                 tolerance = 1e-4)
  })

# predict for lm_from_lavaan_list
tmpdat <- dat[6:9, ]
yhat1 <- predict(fit_list,
               x = "x",
               y = "y",
               m = c("m1", "m2"),
               newdata = tmpdat)
tmpdat_i <- tmpdat
m1hat1 <- predict(lm_m1, tmpdat_i) - coef(lm_m1)[1]
tmpdat_i$m1 <- m1hat1
m2hat1 <- predict(lm_m2, tmpdat_i) - coef(lm_m2)[1]
tmpdat_i$m2 <- m2hat1
yhat1_chk <- predict(lm_y, tmpdat_i) - coef(lm_y)[1]

tmpdat <- dat[6:9, ]
yhat2 <- predict(fit_m_list,
               x = "x",
               y = "y",
               m = c("m1", "m2"),
               newdata = tmpdat)
tmpdat_i <- tmpdat
m1hat2 <- predict(lm_m1, tmpdat_i)
tmpdat_i$m1 <- m1hat2
m2hat2 <- predict(lm_m2, tmpdat_i)
tmpdat_i$m2 <- m2hat2
yhat2_chk <- predict(lm_y, tmpdat_i)

test_that("lm_from_lavaan_list: predict", {
    expect_equal(yhat1,
                 yhat1_chk,
                 ignore_attr = TRUE,
                 tolerance = 1e-4)
    expect_equal(yhat2,
                 yhat2_chk,
                 ignore_attr = TRUE,
                 tolerance = 1e-4)
  })