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
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 6:7]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)

outputs <- list(lm_m1, lm_m2, lm_m3, lm_y)
out <- lm2boot_out(outputs = outputs, R = 5, seed = 81674958)
out_chk <- lm(m1 ~ x * w1, data = out[[2]]$data)

out2 <- lm_boot2est_i(dat, outputs = outputs)

out2_db <- do_boot(fit = outputs, R = 5, seed = 81674958)

test_that("lm2boot_out", {
    expect_identical(
        unname(out[[2]]$est$est[c(4, 1:3)]),
        unname(coef(out_chk))
      )
    expect_identical(
        unname(out2$est$est[c(4, 1:3)]),
        unname(coef(lm_m1))
      )
    expect_identical(
        out2_db,
        out
      )
  })
