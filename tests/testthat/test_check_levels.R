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
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

mm_n_one <- mod_levels(fit, w = "w1")
mm_c_one <- mod_levels(fit, w = c("gpgp2", "gpgp3"), w_type = "categorical")
mm_nc_list <- mod_levels_list("w1", c("gpgp2", "gpgp3"), fit = fit)
mm_nc_list_one <- mod_levels_list(c("gpgp2", "gpgp3"), fit = fit)
mm_nc_merged <- mod_levels_list("w1", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

check_wlevels(mm_n_one)
check_wlevels(mm_c_one)
check_wlevels(mm_nc_list)
check_wlevels(mm_nc_list_one)
check_wlevels(mm_nc_merged)

test_that("check_wlevels", {
    expect_equal(check_wlevels(mm_n_one), mm_n_one)
    expect_equal(check_wlevels(mm_c_one), mm_c_one)
    expect_equal(check_wlevels(mm_nc_list), merge_mod_levels(mm_nc_list))
    expect_equal(check_wlevels(mm_nc_list_one), merge_mod_levels(mm_nc_list_one))
    expect_equal(check_wlevels(mm_nc_merged), mm_nc_merged)
  })

ws1 <- c("w1", "w4")
ws2 <- list("w1", "w4")
ws3 <- list(c("gpgp2", "gpgp3"), "w4")
ws4 <- c("gpgp2", "gpgp3", "w4")

test_that("check_wlevels: NA", {
    expect_true(is.null(check_wlevels(ws1)))
    expect_true(is.null(check_wlevels(ws2)))
    expect_true(is.null(check_wlevels(ws3)))
    expect_true(is.null(check_wlevels(ws4)))
  })
