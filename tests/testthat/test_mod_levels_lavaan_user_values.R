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

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m1 ~ x + w1 + x:w1
m2 ~ m1 + gpgp2 + gpgp3 + citybeta + citygamma + citysigma
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)

out_i_n_user <- mod_levels_i_lm_numerical(fit, w = "w1", values = c(-2, 2, 3, 5, 8))
out_i_n_user2 <- mod_levels_i_lm_numerical(fit, w = "w1", values = c(low = 2, hi = 5, med = 1))
out_i_n_user3 <- mod_levels_i_lm_numerical(fit, w = "w1", values = c(3, 2, 1, NA, 8, NA))

out_i_c <- mod_levels_i_lm_categorical(fit, w = c("gpgp2", "gpgp3"),
                                       values = list(gp2 = c(1, 0),
                                                     gp1 = c(0, 0)))

out_n_user <- mod_levels(fit, w = "w1", values = c(-2, 2, 3, 5, 8))

out_c_user <- mod_levels(fit, w = c("gpgp2", "gpgp3"),
                                       values = list(gp2 = c(1, 0),
                                                     gp1 = c(0, 0)))

# out_l_1 <- mod_levels_list("w4", "w1", fit = fit)
# out_l_2 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit)
# out_l_3 <- mod_levels_list(c("gpgp2", "gpgp3"), "w4", fit = fit)
# out_l_4 <- mod_levels_list(c("gpgp2", "gpgp3"), c("citybeta", "citygamma", "citysigma"), "w4", fit = fit)


test_that("mod_levels: lavaan: user values", {
    expect_equal(unlist(out_i_n_user), rev(c(-2, 2, 3, 5, 8)), ignore_attr = TRUE)
    expect_equal(unlist(out_i_n_user2), rev(c(2, 5, 1)), ignore_attr = TRUE)
    expect_equal(rownames(out_i_n_user2), rev(c("low", "hi", "med")), ignore_attr = TRUE)
    expect_equal(unlist(out_i_n_user3), rev(c(3, 2, 1, 8)), ignore_attr = TRUE)
    expect_equal(out_i_c, data.frame(x = c(1, 0), y = c(0, 0)), ignore_attr = TRUE)
    expect_equal(rownames(out_i_c), c("gp2", "gp1"), ignore_attr = TRUE)
    expect_equal(out_n_user, out_i_n_user, ignore_attr = TRUE)
    expect_equal(out_c_user, out_i_c, ignore_attr = TRUE)
  })

test_that("mod_levels: lavaan: user values: error", {
    expect_error(mod_levels_i_lm_numerical(fit, w = "w1", values = c("a","b")))
    expect_error(mod_levels_i_lm_categorical(fit, w = c("gpgp2", "gpgp3"),
                                       values = list(gp2 = c(1, 1),
                                                     gp1 = c(0, 0))))
    expect_error(mod_levels_i_lm_categorical(fit, w = c("gpgp2", "gpgp3"),
                                       values = list(c(1, 0),
                                                     c(0, 0))))
  })