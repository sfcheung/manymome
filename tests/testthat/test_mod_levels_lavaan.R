
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

# mod_levels(fit, w = list(gp = c("gpgp2", "gpgp3"),
#                          w1 = "w1"))
# Output:
# gp:
# data.frame(label = c("cat1", "cat2"),
#            gpgp2 = c(1, 0),
#            gpgp3 = c(0, 1))
# w1:
# data.frame(label = c("Low", "Medium", "High"),
#            w1 = c(-2, 0, 2))

out_i_n_sd <- mod_levels_i_lavaan_numerical(fit, w = "w1")
out_i_n_p <- mod_levels_i_lavaan_numerical(fit, w = "w1", w_method = "percentile")
out_i_n_sd2 <- mod_levels_i_lavaan_numerical(fit, w = "w1", sd_from_mean = c(-1, 4))
out_i_n_p2 <- mod_levels_i_lavaan_numerical(fit, w = "w1", w_method = "percentile", percentiles = c(.25, .75))

out_i_c <- mod_levels_i_lavaan_categorical(fit, w = c("gpgp2", "gpgp3"))
out_i_c2 <- mod_levels_i_lavaan_categorical(fit, w = c("citybeta", "citygamma", "citysigma"))
out_i_c3 <- mod_levels_i_lavaan_categorical(fit, w = c("gpgp2", "gpgp3"), prefix = "gp")

out_n_sd <- mod_levels(fit, w = "w1")
out_n_p <- mod_levels(fit, w = "w1", w_method = "percentile")
out_n_sd2 <- mod_levels(fit, w = "w1", sd_from_mean = c(-1, 4))
out_n_p2 <- mod_levels(fit, w = "w1", w_method = "percentile", percentiles = c(.25, .75))

out_c <- mod_levels(fit, w = c("gpgp2", "gpgp3"))
out_c2 <- mod_levels(fit, w = c("citybeta", "citygamma", "citysigma"))
out_c3 <- mod_levels(fit, w = c("gpgp2", "gpgp3"), prefix = "gp")

out_i_n_sd_chk <- mean(dat$w1) + sd(dat$w1) * rev(c(-1, 0, 1))
out_i_n_p_chk <- quantile(dat$w1, rev(c(.16, .50, .84)))
out_i_n_sd2_chk <- mean(dat$w1) + sd(dat$w1) * rev(c(-1, 4))
out_i_n_p2_chk <- quantile(dat$w1, rev(c(.25, .75)))

out_i_c_chk <- structure(list(gpgp2 = c(0, 1, 0), gpgp3 = c(0, 0, 1)), row.names = c("Reference",
"2", "3"), class = "data.frame")
out_i_c2_chk <- structure(list(citybeta = c(0, 1, 0, 0), citygamma = c(0, 0,
1, 0), citysigma = c(0, 0, 0, 1)), row.names = c("Reference",
"beta", "gamma", "sigma"), class = "data.frame")
out_i_c3_chk <- structure(list(gpgp2 = c(0, 1, 0), gpgp3 = c(0, 0, 1)), row.names = c("Reference",
"gp2", "gp3"), class = "data.frame")

out_l_1 <- mod_levels_list("w4", "w1", fit = fit)
out_l_2 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit)
out_l_3 <- mod_levels_list(c("gpgp2", "gpgp3"), "w4", fit = fit)
out_l_4 <- mod_levels_list(c("gpgp2", "gpgp3"), c("citybeta", "citygamma", "citysigma"), "w4", fit = fit)

test_that("mod_levels: lavaan", {
    expect_equal(unlist(out_i_n_sd), out_i_n_sd_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_i_n_p), out_i_n_p_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_i_n_sd2), out_i_n_sd2_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_i_n_p2), out_i_n_p2_chk, ignore_attr = TRUE)
    expect_equal(out_i_c, out_i_c_chk, ignore_attr = TRUE)
    expect_equal(out_i_c2, out_i_c2_chk, ignore_attr = TRUE)
    expect_equal(out_i_c3, out_i_c3_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_n_sd), out_i_n_sd_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_n_p), out_i_n_p_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_n_sd2), out_i_n_sd2_chk, ignore_attr = TRUE)
    expect_equal(unlist(out_n_p2), out_i_n_p2_chk, ignore_attr = TRUE)
    expect_equal(out_c, out_i_c_chk, ignore_attr = TRUE)
    expect_equal(out_c2, out_i_c2_chk, ignore_attr = TRUE)
    expect_equal(out_c3, out_i_c3_chk, ignore_attr = TRUE)
  })

out_c_ref <- mod_levels(fit, w = c("gpgp2", "gpgp3"), reference_group_label = "Group 1")
out_c2_ref <- mod_levels(fit, w = c("citybeta", "citygamma", "citysigma"), reference_group_label = "alpha")
out_c3_ref <- mod_levels(fit, w = c("gpgp2", "gpgp3"), prefix = "gp", reference_group_label = "gp1")

out_c_ref_chk <- structure(list(gpgp2 = c(0, 1, 0), gpgp3 = c(0, 0, 1)), row.names = c("Group 1",
"2", "3"), class = "data.frame", wname = "gpgp", wlevels = structure(list(
    gpgp = c("Group 1", "2", "3")), class = "data.frame", row.names = c(NA,
-3L)), w_type = "categorical")

out_c2_ref_chk <- structure(list(citybeta = c(0, 1, 0, 0), citygamma = c(0, 0,
1, 0), citysigma = c(0, 0, 0, 1)), row.names = c("alpha", "beta",
"gamma", "sigma"), class = "data.frame", wname = "city", wlevels = structure(list(
    city = c("alpha", "beta", "gamma", "sigma")), class = "data.frame", row.names = c(NA,
-4L)), w_type = "categorical")

out_c3_ref_chk <- structure(list(gpgp2 = c(0, 1, 0), gpgp3 = c(0, 0, 1)), row.names = c("gp1",
"gp2", "gp3"), class = "data.frame", wname = "gp", wlevels = structure(list(
    gp = c("gp1", "gp2", "gp3")), class = "data.frame", row.names = c(NA,
-3L)), w_type = "categorical")

test_that("mod_levels: lavaan, custome label for reference group", {
    expect_equal(out_c_ref, out_c_ref_chk, ignore_attr = TRUE)
    expect_equal(out_c2_ref, out_c2_ref_chk, ignore_attr = TRUE)
    expect_equal(out_c3_ref, out_c3_ref_chk, ignore_attr = TRUE)
  })
