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

out_n_sd <- mod_levels(fit, w = "w1")
out_n_p <- mod_levels(fit, w = "w4", w_method = "percentile")
out_n_sd2 <- mod_levels(fit, w = "w1", sd_from_mean = c(-1, 4))
out_n_p2 <- mod_levels(fit, w = "w1", w_method = "percentile", percentiles = c(.25, .75))

out_c <- mod_levels(fit, w = c("gpgp2", "gpgp3"), w_type = "categorical")
out_c2 <- mod_levels(fit, w = c("citybeta", "citygamma", "citysigma"), w_type = "categorical")
out_c3 <- mod_levels(fit, w = c("gpgp2", "gpgp3"), prefix = "gp", w_type = "categorical")

out_m_1 <- merge_mod_levels(out_n_sd, out_n_p)
out_m_2 <- merge_mod_levels(out_n_sd, out_c)
out_m_3 <- merge_mod_levels(out_n_sd, out_c2)
out_m_4 <- merge_mod_levels(out_c2, out_n_p2)
out_m_5 <- merge_mod_levels(out_c, out_c2)

out_m_l_2 <- merge_mod_levels(mod_levels_list("w1", c("gpgp2", "gpgp3"), fit = fit))
out_m_l_m_2 <- mod_levels_list("w1", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

out_mm_1 <- merge_mod_levels(mod_levels(fit, w = "w1"),
                             mod_levels(fit, w = c("gpgp2", "gpgp3"), w_type = "categorical"),
                             mod_levels(fit, w = "w4", w_method = "percentile"))

# out_mm_1[, c(2, 4, 1)]

test_that("[.wlevels", {
    # Check one w
    expect_identical(attr(out_n_sd[c(3, 1), ], "wname"), "w1")
    expect_identical(attr(out_n_sd[c(3, 1), ], "w_type"), "numeric")
    expect_identical(dim(out_n_sd[c(3, 1), ]), c(2L, 1L))
    expect_identical(attr(out_c2[c(3, 1), ], "wname"), "city")
    expect_identical(attr(out_c2[c(3, 1), ], "w_type"), "categorical")
    expect_identical(dim(out_c2[c(3, 1), ]), c(2L, 3L))

    # Check wvars
    expect_identical(attr(out_mm_1[c(4, 2, 3)], "wvars"),
                     list(gp = c("gpgp2", "gpgp3"), w4 = "w4"))
    expect_identical(attr(out_mm_1[c("w4", "w1", "gpgp3", "gpgp2")], "wvars"),
                     list(w1 = "w1", gp = c("gpgp2", "gpgp3"), w4 = "w4"))
    expect_identical(attr(out_mm_1[c(2, 3)], "wvars"),
                     list(gp = c("gpgp2", "gpgp3")))
    expect_identical(attr(out_mm_1[, c(4, 2, 3)], "wvars"),
                     list(gp = c("gpgp2", "gpgp3"), w4 = "w4"))
    expect_identical(attr(out_mm_1[, c(4, 1, 2, 3)], "wvars"),
                     list(w1 = "w1", gp = c("gpgp2", "gpgp3"), w4 = "w4"))
    expect_identical(attr(out_mm_1[, c("gpgp3", "gpgp2")], "wvars"),
                     list(gp = c("gpgp2", "gpgp3")))
    expect_identical(attr(out_mm_1[1, c(4, 2, 3)], "wvars"),
                     list(gp = c("gpgp2", "gpgp3"), w4 = "w4"))
    expect_identical(attr(out_mm_1[c(3, 7, 2), c(4, 1, 2, 3)], "wvars"),
                     list(w1 = "w1", gp = c("gpgp2", "gpgp3"), w4 = "w4"))
    expect_identical(attr(out_mm_1[c(2, 5, 2), c("gpgp3", "gpgp2")], "wvars"),
                     list(gp = c("gpgp2", "gpgp3")))

    # Check w_type
    expect_identical(sort(attr(out_mm_1[c(4, 2, 3)], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")[2:3]))
    expect_identical(sort(attr(out_mm_1[c("w4", "w1", "gpgp3", "gpgp2")], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")))
    expect_identical(sort(attr(out_mm_1[c(2, 3)], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")[2]))
    expect_identical(sort(attr(out_mm_1[, c(4, 2, 3)], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")[2:3]))
    expect_identical(sort(attr(out_mm_1[, c(4, 1, 2, 3)], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")))
    expect_identical(sort(attr(out_mm_1[, c("gpgp3", "gpgp2")], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")[2]))
    expect_identical(sort(attr(out_mm_1[1, c(4, 2, 3)], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")[2:3]))
    expect_identical(sort(attr(out_mm_1[c(3, 7, 2), c(4, 1, 2, 3)], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")))
    expect_identical(sort(attr(out_mm_1[c(2, 5, 2), c("gpgp3", "gpgp2")], "w_type")),
                     sort(c(w1 = "numeric", gp = "categorical", w4 = "numeric")[2]))

    # Check w_levels
    expect_identical(attr(out_mm_1[c(1, 5, 7), c(4, 2, 3)], "wlevels"),
                     attr(out_mm_1, "wlevels")[c(1, 5, 7), ])
    expect_identical(attr(out_mm_1[rownames(out_mm_1)[c(1, 5, 7)], c(4, 2, 3)], "wlevels"),
                     attr(out_mm_1, "wlevels")[c(1, 5, 7), ])

    # Check drop
    expect_equal(out_mm_1[, 1],
                 as.data.frame(out_mm_1)[, 1],
                 ignore_attr = TRUE)
    expect_equal(dim(out_mm_1[4]),
                 c(nrow(out_mm_1), 1))
    expect_equal(out_mm_1$gpgp2,
                 as.data.frame(out_mm_1)[, 2, drop = TRUE],
                 ignore_attr = TRUE)
  })

test_that("check_gp_columns", {
    expect_true(check_gp_columns(out_mm_1, c(1, 4)))
    expect_true(check_gp_columns(out_mm_1, c(4)))
    expect_true(check_gp_columns(out_mm_1, c(2, 3)))
    expect_true(check_gp_columns(out_mm_1, c(4, 1)))
    expect_false(check_gp_columns(out_mm_1, c(4, 2)))
    expect_false(check_gp_columns(out_mm_1, c(2, 1, 4)))
    expect_false(check_gp_columns(out_mm_1, c("gpgp2", "w4", "w1")))
    expect_false(check_gp_columns(out_mm_1))
  })

test_that("[<-.wlevels]", {
    expect_error(out_n_sd[1] <- 1)
    expect_error(out_c[2, ] <- 1)
    expect_error(out_mm_1[1, 3] <- 10)
  })

