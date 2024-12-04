skip_on_cran()

library(manymome)
suppressMessages(library(lavaan))

# lm

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

# Moderated mediation

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)
fit_boot_out <- lm2boot_out(fit, R = 40, seed = 87415, progress = FALSE)
out_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE, boot_out = fit_boot_out)

# Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

outmo_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit)

outmo_1_std <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                 standardized_x = TRUE)

outmo_1_boot <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                      boot_ci = TRUE,
                                      R = 40,
                                      progress = FALSE,
                                      parallel = FALSE,
                                      seed = 1234)

outmo_1_std <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                 standardized_x = TRUE)

outmo_1_std_boot <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                 boot_ci = TRUE,
                                 R = 40,
                                 progress = FALSE,
                                 parallel = FALSE,
                                 standardized_x = TRUE)

# lavaan

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
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit_lav <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "boot", bootstrap = 5,
                warn = FALSE)

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

# Suppress warnings due to small number of bootstrap samples.

suppressWarnings(out_lav_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit))
suppressWarnings(out_lav_2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit,
                               standardized_x = TRUE))
suppressWarnings(out_lav_5 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               boot_ci = TRUE))
suppressWarnings(out_lav_6 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit_boot,
                               standardized_x = TRUE,
                               boot_ci = TRUE))

suppressWarnings(outmo_lav_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit))
suppressWarnings(outmo_lav_1_std <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                                                           standardized_x = TRUE))

# Target output
#        ind      CI.lo    CI.hi pvalue        SE
# 1 3.060021  2.1677888 4.039487   0.00 0.4596262
# 2 2.136191  1.4073608 2.925047   0.00 0.3583436
# 3 1.212360 -0.2880764 2.564034   0.08 0.5921044
#
#          std       CI.lo     CI.hi pvalue        SE
# 1 0.06344079 -0.20196663 0.3030855   0.64 0.1237579
# 2 0.12925647 -0.06795164 0.3469891   0.30 0.1058540
# 3 0.01399145 -0.19169346 0.2860562   0.76 0.1300183

test_that("lm", {
  expect_equal(class(as.data.frame(out_1))[1], "data.frame")
  expect_false("SE" %in% colnames(as.data.frame(out_1, pvalue = TRUE, se = TRUE)))
  expect_false("SE" %in% colnames(as.data.frame(out_6)))
  expect_true(all(match(c("pvalue", "SE"), colnames(as.data.frame(out_6, pvalue = TRUE, se = TRUE))) > 0))

  expect_equal(class(as.data.frame(outmo_1))[1], "data.frame")
  expect_equal(class(as.data.frame(outmo_1_std))[1], "data.frame")
  expect_true("pvalue" %in% colnames(as.data.frame(outmo_1_boot, pvalue = TRUE)))
  expect_true(is.character(as.data.frame(outmo_1_boot, to_string = TRUE, pvalue = TRUE)$ind))
  expect_true(all(match(c("pvalue", "SE"), colnames(as.data.frame(outmo_1_boot, pvalue = TRUE, se = TRUE)))))
  expect_true("pvalue" %in% colnames(as.data.frame(outmo_1_std_boot, pvalue = TRUE)))
  expect_true(all(match(c("pvalue", "SE"), colnames(as.data.frame(outmo_1_std_boot, pvalue = TRUE, se = TRUE)))))
})

test_that("lavaan", {
  expect_equal(class(as.data.frame(outmo_lav_1))[1], "data.frame")
  expect_equal(class(as.data.frame(outmo_lav_1_std))[1], "data.frame")
  expect_true("SE" %in% colnames(as.data.frame(outmo_lav_1, pvalue = FALSE)))
  expect_false("SE" %in% colnames(as.data.frame(outmo_lav_1_std, pvalue = TRUE)))

  expect_equal(class(as.data.frame(out_lav_1))[1], "data.frame")
  expect_equal(class(as.data.frame(out_lav_2))[1], "data.frame")
  expect_equal(class(as.data.frame(out_lav_5))[1], "data.frame")
  expect_equal(class(as.data.frame(out_lav_6))[1], "data.frame")
  expect_true(all(match(c("pvalue", "SE"), colnames(as.data.frame(out_lav_5, pvalue = TRUE, se = TRUE)))))
  expect_true(all(match(c("SE"), colnames(as.data.frame(out_lav_6, pvalue = FALSE, se = TRUE)))))
})
