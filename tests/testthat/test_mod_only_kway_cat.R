skip_on_cran()

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

# Moderation only

outmo_mm_1 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)

outmo_1 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit)
outmo_2 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE)
outmo_3 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_y = TRUE)
outmo_4 <- cond_indirect_effects(wlevels = outmo_mm_1, x = "x", y = "m3", fit = fit,
                               standardized_x = TRUE, standardized_y = TRUE)

# Kway

library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ x * gp, dat)
lm_y <- lm(y ~ m1 + m2 + x * w3 * city, dat)
fit <- lm2list(lm_m1, lm_m2, lm_y)

# Moderation only

outmo_mm_1 <- mod_levels("w1", fit = fit)
outmo_mm_2 <- mod_levels("w3", fit = fit)
outmo_mm_3 <- mod_levels(c("gpgp2", "gpgp3"), fit = fit)
outmo_mm_4 <- mod_levels(c("citybeta", "citygamma", "citysigma"), fit = fit)
outmo_mm <- merge_mod_levels(outmo_mm_1,
                             outmo_mm_2,
                             outmo_mm_3,
                             outmo_mm_4)
outmo_mm_24 <- merge_mod_levels(outmo_mm_2,
                                outmo_mm_4)

outmo_3 <- cond_indirect_effects(wlevels = outmo_mm_3, x = "x", y = "m2", fit = fit)
outmo_24 <- cond_indirect_effects(wlevels = outmo_mm_24, x = "x", y = "y", fit = fit)

dat$gpref2 <- relevel(factor(dat$gp), ref = "gp2")
lm_m2_gp2 <- lm(m2 ~ x * gpref2, dat)
outmo_3_gp2_chk <- sqrt(vcov(lm_m2_gp2)["x", "x"])
dat$gpref3 <- relevel(factor(dat$gp), ref = "gp3")
lm_m2_gp3 <- lm(m2 ~ x * gpref3, dat)
outmo_3_gp3_chk <- sqrt(vcov(lm_m2_gp3)["x", "x"])

dat$cityrefgamma <- relevel(factor(dat$city), ref = "gamma")
dat$w3_high <- dat$w3 - (mean(dat$w3) + sd(dat$w3))
lm_y_w3_high_gamma <- lm(y ~ m1 + m2 + x * w3_high * cityrefgamma, dat)
outmo_24_chk <- sqrt(vcov(lm_y_w3_high_gamma)["x", "x"])

test_that("Categorical moderator", {
    se_3 <- cond_effects_original_se(outmo_3)
    expect_equal(se_3$se[2],
                 outmo_3_gp2_chk,
                 ignore_attr = TRUE)
    expect_equal(se_3$se[3],
                 outmo_3_gp3_chk,
                 ignore_attr = TRUE)
    se_24 <- cond_effects_original_se(outmo_24)
    expect_equal(se_24$se[3],
                 outmo_24_chk,
                 ignore_attr = TRUE)
  })
