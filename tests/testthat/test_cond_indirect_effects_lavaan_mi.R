skip("To be examined in an interactive session")
# Require Amelia
library(testthat)
library(manymome)
library(lavaan)
suppressMessages(library(semTools))
suppressMessages(library(Amelia))

# Categorical IVs in lavaan

dat_cat <- modmed_x1m3w4y1
dat_cat$gp[dat_cat$gp == "earth"] <- "gp1"
dat_cat$gp[dat_cat$gp == "mars"] <- "gp2"
dat_cat$gp[dat_cat$gp == "venus"] <- "gp3"
dat_cat_miss <- set_missing(dat_cat,
                            nmiss = .01,
                            seed = 54154)
dat_cat_miss <- cbind(dat_cat_miss, factor2var(dat_cat_miss$gp, prefix = "gp", add_rownames = FALSE))
dat_cat_miss <- cbind(dat_cat_miss, factor2var(dat_cat_miss$city, prefix = "city", add_rownames = FALSE))
head(dat_cat_miss)

mod_cat <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit_cat_lav <- sem(mod_cat,
                   dat_cat_miss,
                   meanstructure = TRUE,
                   fixed.x = FALSE,
                   missing = "fiml.x",
                   se = "standard",
                   baseline = FALSE,
                   warn = FALSE)

set.seed(235413)
dat_cat_mi <- amelia(dat_cat_miss[, 1:11],
                     m = 50,
                     noms = c(10, 11))$imputations
dat_cat_mi <- lapply(dat_cat_mi, function(x) {
    x <- cbind(x, factor2var(x$gp, prefix = "gp", add_rownames = FALSE))
    x
  })

fit_cat_mi <- sem.mi(mod_cat,
                     dat_cat_mi,
                     meanstructure = TRUE,
                     fixed.x = FALSE,
                     baseline = FALSE,
                     h1 = FALSE,
                     warn = FALSE)

# Continuous IVs in lavaan

dat <- modmed_x1m3w4y1
dat_miss <- set_missing(dat,
                        nmiss = .01,
                        seed = 54154)

mod <-
"
m1 ~ a1 * x
m2 ~ a2 * m1 + m1:w2 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x + m3:w4 + w4
"
fit1_lav <- sem(mod,
                dat_miss,
                meanstructure = TRUE,
                fixed.x = FALSE,
                se = "standard",
                baseline = FALSE,
                missing = "fiml.x",
                warn = FALSE)

mod2 <-
"
m1 ~ a1 * x
m2 ~ a2 * m1 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x
"
fit2_lav <- sem(mod2,
                dat_miss,
                meanstructure = TRUE,
                fixed.x = FALSE,
                se = "standard",
                baseline = FALSE,
                missing = "fiml.x",
                warn = FALSE)

set.seed(235413)
dat_mi <- amelia(dat_miss, m = 50, noms = c(10, 11))$imputations

fit1_mi <- sem.mi(mod, dat_mi,
                  meanstructure = TRUE,
                  fixed.x = FALSE,
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)
fit2_mi <- sem.mi(mod2, dat_mi,
                  meanstructure = TRUE,
                  fixed.x = FALSE,
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)

dat_lv <- simple_mediation_latent
dat_lv_miss <- set_missing(dat_lv,
                      nmiss = .01,
                      seed = 89741)

mod_lv <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * fx
fy ~ b * fm + cp * fx
indirect := a * b
"
fit_lv_lav <- sem(mod_lv,
                  dat_lv_miss,
                  meanstructure = TRUE,
                  missing = "fiml.x",
                  baseline = FALSE,
                  h1 = FALSE,
                  warn = FALSE)

set.seed(235413)
dat_lv_mi <- amelia(dat_lv_miss, m = 50)$imputations

fit_lv_mi <- sem.mi(mod_lv, dat_lv_mi,
                    meanstructure = TRUE,
                    missing = "fiml.x",
                    baseline = FALSE,
                    h1 = FALSE,
                    warn = FALSE)

# fit_cat_lav
# fit_cat_mi
# fit1_lav
# fit1_mi
# fit2_lav
# fit2_mi
# fit_lv_lav
# fit_lv_mi

# fit_cat

out_mm_1_lav <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit_cat_lav, merge = TRUE)
out_mm_1_mi <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit_cat_mi, merge = TRUE)

test_that("mod_levels_list: MI", {
    expect_identical(out_mm_1_lav,
                     out_mm_1_mi)
  })

out_1_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav)
out_1_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi)
out_2_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                standardized_x = TRUE)
out_2_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                standardized_x = TRUE)
out_3_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                standardized_y = TRUE)
out_3_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                standardized_y = TRUE)
out_4_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                standardized_x = TRUE, standardized_y = TRUE)
out_4_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                standardized_x = TRUE, standardized_y = TRUE)

out_5_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_5_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                mc_ci = TRUE, R = 1000, seed = 895732)

out_6_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                standardized_x = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_6_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                standardized_x = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)

out_7_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_7_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)

out_8_lav <- cond_indirect_effects(wlevels = out_mm_1_lav, x = "x", y = "y", m = c("m3"), fit = fit_cat_lav,
                standardized_x = TRUE, standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_8_mi <- cond_indirect_effects(wlevels = out_mm_1_mi, x = "x", y = "y", m = c("m3"), fit = fit_cat_mi,
                standardized_x = TRUE, standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)


# fit1

m1 ~ a1 * x
m2 ~ a2 * m1 + m1:w2 + w2
m3 ~ a3 * m2
y  ~ a4 * m3 + c4 * x + m3:w4 + w4

out_mm_2_lav <- mod_levels_list("w4", "w2", fit = fit1_lav, merge = TRUE)
out_mm_2_mi <- mod_levels_list("w4", "w2", fit = fit1_mi, merge = TRUE)

test_that("mod_levels_list: MI, continuous ws", {
    expect_identical(out_mm_2_lav,
                     out_mm_2_mi)
  })

out_1_1_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav)
out_1_1_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi)
out_1_2_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                standardized_x = TRUE)
out_1_2_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                standardized_x = TRUE)
out_1_3_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                standardized_y = TRUE)
out_1_3_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                standardized_y = TRUE)
out_1_4_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                standardized_x = TRUE, standardized_y = TRUE)
out_1_4_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                standardized_x = TRUE, standardized_y = TRUE)

out_1_5_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_1_5_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                mc_ci = TRUE, R = 1000, seed = 895732)

out_1_6_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                standardized_x = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_1_6_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                standardized_x = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)

out_1_7_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_1_7_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)

out_1_8_lav <- cond_indirect_effects(wlevels = out_mm_2_lav, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_lav,
                standardized_x = TRUE, standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)
out_1_8_mi <- cond_indirect_effects(wlevels = out_mm_2_mi, x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit1_mi,
                standardized_x = TRUE, standardized_y = TRUE,
                mc_ci = TRUE, R = 1000, seed = 895732)


