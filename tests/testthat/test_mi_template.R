skip("Template for tests for MI")
skip_on_cran()
skip_if_not_installed("lavaan.mi")
skip_if_not_installed("Amelia")
# Require Amelia
library(testthat)
library(manymome)
library(lavaan)
suppressMessages(library(lavaan.mi))
suppressMessages(library(Amelia))

# Categorical IVs in lavaan

dat_cat <- modmed_x1m3w4y1
dat_cat$gp[dat_cat$gp == "earth"] <- "gp1"
dat_cat$gp[dat_cat$gp == "mars"] <- "gp2"
dat_cat$gp[dat_cat$gp == "venus"] <- "gp3"
dat_cat_miss <- add_na(dat_cat,
                       prop = .10,
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
                   se = "none",
                   baseline = FALSE,
                   warn = FALSE)

set.seed(235413)
dat_cat_mi <- amelia(dat_cat_miss[, 1:11],
                     m = 5,
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
dat_miss <- add_na(dat,
                   prop = .10,
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
                se = "none",
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
                se = "none",
                baseline = FALSE,
                missing = "fiml.x",
                warn = FALSE)

set.seed(235413)
dat_mi <- amelia(dat_miss, m = 5, noms = c(10, 11))$imputations

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
dat_lv_miss <- add_na(dat_lv,
                      prop = .10,
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
dat_lv_mi <- amelia(dat_lv_miss, m = 5)$imputations

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
