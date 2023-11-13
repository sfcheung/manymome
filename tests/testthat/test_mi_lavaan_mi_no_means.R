skip_on_cran()
# Require Amelia
library(testthat)
library(manymome)
library(lavaan)
suppressMessages(library(semTools))
suppressMessages(library(Amelia))

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

fit_lv <- sem(mod_lv, dat_lv,
              meanstructure = FALSE,
              baseline = FALSE,
              h1 = FALSE,
              warn = FALSE)
mc_out0 <- do_mc(fit_lv,
                 R = 5,
                 seed = 12345,
                 parallel = FALSE,
                 progress = FALSE)

fit_lv_m <- sem(mod_lv, dat_lv,
              meanstructure = TRUE,
              baseline = FALSE,
              h1 = FALSE,
              warn = FALSE)
mc_out0_m <- do_mc(fit_lv_m,
                   R = 5,
                   seed = 12345,
                   parallel = FALSE,
                   progress = FALSE)

set.seed(235413)
dat_lv_mi <- amelia(dat_lv_miss, m = 5)$imputations

fit_lv_mi <- sem.mi(mod_lv, dat_lv_mi,
                    meanstructure = FALSE,
                    baseline = FALSE,
                    estimator = "MLR",
                    h1 = FALSE,
                    warn = FALSE)
mc_out <- do_mc(fit_lv_mi,
                R = 5,
                seed = 12345,
                parallel = FALSE,
                progress = FALSE)

fit_lv_mi_m <- sem.mi(mod_lv, dat_lv_mi,
                      meanstructure = TRUE,
                      baseline = FALSE,
                      h1 = FALSE,
                      warn = FALSE)
mc_out_m <- do_mc(fit_lv_mi_m,
                  R = 5,
                  seed = 12345,
                  parallel = FALSE,
                  progress = FALSE)

test_that("MC with no mean structure", {
    expect_true(all(is.na(mc_out0[[1]]$implied_stats$mean)))
    expect_true(all(is.na(mc_out[[1]]$implied_stats$mean)))
    expect_true(is.numeric(mc_out0_m[[1]]$implied_stats$mean))
    expect_true(is.numeric(mc_out_m[[1]]$implied_stats$mean))
  })
