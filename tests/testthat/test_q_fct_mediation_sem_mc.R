skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem: mc", {

dat <- data_med_mod_parallel_cat
dat[1:5, "x"] <- NA
dat[6:10, "m1"] <- NA
dat[11:15, "y"] <- NA
dat[2:3, "w1"] <- NA
dat[3:5, "w2"] <- NA

out1 <- q_simple_mediation(
          x = "x",
          y = "y",
          m = "m1",
          cov = c("w1", "c1"),
          data = dat,
          R = 2001,
          seed = 1234,
          ci_type = "mc",
          fit_method = "sem",
          parallel = FALSE,
          progress = FALSE
        )
out1
expect_equal(lavaan::lavTech(out1$lm_out, "nobs"),
             295)

})

