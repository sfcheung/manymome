skip_on_cran()
skip("WIP")

library(testthat)
library(manymome)
library(lavaan)

test_that("q function: model:: moderated: betaS", {

suppressWarnings(
out1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m1 -> m2 -> y1",
                      "x1 -> m1 -> m3 -> y1"),
            cov = c("c1", "c2"),
            moderators = list(
              "x1 -> m1" = "w1",
              "m1 -> m3" = "w2",
              "m1 -> m3" = "w1",
              "m2 -> y1" = "w2"
            ),
            data = data_mome_demo,
            R = 5,
            seed = 1234,
            ci_type = "boot",
            fit_method = "lm",
            parallel = FALSE,
            progress = !is_testing()
          )
)

names(out1)
chk <- summary(out1$lm_out, betaselect = TRUE)
coef1 <- chk[[1]]$coefficients

sd_x1 <- sd(data_mome_demo$x1)
sd_w1 <- sd(data_mome_demo$w1)
sd_m1 <- sd(data_mome_demo$m1)

# Check betaS of a product term
expect_equal(
  coef1["x1:w1", "Estimate"] * sd_x1 * sd_w1 / sd_m1,
  coef1["x1:w1", "betaS"]
)

coef1 <- chk[[3]]$coefficients

sd_m2 <- sd(data_mome_demo$m2)
sd_w2 <- sd(data_mome_demo$w2)
sd_y1 <- sd(data_mome_demo$y1)

# Check betaS of a product term
expect_equal(
  coef1["m2:w2", "Estimate"] * sd_m2 * sd_w2 / sd_y1,
  coef1["m2:w2", "betaS"]
)

})
