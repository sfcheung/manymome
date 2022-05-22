library(stdmodsem)
library(lavaan)
dat <- simple_mediation_latent
mod <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * fx
fy ~ b * fm + cp * fx
indirect := a * b
"
fit <- sem(mod, dat, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)
std <- standardizedSolution(fit)
est[c(10:12, 25), ]
out <- indirect(x = "fx", y = "fy", m = c("fm"), fit = fit)
out
out_std <- indirect(x = "fx", y = "fy", m = c("fm"), fit = fit,
                    standardized_x = TRUE,
                    standardized_y = TRUE)
out_std
std[c(10:12, 25), ]

test_that("indirect: latent variable", {
    expect_equal(out$indirect,
                 est[25, "est"])
    expect_equal(out_std$indirect,
                 std[25, "est.std"])
  })
