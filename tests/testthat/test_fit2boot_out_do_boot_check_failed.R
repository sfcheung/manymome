library(stdmodsem)
library(lavaan)

dat0 <- simple_mediation_latent
mod <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * fx
fy ~ b * fm + cp * fx
indirect := a * b
"
dat <- dat0[1:80, ]
set.seed(8715)
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           baseline = FALSE,
           h1 = FALSE,
           warn = FALSE)
tmp1 <- capture.output(out1 <- fit2boot_out_do_boot(fit, R = 4, seed = 8734),
                      type = "message")

test_that("Check failed boot samples", {
    expect_true(any(grepl("failed in 2 bootstrap", tmp1)))
    expect_error(fit2boot_out_do_boot(fit, R = 1, seed = 1378),
                 "failed in all bootstrap")
  })