library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("find_product: skip indicators", {

dat <- data_sem
dat$x01x02 <- dat$x01 * dat$x02
dat$x04x13 <- dat$x04 * dat$x13
mod <-
'
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14 + x04x13
f3 ~  a1*f1 + a2*f2
f4 ~  b1*f1 + b3*f3
'
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

out_0 <- find_all_products(data_sem)
expect_true(length(out_0) == 0)

out_1 <- find_all_products(lavInspect(fit, "data"))
out_1
expect_identical(names(out_1),
                 "x04x13")

out_2 <- find_all_products(lavInspect(fit, "data"),
                           fit = fit)
expect_true(length(out_0) == 0)

out_3 <- find_all_products(dat)
expect_true(length(out_3) == 2)

})
