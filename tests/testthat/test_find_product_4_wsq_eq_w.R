test_that("wsq == w", {

library(testthat)
library(manymome)

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(1234)
dat$w1 <- sample(c(-1, 1), n, replace = TRUE)
dat$w2 <- sample(c(-1, 1), n, replace = TRUE)
if (!is_testing()) print(xtabs(~ w1 + w2, dat))
dat_no_prod <- dat
dat$xw1 <- dat$x * dat$w1
dat$yw1 <- dat$y * dat$w1
dat$w1w2 <- dat$w1 * dat$w2

mod <-
"
y ~ x + w1 + m1 + w2 + xw1
m2 ~ m1 + y + w1 + yw1
"

fit <- lavaan::sem(
  mod,
  dat
)

mod2 <-
"
y ~ x + w1 + m1 + w2 + x:w1
m2 ~ m1 + y + w1 + y:w1
"

fit2 <- lavaan::sem(
  mod2,
  dat_no_prod
)

# Should not loop
out_1 <- find_all_products(dat[, c("y", "m1", "x", "w1", "w2", "xw1", "yw1")])
out_2 <- find_all_products(dat[, c("y", "m1", "x", "w1", "w2", "w1w2")])
out_3 <- find_all_products(
  data = dat,
  fit = fit
)
out_4 <- find_all_products(
  data = dat_no_prod,
  fit = fit2
)

# OK to keep duplicated terms. Only one will be used
# in indirect_i()
expect_in(
  c("xw1", "yw1"),
  names(out_1)
)
expect_in(
  c("w1w2"),
  names(out_2)
)
expect_in(
  c("xw1", "yw1", "w1w2"),
  names(out_3)
)
expect_setequal(
  out_4,
  list()
)

ind3 <- cond_indirect_effects(
  wlevels = c("w1"),
  x = "x",
  y = "m2",
  m = "y",
  fit = fit
)
if (!is_testing()) print(ind3b)

expect_no_error(suppressWarnings(ind3b <- indirect_effect(
  x = "x",
  y = "m2",
  m = "y",
  fit = fit
)))
if (!is_testing()) print(ind3b)


dat2 <- modmed_x1m3w4y1
dat2$xw1 <- dat2$x * dat2$w1
dat2$xw2 <- dat2$x * dat2$w2
dat2$w1w2 <- dat2$w1 * dat2$w2
dat2$xw1w2 <- dat2$x * dat2$w1 * dat2$w2

# Should not loop
out_1 <- find_all_products(dat2[, c("y", "m1", "x", "w1", "w2", "xw1", "xw2", "xw1w2", "w1w2")])
expect_setequal(
  names(out_1),
  c("xw1", "xw2", "w1w2", "xw1w2")
)

})

