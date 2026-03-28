library(testthat)
library(manymome)

test_that("indicators: reliability", {

dat <- data_sem

ind <- list(
  y = c("-x01", "x02", "x03"),
  m = c("x04", "x05", "x09"),
  c2 = c("x11", "-x13", "x14")
)

ind_m <- ind$m

out <- scale_reliability_i(
  indicators = ind_m,
  data = dat
)

out_chk <- suppressWarnings(suppressMessages(psych::omega(
  dat[, ind_m],
  nfactors = 1
)))

expect_equal(out$reliability,
             out_chk$omega.tot)


out <- scale_reliability(
  indicators = ind,
  data = dat
)

expect_equal(out$reliability["m"],
             out_chk$omega.tot,
             ignore_attr = TRUE)

# Reverse items

out <- scale_reliability_i(
  indicators_i = ind$c2,
  data = dat
)

key_tmp <- c(x11 = 1, x13 = -1, x14 = 1)
out_chk <- suppressWarnings(suppressMessages(psych::omega(
  dat[, gsub("^-", "", ind$c2)],
  nfactors = 1,
  key = key_tmp
)))

expect_equal(out$reliability,
             out_chk$omega.tot)
expect_equal(key_tmp,
             out_chk$key)

})
