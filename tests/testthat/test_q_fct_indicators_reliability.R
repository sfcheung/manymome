library(testthat)
library(manymome)
library(psych)
suppressMessages(library(semTools))
suppressMessages(library(lavaan))

test_that("indicators: reliability: sem", {

dat <- data_sem

ind <- list(
  y = c("-x01", "x02", "x03"),
  m = c("x04", "x05", "x09"),
  c2 = c("x11", "-x13", "x14")
)

ind_m <- ind$m

out <- scale_reliability_i_sem(
  indicators = ind_m,
  data = dat
)

mod_m <- "f =~ x04 + x05 + x09"
fit <- cfa(mod_m, dat, std.lv = TRUE)
out_chk <- compRelSEM(fit, simplify = TRUE)

expect_equal(out$reliability,
             as.numeric(out_chk))

out <- scale_reliability(
  indicators = ind,
  data = dat
)

expect_equal(out$reliability["m"],
             as.numeric(out_chk),
             ignore_attr = TRUE)

out <- scale_reliability_i_omega(
  indicators = ind_m,
  data = dat
)

out_chk <- suppressWarnings(suppressMessages(psych::omega(
  dat[, ind_m],
  nfactors = 1
)))

expect_equal(out$reliability,
             as.numeric(out_chk$omega.tot))

# Reverse items

out <- scale_reliability_i_sem(
  indicators_i = ind$c2,
  data = dat
)

dat2 <- dat
dat2$x13 <- -1 * dat2$x13
mod_c2 <- "f =~ x11 + x13 + x14"
fit <- cfa(mod_c2, dat2, std.lv = TRUE)
out_chk <- compRelSEM(fit, simplify = TRUE)

expect_equal(out$reliability,
             as.numeric(out_chk))


out <- scale_reliability_i_omega(
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
             as.numeric(out_chk$omega.tot))

})
