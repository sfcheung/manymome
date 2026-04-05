skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("do_boot: Empty cases", {

data(data_med_mod_ab1)
dat <- data_med_mod_ab1
dat[1:10, c("m", "y")] <- NA

set.seed(1234)
dat$gp <- sample(c("gp1", "gp2"), nrow(dat), replace = TRUE)

mod <-
"
m ~ x
y ~ m + x
"
suppressWarnings(fit <- sem(mod, dat, group = "gp", missing = "fiml"))
lavInspect(fit, "empty.idx")

system.time(
boot_out1 <- do_boot(fit, R = 5, seed = 1234, parallel = FALSE, progress = !is_testing())
)

boot_chk1 <- bootstrapLavaan(
  fit,
  R = 5,
  iseed = 1234
)

boot_est <- lapply(
              boot_out1,
              function(x) {
                est <- x$est
                est$label <- lav_partable_labels(est)
                boot_est <- stats::setNames(est$est, est$label)
                boot_est <- boot_est[colnames(boot_chk1)]
              })
boot_est <- do.call(rbind, boot_est)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE)

})


test_that("do_boot: Empty cases", {

skip("WIP")
# Need to check bootstrapLavaan() first.

dat <- data_sem
dat[c(1, 3, 5, 7), ] <- NA

set.seed(1234)
dat$gp <- sample(c("gp1", "gp2"), nrow(dat), replace = TRUE)

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
suppressWarnings(fit <- sem(mod, dat, group = "gp", missing = "fiml"))
lavInspect(fit, "empty.idx")

system.time(
boot_out1 <- do_boot(fit, R = 4, seed = 123, parallel = FALSE, progress = !is_testing())
)

boot_chk1 <- bootstrapLavaan(
  fit,
  R = 2,
  iseed = 1234,
  keep.idx = TRUE
)

boot_idx <- attr(boot_chk1, "boot.idx")
empty_idx <- lavInspect(fit, "empty.idx")

boot_idx_11 <- boot_idx[[1]][1, ]
range(boot_idx_11)
any(empty_idx[[1]] %in% boot_idx[[1]][1, ])

boot_est <- lapply(
              boot_out1,
              function(x) {
                est <- x$est
                est$label <- lav_partable_labels(est)
                boot_est <- stats::setNames(est$est, est$label)
                boot_est <- boot_est[colnames(boot_chk1)]
              })
boot_est <- do.call(rbind, boot_est)
expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE)

})
