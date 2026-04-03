skip("WIP")

library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("SAM: Temp tests", {

# Test when functions will SAM

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~  a1*f1 + a2*f2
f4 ~  b1*f1 + b3*f3
a1b3 := a1 * b3
a2b3 := a2 * b3
"

# The warning is expected
fit <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml"
)
summary(fit)

ind <- indirect_effect(
  x = "f2",
  y = "f4",
  m = "f3",
  fit = fit
)

ind

boot_out <- do_boot(
  fit,
  R = 100,
  seed = 1234
)

ind <- indirect_effect(
  x = "f2",
  y = "f4",
  m = "f3",
  fit = fit,
  boot_ci = TRUE,
  boot_out = boot_out
)

})

test_that("SAM: do_boot", {

# Test when functions will SAM

data_sem_miss <- data_sem

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~  a1*f1 + a2*f2
f4 ~  b1*f1 + b3*f3
a1b3 := a1 * b3
a2b3 := a2 * b3
"

fit <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml"
)
summary(fit)

fitb <- sam(
  model = mod,
  data = data_sem_miss,
  se = "bootstrap",
  bootstrap.args = list(R = 5),
  iseed = 1234
)
vcov(fitb)[1:5, 1:5]
summary(fitb)

lavInspect(fitb, "boot")[1:5, 1:10]

fit2 <- sem(
  model = mod,
  data = data_sem_miss,
  missing = "fiml"
)
summary(fit2)

boot_out <- do_boot(
  fit,
  R = 5,
  seed = 1234,
  progress = !is_testing(),
  parallel = FALSE)

# They are not supposed to be the same
boot_out[[1]]$est[1:5, ]
boot_out[[2]]$est[1:5, ]

boot_outb <-  do_boot(
  fitb,
  progress = !is_testing(),
  parallel = FALSE)

# They are not supposed to be the same
boot_outb[[1]]$est[1:5, ]
boot_outb[[2]]$est[1:5, ]

boot_out2 <- do_boot(
  fit2,
  R = 5,
  seed = 1234,
  progress = !is_testing(),
  parallel = FALSE)

# They are not supposed to be the same
boot_out2[[1]]$est[1:5, ]
boot_out2[[2]]$est[1:5, ]

})
