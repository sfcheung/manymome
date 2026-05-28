library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("SAM: Indirect effects: Standardized", {

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
f2 ~ a2*f1
f3 ~ a3*f1
f4 ~  b2*f2 + b3*f3 + cp*f1
a2b2 := a2 * b2
a3b3 := a3 * b3
"

# The warning is expected
fit <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml",
  warn = FALSE
)
est <- parameterEstimates(fit, standardized = TRUE)
fit_implied <- lavInspect(fit, "cov.all")

std2 <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f2",
  fit = fit,
  standardized_x = TRUE,
  standardized_y = TRUE
)

std3 <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit,
  standardized_x = TRUE,
  standardized_y = TRUE
)

expect_equal(coef(std2),
             est[est$label == "a2b2", "std.all"],
             ignore_attr = TRUE)
expect_equal(coef(std3),
             est[est$label == "a3b3", "std.all"],
             ignore_attr = TRUE)


})
