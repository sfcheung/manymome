library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("lv moderation by SAM: Single-Group", {

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f3 ~ a*f1 + f2 + d*f1:f2
   f2 ~~ f2v*f2
  '

fit <- sam(
  mod,
  data = data_sem
)

summary(fit)

out1 <- cond_effects(
  wlevels = "f2",
  x = "f1",
  y = "f3",
  fit = fit
)

est <- parameterEstimates(fit)

f2sd <- sqrt(est[est$label == "f2v", "est"])
a <- est[est$label == "a", "est"]
d <- est[est$label == "d", "est"]

a_lo <- a - f2sd * d
a_hi <- a + f2sd * d

expect_equal(c(a_hi, a, a_lo),
             coef(out1),
             ignore_attr = TRUE)

plot(out1)

})
