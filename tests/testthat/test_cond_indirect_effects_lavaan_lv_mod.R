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

expect_no_error(plot(out1))

})


test_that("lv moderated-mediation by SAM, mod-a: Single-Group", {

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~ a*f1 + f2 + d*f1:f2
   f4 ~ b*f3
   f2 ~~ f2v*f2
  '

fit <- sam(
  mod,
  data = data_sem
)

summary(fit)

out1 <- cond_indirect_effects(
  wlevels = "f2",
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit
)

est <- parameterEstimates(fit)

f2sd <- sqrt(est[est$label == "f2v", "est"])
a <- est[est$label == "a", "est"]
d <- est[est$label == "d", "est"]
b <- est[est$label == "b", "est"]

ind_lo <- (a - f2sd * d) * b
ind_hi <- (a + f2sd * d) * b
ind_me <- a * b

expect_equal(c(ind_hi, a * b, ind_lo),
             coef(out1),
             ignore_attr = TRUE)

})

test_that("lv moderated-mediation by SAM, mod-b: Single-Group", {

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~ a*f1
   f4 ~ b*f3 + f2 + d*f3:f2
   f2 ~~ f2v*f2
  '

fit <- sam(
  mod,
  data = data_sem
)

summary(fit)

out1 <- cond_indirect_effects(
  wlevels = "f2",
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit
)

est <- parameterEstimates(fit)

f2sd <- sqrt(est[est$label == "f2v", "est"])
a <- est[est$label == "a", "est"]
d <- est[est$label == "d", "est"]
b <- est[est$label == "b", "est"]

ind_lo <- a * (b - f2sd * d)
ind_hi <- a * (b + f2sd * d)
ind_me <- a * b

expect_equal(c(ind_hi, a * b, ind_lo),
             coef(out1),
             ignore_attr = TRUE)

})

test_that("lv moderated-mediation by SAM, mod-ab: Single-Group", {

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~ a*f1 + f2 + da*f1:f2
   f4 ~ b*f3 + f2 + db*f3:f2
   f2 ~~ f2v*f2
  '

fit <- sam(
  mod,
  data = data_sem
)

summary(fit)

out1 <- cond_indirect_effects(
  wlevels = "f2",
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit
)

est <- parameterEstimates(fit)

f2sd <- sqrt(est[est$label == "f2v", "est"])
a <- est[est$label == "a", "est"]
da <- est[est$label == "da", "est"]
db <- est[est$label == "db", "est"]
b <- est[est$label == "b", "est"]

ind_lo <- (a - f2sd * da) * (b - f2sd * db)
ind_hi <- (a + f2sd * da) * (b + f2sd * db)
ind_me <- a * b

expect_equal(c(ind_hi, a * b, ind_lo),
             coef(out1),
             ignore_attr = TRUE)

})
