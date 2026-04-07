library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("SAM: Mediation: Multigroup", {

data_sem_mg <- data_sem
set.seed(54423)
data_sem_mg$gp <- sample(
                    paste0("gp", 1:3),
                    nrow(data_sem_mg),
                    replace = TRUE
                  )

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f2 ~  c(a121, a122, a123)*f1
   f3 ~  c(a131, a132, a133)*f1
   f4 ~  c(b241, b242, b243)*f2 +
         c(b341, b342, b343)*f3
   ab1241 := a121 * b241
   ab1242 := a122 * b242
   ab1243 := a123 * b243
   ab1341 := a131 * b341
   ab1342 := a132 * b342
   ab1343 := a133 * b343
  '
fit <- sam(
  model = mod,
  data = data_sem_mg,
  group = "gp",
  group.label = c("gp1", "gp2", "gp3"))

out <- cond_indirect_effects(
  x = "f1",
  y = "f4",
  m = "f2",
  fit = fit
)
out

est <- parameterEstimates(fit)

ab1241 <- est[est$label == "ab1241", "est"]
ab1242 <- est[est$label == "ab1242", "est"]
ab1243 <- est[est$label == "ab1243", "est"]

expect_equal(coef(out),
             c(ab1241, ab1242, ab1243),
             ignore_attr = TRUE)

out <- cond_indirect_effects(
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit
)
out

ab1341 <- est[est$label == "ab1341", "est"]
ab1342 <- est[est$label == "ab1342", "est"]
ab1343 <- est[est$label == "ab1343", "est"]

expect_equal(coef(out),
             c(ab1341, ab1342, ab1343),
             ignore_attr = TRUE)

})
