skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("do_boot: Latent mean", {

mod <-
"
f3 ~ a*f1
f4 ~ b*f3
f1 =~ x01 + x02 + x03
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~ 2*1
f4_mean := b*2
"
suppressWarnings(
fit <- sem(mod,
           data_sem,
           se = "boot",
           bootstrap = 2,
           iseed = 1234,
           meanstructure = TRUE)
)
suppressWarnings(est <- parameterEstimates(fit))

boot_out1 <- do_boot(
  fit,
  R = 2,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
boot_est <- lavInspect(fit, "boot")

# Check latent means
lv_means <- lavInspect(fit, "mean.lv")

expect_equal(lv_means["f4"],
             coef(fit, type = "user")["f4_mean"],
             ignore_attr = TRUE)

boot_out1_1 <- boot_out1[[1]]
i <- (boot_out1_1$est$lhs == "f4") &
     (boot_out1_1$est$rhs == "f3")

# The following two must be equal
expect_equal(boot_out1_1$est[i, "est"] * 2,
             boot_out1_1$implied_stats$mean_lv["f4"],
             ignore_attr = TRUE)

})


test_that("do_boot: Latent mean: Multigroup", {

set.seed(4321)
data_sem$gp <- sample(c("gp1", "gp2"), nrow(data_sem), replace = TRUE)

mod <-
"
f3 ~ f1
f4 ~ c(b1, b2)*f3
f1 =~ x01 + x02 + x03
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f3 ~ c(2, 3)*1
f3 ~ c(m1, m2)*1
f4_mean1 := b1*2
f4_mean2 := b2*3
"
suppressWarnings(
fit <- sem(mod,
           data_sem,
           group = "gp",
           se = "boot",
           bootstrap = 2,
           iseed = 1234,
           meanstructure = TRUE)
)
suppressWarnings(est <- parameterEstimates(fit))

boot_out1 <- do_boot(
  fit,
  R = 2,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing())
boot_est <- lavInspect(fit, "boot")

# Check latent means
lv_means <- lavInspect(fit, "mean.lv")

gp_labels <- lavInspect(fit, "group.label")
gp1 <- gp_labels[[1]]
gp2 <- gp_labels[[2]]

expect_equal(lv_means[[gp1]]["f4"],
             coef(fit, type = "user")["f4_mean1"],
             ignore_attr = TRUE)
expect_equal(lv_means[[gp2]]["f4"],
             coef(fit, type = "user")["f4_mean2"],
             ignore_attr = TRUE)

boot_out1_1 <- boot_out1[[1]]
i1 <- which(boot_out1_1$est$label == "b1")
i2 <- which(boot_out1_1$est$label == "b2")
j1 <- which(boot_out1_1$est$label == "m1")
j2 <- which(boot_out1_1$est$label == "m2")

# The following two must be equal
expect_equal(boot_out1_1$est[i1, "est"] * boot_out1_1$est[j1, "est"],
             boot_out1_1$implied_stats$mean[[gp1]]["f4"],
             ignore_attr = TRUE)
expect_equal(boot_out1_1$est[i2, "est"] * boot_out1_1$est[j2, "est"],
             boot_out1_1$implied_stats$mean[[gp2]]["f4"],
             ignore_attr = TRUE)

expect_equal(boot_out1_1$est[i1, "est"] * boot_out1_1$est[j1, "est"],
             boot_out1_1$implied_stats$mean_lv[[gp1]]["f4"],
             ignore_attr = TRUE)
expect_equal(boot_out1_1$est[i2, "est"] * boot_out1_1$est[j2, "est"],
             boot_out1_1$implied_stats$mean_lv[[gp2]]["f4"],
             ignore_attr = TRUE)

})
