skip("WIP")

library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("SAM: lavaan functions", {

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

# The warning is expected
fit2 <- sem(
  model = mod,
  data = data_sem_miss,
  missing = "fiml",
  warn = FALSE
)

expect_identical(lavInspect(fit),
                 lavInspect(fit2))

expect_identical(lavNames(fit, type = "eqs.y"),
                 lavNames(fit2, type = "eqs.y"))

expect_identical(lavNames(fit, type = "eqs.x"),
                 lavNames(fit2, type = "eqs.x"))

expect_identical(names(coef(fit)),
                 names(coef(fit2)))

expect_true(is.numeric(lavInspect(fit, "timing")$total))

pt <- parameterTable(fit)
pt2 <- parameterTable(fit2)

pt_sorted <- pt[order(pt$lhs, pt$op, pt$rhs), ]
pt2_sorted <- pt2[order(pt2$lhs, pt2$op, pt2$rhs), ]
expect_equal(pt_sorted$lhs,
             pt2_sorted$lhs)
expect_equal(pt_sorted$rhs,
             pt2_sorted$rhs)
expect_equal(pt_sorted$op,
             pt2_sorted$op)

opt <- lavInspect(fit, "options")
opt2 <- lavInspect(fit2, "options")

setdiff(names(opt2), names(opt))

expect_equal(lavaan::lavTech(fit, "ngroups"),
             1)

expect_equal(lavaan::lavTech(fit, "group.label"),
             character(0))

expect_identical(pt$free,
                 pt2$free)

expect_equal(lavNames(fit, "ov"),
             lavNames(fit2, "ov"))

expect_equal(lavNames(fit, "lv"),
             lavNames(fit2, "lv"))

expect_equal(lavNames(fit, "ov.ind"),
             lavNames(fit2, "ov.ind"))

expect_equal(rownames(lavInspect(fit, "cov.all")),
             rownames(lavInspect(fit2, "cov.all")))

expect_identical(lavaan::lavInspect(fit, "mean.lv"),
                 lavaan::lavInspect(fit2, "mean.lv"))

expect_identical(names(lavaan::lavInspect(fit, "mean.ov")),
                 names(lavaan::lavInspect(fit2, "mean.ov")))

expect_identical(lavaan::lavTech(fit, what = "post.check"),
                 lavaan::lavTech(fit2, what = "post.check"))

expect_identical(lavaan::lavTech(fit, what = "converged"),
                 lavaan::lavTech(fit2, what = "converged"))

expect_identical(lavaan::lavInspect(fit, "data"),
                 lavaan::lavInspect(fit2, "data"))

expect_identical(lavaan::lav_partable_labels(pt),
                 lavaan::lav_partable_labels(pt2))

expect_identical(lavaan::lavInspect(fit, "empty.idx"),
                 lavaan::lavInspect(fit2, "empty.idx"))

expect_identical(lavaan::lavInspect(fit, "meanstructure"),
                 lavaan::lavInspect(fit2, "meanstructure"))

expect_identical(colnames(lavaan::lavInspect(fit, "vcov")),
                 colnames(lavaan::lavInspect(fit2, "vcov")))

expect_identical(lavaan::lavTech(fit, "fixed.x"),
                 lavaan::lavTech(fit2, "fixed.x"))

expect_identical(lavaan::lavTech(fit, "ntotal"),
                 lavaan::lavTech(fit2, "ntotal"))

expect_identical(lavaan::lavTech(fit, "norig"),
                 lavaan::lavTech(fit2, "norig"))

expect_identical(lavaan::lavTech(fit, "pattern"),
                 lavaan::lavTech(fit2, "pattern"))

expect_identical(lavaan::lavTech(fit, "options")$test,
                 lavaan::lavTech(fit2, "options")$test)

fit@internal$sam.struc.fit
fit@internal$sam.mm.rel
fit@t@internal$sam.mm.table
fit@internal$sam.method

})

test_that("SAM: Indirect effects", {

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
# summary(fit)

ind2 <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f2",
  fit = fit
)
ind3 <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit
)

expect_equal(coef(ind2),
             coef(fit, type = "user")["a2b2"],
             ignore_attr = TRUE)
expect_equal(coef(ind3),
             coef(fit, type = "user")["a3b3"],
             ignore_attr = TRUE)

paths <- all_indirect_paths(fit)

expect_true(length(paths) == 2)

ind_all <- many_indirect_effects(
              paths,
              fit = fit)

expect_equal(coef(ind_all),
             c(coef(ind2), coef(ind3)),
             ignore_attr = TRUE)

mc_out <- do_mc(
  fit = fit,
  R = 100,
  seed = 1234,
  parallel = FALSE,
  progress = !is_testing()
)

ind2_mc <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f2",
  fit = fit,
  mc_ci = TRUE,
  mc_out = mc_out
)

tmp1 <- vcov(fit)
set.seed(1234)
tmp2 <- MASS::mvrnorm(
          n = 100,
          mu = coef(fit)[colnames(tmp1)],
          Sigma = tmp1)
tmp3 <- apply(tmp2[, c("a2", "b2")], MARGIN = 1, prod)
chk <- boot_ci_internal(
  t0 = prod(coef(fit)[c("a2", "b2")]),
  t = cbind(tmp3)
)

expect_equal(confint(ind2_mc),
             chk,
             ignore_attr = TRUE)

})

test_that("SAM: boot_ci: se = 'bootstrap'", {

# Test when functions will SAM

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

fit <- sam(
  model = mod,
  data = data_sem,
  se = "bootstrap",
  bootstrap.args = list(R = 10),
  iseed = 1234
)

boot_out <- do_boot(
  fit,
  progress = !is_testing(),
  parallel = FALSE)

suppressWarnings(
ind2 <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f2",
  fit = fit,
  boot_ci = TRUE,
  boot_out = boot_out)
)
suppressWarnings(
ind3 <- indirect_effect(
  x = "f1",
  y = "f4",
  m = "f3",
  fit = fit,
  boot_ci = TRUE,
  boot_out = boot_out)
)

tmp <- lavInspect(fit, "boot")[1:10, ]

chk_a2b2 <- apply(
            tmp[, c("a2", "b2")],
            MARGIN = 1,
            FUN = prod
          )

expect_equal(ind2$boot_indirect,
             chk_a2b2,
             ignore_attr = TRUE)

chk_a3b3 <- apply(
            tmp[, c("a3", "b3")],
            MARGIN = 1,
            FUN = prod
          )

expect_equal(ind3$boot_indirect,
             chk_a3b3,
             ignore_attr = TRUE)

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
