library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_serial_parallel_latent
mod <-
"
fx1 =~ x1 + x2 + x3
fx2 =~ x4 + x5 + x6
fm11 =~ m11a + m11b + m11c
fm12 =~ m12a + m12b + m12c
fm2  =~ m2a + m2b + m2c
fy1 =~ y1 + y2 + y3
fy2 =~ y3 + y4 + y5
fm11 ~ a1 * fx1
fm12 ~ b11 * fm11 + a2m * fx2
fm2 ~ a2 * fx2
fy1 ~ b12 * fm12 + b11y1 * fm11 + cp1 * fx1
fy2 ~ b2 * fm2 + cp2 * fx2
a1b11b12 := a1 * b11 * b12
a1b11y1 := a1 * b11y1
a2b2 := a2 * b2
a2mb12 := a2m * b12
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

outa1 <- all_indirect_paths(fit)
outa2 <- all_indirect_paths(fit, exclude = c("fx2"))
outa3 <- all_indirect_paths(fit, x = c("fx2"), y = c("fy2"))
outa4 <- all_indirect_paths(fit, y = c("fy1", "fm12", "fm2"),
                           exclude = c("fx1"))
outa5 <- all_indirect_paths(fit, x = c("fx1"), y = c("fy2"))

test_that("Check identified paths: Length", {
    expect_equal(
        length(outa1),
        6
      )
    expect_equal(
        length(outa2),
        4
      )
    expect_equal(
        length(outa3),
        1
      )
    expect_equal(
        length(outa4),
        2
      )
    expect_equal(
        length(outa5),
        0
      )
  })

tmpfct <- function(object, fit) {
    check_path(x = object$x,
               y = object$y,
               m = object$m,
               fit = fit)
  }

test_that("Check whether paths are valid", {
    expect_true(all(sapply(outa1, tmpfct, fit = fit)))
    expect_true(all(sapply(outa2, tmpfct, fit = fit)))
    expect_true(all(sapply(outa3, tmpfct, fit = fit)))
    expect_true(all(sapply(outa4, tmpfct, fit = fit)))
    expect_true(all(sapply(outa5, tmpfct, fit = fit)))
  })

get_x <- function(object) {
    unique(sapply(object, function(x) x$x))
  }
get_y <- function(object) {
    unique(sapply(object, function(x) x$y))
  }
get_all <- function(object) {
    unique(unlist(object))
  }

test_that("Check exclude", {
    expect_false(any(get_all(outa2) %in% c("fx2")))
    expect_false(any(get_all(outa4) %in% c("fx1")))
  })

test_that("Check x", {
    expect_true(all(get_x(outa3) %in% c("fx2")))
  })

test_that("Check y", {
    expect_true(all(get_y(outa3) %in% c("fy2")))
    expect_true(all(get_y(outa4) %in% c("fy1", "fm12", "fm2")))
  })
