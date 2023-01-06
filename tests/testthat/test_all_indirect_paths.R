skip_on_cran()
# This test is long and should be tested locally.
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_med_complicated
mod <-
"
m11 ~ x1 + x1 + x2 + c1 + c2
m12 ~ m11 + x1 + x2 + c1 + c2
m2 ~ x1 + x2 + c1 + c2
y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2
y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

lm_m11 <- lm(m11 ~ x1 + x1 + x2 + c1 + c2, dat)
lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, dat)
lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, dat)
lm_y1 <- lm(y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat)
lm_y2 <- lm(y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat)

fit_lm <- lm2list(lm_m11, lm_m12, lm_m2, lm_y1, lm_y2)

outa1 <- all_indirect_paths(fit)
outa2 <- all_indirect_paths(fit, exclude = c("c1", "c3", "c2"))
outa3 <- all_indirect_paths(fit, x = c("x1", "m2", "m11"))
outa4 <- all_indirect_paths(fit, y = c("y1", "m12", "m2"),
                           exclude = c("c1", "c2"))

outb1 <- all_indirect_paths(fit_lm)
outb2 <- all_indirect_paths(fit_lm, exclude = c("c1", "c3", "c2"))
outb3 <- all_indirect_paths(fit_lm, x = c("x1", "m2", "m11"))
outb4 <- all_indirect_paths(fit_lm, y = c("y1", "m12", "m2"),
                           exclude = c("c1", "c2"))

# tmp <- list(x = lapply(outa2, function(x) {x$x}),
#             y = lapply(outa2, function(x) {x$y}),
#             m = lapply(outa2, function(x) {x$m}))
# ind <- mapply(indirect_effect,
#               x = tmp$x,
#               y = tmp$y,
#               m = tmp$m,
#               MoreArgs = list(fit = fit),
#               USE.NAMES = FALSE,
#               SIMPLIFY = FALSE)
# sapply(ind, coef)

test_that("Check identified paths: Length", {
    expect_equal(
        length(outa1),
        38
      )
    expect_equal(
        length(outa2),
        20
      )
    expect_equal(
        length(outa3),
        11
      )
    expect_equal(
        length(outa4),
        11
      )
    expect_equal(
        length(outb1),
        38
      )
    expect_equal(
        length(outb2),
        20
      )
    expect_equal(
        length(outb3),
        11
      )
    expect_equal(
        length(outb4),
        11
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
    expect_true(all(sapply(outb1, tmpfct, fit = fit)))
    expect_true(all(sapply(outb2, tmpfct, fit = fit)))
    expect_true(all(sapply(outb3, tmpfct, fit = fit)))
    expect_true(all(sapply(outb4, tmpfct, fit = fit)))
  })


test_that("Check whether paths are valid", {
    expect_true(all(sapply(outa1, tmpfct, fit = fit)))
    expect_true(all(sapply(outa2, tmpfct, fit = fit)))
    expect_true(all(sapply(outa3, tmpfct, fit = fit)))
    expect_true(all(sapply(outa4, tmpfct, fit = fit)))
    expect_true(all(sapply(outb1, tmpfct, fit = fit)))
    expect_true(all(sapply(outb2, tmpfct, fit = fit)))
    expect_true(all(sapply(outb3, tmpfct, fit = fit)))
    expect_true(all(sapply(outb4, tmpfct, fit = fit)))
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
    expect_false(any(get_all(outa2) %in% c("c1", "c3", "c2")))
    expect_false(any(get_all(outa4) %in% c("c1", "c3", "c2")))
    expect_false(any(get_all(outb2) %in% c("c1", "c3", "c2")))
    expect_false(any(get_all(outb4) %in% c("c1", "c3", "c2")))
  })

test_that("Check x", {
    expect_true(all(get_x(outa3) %in% c("x1", "m2", "m11")))
    expect_true(all(get_x(outb3) %in% c("x1", "m2", "m11")))
  })

test_that("Check y", {
    expect_true(all(get_y(outa4) %in% c("y1", "m12", "m2")))
    expect_true(all(get_y(outb4) %in% c("y1", "m12", "m2")))
  })

# Test many_indirect_effects.

# TODO: Add tests

fit_boot_out <- do_boot(fit, R = 100, seed = 98743, ncores = 2)
out_tmp <- outa4[c(1, 3, 5, 6)]

ind_1_no_ci <- many_indirect_effects(out_tmp,
                               fit = fit)
ind_1_no_ci

ind_1_no_ci_stdx <- many_indirect_effects(out_tmp,
                               fit = fit,
                               standardized_x = TRUE)
ind_1_no_ci_stdx

ind_1_no_ci_stdy <- many_indirect_effects(out_tmp,
                               fit = fit,
                               standardized_y = TRUE)
ind_1_no_ci_stdy

ind_1_no_ci_std <- many_indirect_effects(out_tmp,
                               fit = fit,
                               standardized_x = TRUE,
                               standardized_y = TRUE)
ind_1_no_ci_std

ind_1 <- many_indirect_effects(out_tmp,
                               fit = fit,
                               boot_ci = TRUE,
                               boot_out = fit_boot_out)
ind_1

ind_1_stdx <- many_indirect_effects(out_tmp,
                               fit = fit,
                               standardized_x = TRUE,
                               boot_ci = TRUE,
                               boot_out = fit_boot_out)
ind_1_stdx

ind_1_stdy <- many_indirect_effects(out_tmp,
                               fit = fit,
                               standardized_y = TRUE,
                               boot_ci = TRUE,
                               boot_out = fit_boot_out)
ind_1_stdy

ind_1_std <- many_indirect_effects(out_tmp,
                               fit = fit,
                               standardized_x = TRUE,
                               standardized_y = TRUE,
                               boot_ci = TRUE,
                               boot_out = fit_boot_out)
ind_1_std

coef(ind_1_no_ci)
coef(ind_1_no_ci_stdx)
coef(ind_1_no_ci_stdy)
coef(ind_1_no_ci_std)

coef(ind_1)
coef(ind_1_stdx)
coef(ind_1_stdy)
coef(ind_1_std)

test_that("Check confint warning", {
    expect_warning(confint(ind_1_no_ci))
    expect_warning(confint(ind_1_no_ci_stdx))
    expect_warning(confint(ind_1_no_ci_stdy))
    expect_warning(confint(ind_1_no_ci_std))
  })


confint(ind_1)
confint(ind_1_stdx)
confint(ind_1_stdy)
confint(ind_1_std)

# Test total_indirect_effect

tot1_no_ci <- total_indirect_effect(ind_1_no_ci, x = "x1", y = "y1")
tot1_no_ci_chk <- ind_1_no_ci[[2]] + ind_1_no_ci[[3]] + ind_1_no_ci[[4]]

tot1_stdx <- total_indirect_effect(ind_1_stdx, x = "x1", y = "y1")
tot1_stdx_chk <- ind_1_stdx[[2]] + ind_1_stdx[[3]] + ind_1_stdx[[4]]

tot1_no_ci_m11 <- total_indirect_effect(ind_1_no_ci, x = "m11", y = "y1")

test_that("Check total indirect effect", {
    expect_identical(tot1_no_ci, tot1_no_ci_chk)
    expect_identical(tot1_stdx, tot1_stdx_chk)
    expect_identical(tot1_no_ci_m11, ind_1_no_ci[[1]])
    expect_error(total_indirect_effect(ind_1_stdx, x = "x2", y = "y1"))
  })
