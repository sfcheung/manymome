library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_med_complicated

# \describe{
#   \item{x1}{Predictor 1. Numeric.}
#   \item{x2}{Predictor 2. Numeric.}
#   \item{m11}{Mediator 1 in Path 1. Numeric.}
#   \item{m12}{Mediator 2 in Path 1. Numeric.}
#   \item{m2}{Mediator in Path 2. Numeric.}
#   \item{y1}{Outcome variable 1. Numeric.}
#   \item{y2}{Outcome variable 2. Numeric.}
#   \item{c1}{Control variable. Numeric.}
#   \item{c2}{Control variable. Numeric.}
# }

mod <-
"
m11 ~ x1 + x2
y1 ~ m11
"

fit <- sem(mod, dat)

test_that("delta_model_check_fit-1", {
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_single_x = TRUE,
                                    skip_check_m_between_x_y = TRUE,
                                    skip_check_x_to_y = TRUE,
                                    skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_single_x = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_m_between_x_y = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_x_to_y = TRUE))
  })

mod <-
"
m11 ~ x1 + x2
y1 ~ m11 + x1
"

fit <- sem(mod, dat)

test_that("delta_model_check_fit-2", {
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_single_x = TRUE,
                                    skip_check_m_between_x_y = TRUE,
                                    skip_check_x_to_y = TRUE,
                                    skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_latent_variables = TRUE))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_single_x = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_m_between_x_y = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                     skip_check_x_to_y = TRUE))
  })

mod <-
"
m11 ~ x1
m12 ~ x1
y1 ~ m11 + m12 + x1
y2 ~ m11 + m12 + x1
"

fit <- sem(mod, dat)

test_that("delta_model_check_fit-3", {
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_single_x = TRUE,
                                    skip_check_m_between_x_y = TRUE,
                                    skip_check_x_to_y = TRUE,
                                    skip_check_latent_variables = TRUE))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_latent_variables = TRUE))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_single_x = TRUE))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_m_between_x_y = TRUE))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = "m11", fit = fit,
                                    skip_check_x_to_y = TRUE))
  })

mod <-
"
m11 ~ x1
m12 ~ x1
y1 ~ m11
y2 ~ m12
"

fit <- sem(mod, dat)

test_that("delta_model_check_fit-4", {
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit))
    expect_true(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit,
                                    skip_check_single_x = TRUE,
                                    skip_check_m_between_x_y = TRUE,
                                    skip_check_x_to_y = TRUE,
                                    skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit,
                                    skip_check_single_x = TRUE,
                                    skip_check_m_between_x_y = FALSE,
                                    skip_check_x_to_y = TRUE,
                                    skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit,
                                     skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit,
                                     skip_check_single_x = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit,
                                     skip_check_m_between_x_y = TRUE))
    expect_error(delta_med_check_fit(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit,
                                     skip_check_x_to_y = TRUE))
  })

dat2 <- data_sem

mod <-
"
x02 ~ x01
f3 ~ x02
f3 =~ x08 + x09 + x10
"

fit <- sem(mod, dat2)

test_that("delta_model_check_fit-5", {
    expect_error(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit))
    expect_true(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit,
                                    skip_check_single_x = TRUE,
                                    skip_check_m_between_x_y = TRUE,
                                    skip_check_x_to_y = TRUE,
                                    skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit,
                                     skip_check_single_x = TRUE,
                                     skip_check_m_between_x_y = TRUE,
                                     skip_check_x_to_y = TRUE,
                                     skip_check_latent_variables = FALSE))
    expect_error(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit,
                                     skip_check_latent_variables = TRUE))
    expect_error(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit,
                                     skip_check_single_x = TRUE))
    expect_error(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit,
                                     skip_check_m_between_x_y = TRUE))
    expect_error(delta_med_check_fit(x = "x01", y = "f3", m = c("x02"), fit = fit,
                                     skip_check_x_to_y = TRUE))
  })
