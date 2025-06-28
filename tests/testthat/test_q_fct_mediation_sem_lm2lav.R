skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem", {

dat <- data_med_mod_serial_cat
dat[1:2, "x"] <- NA
dat[2:3, "w1"] <- NA
dat[1:4, "y"] <- NA
dat[3:5, "m1"] <- NA
dat$w2 <- ifelse(dat$w2 == "team2",
                 "*team 2",
                 "$team 1$")
head(dat)

dat_no_na <- stats::na.omit(dat)
head(dat_no_na)

lm_forms_simple0 <- form_models_simple(
                      x = "x",
                      y = "y",
                      m = "m1",
                      cov = NULL)
lm_forms_serial0 <- form_models_serial(
                      x = "x",
                      y = "y",
                      m = c("m1", "m2"),
                      cov = NULL)
lm_forms_parallel0 <- form_models_parallel(
                      x = "x",
                      y = "y",
                      m = c("m1", "m2"),
                      cov = NULL)
lm_forms_simple <- form_models_simple(
                      x = "x",
                      y = "y",
                      m = "m1",
                      cov = c("w1", "w2", "c1"))
lm_forms_serial <- form_models_serial(
                      x = "x",
                      y = "y",
                      m = c("m1", "m2"),
                      cov = c("w1", "w2", "c1"))
lm_forms_parallel <- form_models_parallel(
                      x = "x",
                      y = "y",
                      m = c("m1", "m2"),
                      cov = c("w1", "w2", "c1"))

# Test

mm_simple0 <- mm_from_lm_forms(
            lm_forms_simple0,
            data = dat_no_na
          )
mm_serial0 <- mm_from_lm_forms(
            lm_forms_serial0,
            data = dat_no_na
          )
mm_parallel0 <- mm_from_lm_forms(
            lm_forms_parallel0,
            data = dat_no_na
          )

mm_simple <- mm_from_lm_forms(
            lm_forms_simple,
            data = dat
          )
mm_serial <- mm_from_lm_forms(
            lm_forms_serial,
            data = dat
          )
mm_parallel <- mm_from_lm_forms(
            lm_forms_parallel,
            data = dat
          )

fit_simple0 <- lavaan::sem(
          model = b_names_to_lavaan_model(mm_simple0$b_names),
          data = mm_simple0$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )
fit_serial0 <- lavaan::sem(
          model = b_names_to_lavaan_model(mm_serial0$b_names),
          data = mm_serial0$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )
fit_parallel0 <- lavaan::sem(
          model = b_names_to_lavaan_model(mm_parallel0$b_names),
          data = mm_parallel0$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )

fit_simple <- lavaan::sem(
          model = b_names_to_lavaan_model(mm_simple$b_names),
          data = mm_simple$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )
fit_serial <- lavaan::sem(
          model = b_names_to_lavaan_model(mm_serial$b_names),
          data = mm_serial$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )
fit_parallel <- lavaan::sem(
          model = b_names_to_lavaan_model(mm_parallel$b_names),
          data = mm_parallel$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )

lm_simple0 <- lm_from_lavaan_list_for_q(
                    fit = fit_simple0,
                    mm = mm_simple0
                  )
lm_serial0 <- lm_from_lavaan_list_for_q(
                    fit = fit_serial0,
                    mm = mm_serial0
                  )
lm_parallel0 <- lm_from_lavaan_list_for_q(
                    fit = fit_parallel0,
                    mm = mm_parallel0
                  )

lm_simple <- lm_from_lavaan_list_for_q(
                    fit = fit_simple,
                    mm = mm_simple
                  )
lm_serial <- lm_from_lavaan_list_for_q(
                    fit = fit_serial,
                    mm = mm_serial
                  )
lm_parallel <- lm_from_lavaan_list_for_q(
                    fit = fit_parallel,
                    mm = mm_parallel
                  )

lm0_m1 <- lm(m1 ~ x, dat_no_na)
lm0_m2 <- lm(m2 ~ x, dat_no_na)
lm0_y_m1 <-  lm(y ~ m1 + x, dat_no_na)
lm0_y_m1_m2 <-  lm(y ~ m1 + m2 + x, dat_no_na)

expect_equal(coef(lm0_m1),
             lm_simple0$m1$coefs_lm[, "Estimate"],
             ignore_attr = TRUE)
expect_equal(coef(lm0_y_m1),
             lm_simple0$y$coefs_lm[, "Estimate"],
             ignore_attr = TRUE)
expect_equal(coef(lm0_y_m1_m2),
             lm_serial0$y$coefs_lm[, "Estimate"],
             ignore_attr = TRUE)
expect_equal(coef(lm0_y_m1_m2),
             lm_parallel0$y$coefs_lm[, "Estimate"],
             ignore_attr = TRUE)
expect_equal(coef(lm0_m2),
             lm_parallel0$m2$coefs_lm[, "Estimate"],
             ignore_attr = TRUE)

expect_equal(lm_simple$m1$coefs_lm[, "Estimate"],
             lm_parallel$m1$coefs_lm[, "Estimate"],
             tolerance = 1e-2)
expect_equal(lm_serial$y$coefs_lm[, "Estimate"],
             lm_parallel$y$coefs_lm[, "Estimate"],
             tolerance = 1e-2)

})

