skip("WIP")

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
dat[3:5, "m"] <- NA
dat$w2 <- ifelse(dat$w2 == "team2",
                 "*team 2",
                 "$team 1$")
head(dat)

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

})

