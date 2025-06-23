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

#' @noRd
# Fit a list of lm-models by lavaan
mf_from_lm <- function(
                lm_formula,
                data,
                na.action = "na.pass"
              ) {
  rownames(data) <- seq_len(nrow(data))
  mf <- stats::model.frame(
        lm_formula,
        data = data,
        na.action = na.action
      )
  mm <- model.matrix(
          attr(mf, "terms"),
          mf
        )
  out <- mm[, -match("(Intercept)", colnames(mm))]
  out
}

lm_1 <- lm_forms_simple[1]
tmp <- mf_from_lm(
            lm_1,
            dat
          )

lm_mf <- lapply(lm_forms_simple,
                mf_from_lm,
                data = dat)

mm_from_lm_forms <- function(
                      lm_forms,
                      data,
                      na.action = "na.pass"
                    ) {
  all_y <- names(lm_forms)
  lm_all <- sapply(all_y,
                  function(xx) {NA},
                  simplify = FALSE)
  for (i in all_y) {
    lm_all[[i]] <- eval(bquote(lm(.(stats::as.formula(lm_forms[[i]])),
                                      data = data)))
    lm_all[[i]]$model <- stats::model.frame(
                            lm_all[[i]]$terms,
                            data = data,
                            na.action = "na.pass"
                          )
  }
  mm <- merge_model_matrix(lm_all)
  b_names <- sapply(lm_all,
                    function(x) names(x$coefficients[-1]),
                    USE.NAMES = TRUE,
                    simplify = FALSE)
  terms <- sapply(lm_all,
                    terms,
                    USE.NAMES = TRUE,
                    simplify = FALSE)
  coefficients <- sapply(lm_all,
                    function(x) summary(x)$coefficients,
                    USE.NAMES = TRUE,
                    simplify = FALSE)

  list(model_matrix = mm,
       b_names = b_names,
       terms = terms,
       coefficients = coefficients,
       lm_all = lm_all)
}

tmp <- mm_from_lm_forms(lm_forms_simple,
                        data = dat)
head(tmp$model_matrix)
nrow(tmp$model_matrix)
tmp$b_names
tmp$coefficients

tmp <- mm_from_lm_forms(lm_forms_serial,
                        data = dat)
head(tmp$model_matrix)
nrow(tmp$model_matrix)

tmp <- mm_from_lm_forms(lm_forms_parallel,
                        data = dat)
head(tmp$model_matrix)
nrow(tmp$model_matrix)

b_names_to_lavaan_model <- function(
                              x,
                              merge = TRUE) {
  tmpfct <- function(x_i, y_i) {
    paste0(y_i,
           " ~ ",
           paste0(x_i,
                  collapse = " + "))
  }
  out0 <- mapply(
              tmpfct,
              x_i = x,
              y_i = names(x),
              SIMPLIFY = FALSE
            )
  if (merge) {
    out0 <- paste0(out0,
                   collapse = "\n")
  }
  out0
}

tmp2 <- b_names_to_lavaan_model(tmp$b_names)
cat(tmp2)

fit <- lavaan::sem(model = tmp2,
                   data = tmp$model_matrix,
                   missing = "fiml.x")
parameterEstimates(fit,
                   rsquare = TRUE)

tmp$coefficients

lm_from_lavaan_list(fit)

})

