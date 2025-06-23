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
                            na.action = na.action
                          )
  }
  mm <- merge_model_matrix(lm_all)
  b_names <- sapply(lm_all,
                    function(x) names(x$coefficients),
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
  model_matrices <- sapply(
                  lm_all,
                  function(x) {
                    stats::model.matrix(
                        attr(x$model, "terms"),
                        x$model
                    )
                  },
                  USE.NAMES = TRUE,
                  simplify = FALSE
                )
  # TODO:
  # - Removed duplicated objects
  list(model_matrix = mm,
       model_matrices = model_matrices,
       b_names = b_names,
       terms = terms,
       coefficients = coefficients,
       lm_all = lm_all)
}

b_names_to_lavaan_model <- function(
                              x,
                              merge = TRUE) {
  tmpfct <- function(x_i, y_i) {
    x_i <- x_i[-which(x_i == "(Intercept)")]
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

lavaan_get_betas_etc <- function(dv, ivs_list, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "~"), , drop = FALSE]
    ivs <- ivs_list[[dv]]
    betas <- ptable_dv[which(ptable_dv$rhs %in% ivs), , drop = FALSE]
    rownames(betas) <- ivs
    betas
  }

lavaan_get_intercepts_etc <- function(dv, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "~1"), , drop = FALSE]
    if (nrow(ptable_dv) == 0) {
        out <- NULL
      } else {
        out <- ptable_dv
      }
    rownames(out) <- "(Intercept)"
    out
  }

lavaan_get_rsq <- function(dv, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "r2"), , drop = FALSE]
    if (nrow(ptable_dv) == 0) {
        out <- NULL
      } else {
        out <- ptable_dv[, "est", drop = TRUE]
      }
    names(out) <- dv
    out
  }

#' @noRd
# A simplified version of lm_from_lavaan_list_i()
# For q_* function
lm_from_lavaan_list_for_q <- function(
                                  fit,
                                  mm,
                                  ci_level = .95,
                                  group_number = NULL
                                ) {
  # Assume it has only one group
  ptable <- lavaan::parameterEstimates(fit,
                                       standardized = TRUE,
                                       level = .95,
                                       rsquare = TRUE)
  b_names <- mm$b_names
  # Get all dvs (ov.nox, lv.ox)
  dvs <- names(b_names)
  # Get all ivs
  ivs_list <- sapply(b_names,
                     function(x) {
                      x[-which(x == "(Intercept)")]
                     },
                     USE.NAMES = TRUE,
                     simplify = FALSE)
  # Get estimates and other statistics
  bs_list <- sapply(dvs, lavaan_get_betas_etc,
                    ivs_list = ivs_list,
                    ptable = ptable,
                    simplify = FALSE)
  # Get all intercepts
  int_list <- sapply(dvs, lavaan_get_intercepts_etc,
                      ptable = ptable,
                      simplify = FALSE)
  # Get all R-squares
  rsq_list <- sapply(dvs, lavaan_get_rsq,
                      ptable = ptable,
                      simplify = FALSE)
  # Combine them
  coefs_list <- mapply(function(x, y) {rbind(x, y)},
                      x = int_list,
                      y = bs_list,
                      SIMPLIFY = FALSE)

  # TODO:
  # - Add CIs
  # - Add betas

  # lm_version
  coefs_lm_list <- mapply(lm_coef_from_lavaan_i,
                          lav_coefs = coefs_list,
                          lm_coefs = mm$coefficients,
                          USE.NAMES = TRUE,
                          SIMPLIFY = FALSE)

  # Get all product terms
  # NOTE: No need because they are formed from lm()
  # Generate lm_like_object
  mods <- mapply(to_formula, dv = dvs, ivs = ivs_list)
  # Return an lm_list object
  out <- mapply(function(dv, model, ivs, coefs, coefs_lm, rsq) {
                  list(
                        dv = dv,
                        model = model,
                        ivs = ivs,
                        coefs = coefs,
                        coefs_lm = coefs_lm,
                        rsquare = rsq
                      )
                },
                dv = dvs,
                model = mods,
                ivs = ivs_list,
                coefs = coefs_list,
                rsq = rsq_list,
                coefs_lm = coefs_lm_list,
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE)
  out
}

lm_coef_from_lavaan_i <- function(
                        lav_coefs,
                        lm_coefs
                      ) {
  out <- lm_coefs
  i <- match(rownames(lav_coefs), rownames(out))
  out[i, "Estimate"] <- lav_coefs$est
  out[i, "Std. Error"] <- lav_coefs$se
  out[i, "t value"] <- lav_coefs$z
  out[i, "Pr(>|t|)"] <- lav_coefs$pvalue
  colnames(out)[colnames(out) == "t value"] <- "z value"
  colnames(out)[colnames(out) == "Pr(>|t|)"] <- "Pr(>|z|)"
  out
}

lm_coef_from_lavaan <- function(
                        coef_template,
                        lm_from_lavaan
                      ) {
  coef_lav <- sapply(lm_from_lavaan,
                     function(x) x$coefs,
                     USE.NAMES = TRUE,
                     simplify = FALSE)
  out <- mapply(lm_coef_from_lavaan_i,
                lav_coefs = coef_lav,
                lm_coefs = coef_template,
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE)
  out
}

mm <- mm_from_lm_forms(
            lm_forms_simple,
            data = dat
          )
mod <- b_names_to_lavaan_model(mm$b_names)
fit <- lavaan::sem(
          model = mod,
          data = mm$model_matrix,
          missing = "fiml.x",
          meanstructure = TRUE
        )
lm_from_lavaan <- lm_from_lavaan_list_for_q(
                    fit = fit,
                    mm = mm
                  )
lm_from_lavaan[[1]]

# Select std.nox / std.all

attr(mm$model_matrices[[1]], "assign")
attr(mm$terms[[1]], "dataClasses")

attributes(mm$model_matrices[[1]])
attributes(mm$terms[[1]])

numeric_ivs <- function(mm, y) {
  # TODO:
  # - Need to handle special terms, such as product terms
  data_classes <- attr(mm$terms[[y]], "dataClasses")
  term_labels <- attr(mm$terms[[y]], "term.labels")
  mm_assign <- attr(mm$model_matrices[[y]], "assign")
  b_names <- mm$b_names[[y]]
  b_types <- b_names
  names(b_types) <- b_names
  j <- seq_along(term_labels)
  for (xx in seq_along(b_types)) {
    if (mm_assign[xx] %in% j) {
      b_types[xx] <- data_classes[term_labels[mm_assign[xx]]]
    }
  }
}

###

mf <- stats::model.frame(
      lm_forms_simple[1],
      data = dat,
      na.action = "na.pass"
    )
mm <- model.matrix(
        attr(mf, "terms"),
        mf
      )

attributes(mm)

})

