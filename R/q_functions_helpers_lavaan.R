#' @noRd
# Fit a list of lm-models by lavaan
# NOT USED?
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


#' @noRd
fix_names_for_lavaan <- function(data) {
  for (i in seq_len(ncol(data))) {
    data[, i] <- fix_names_for_lavaan_i(data[, i, drop = TRUE])
  }
  data
}
fix_names_for_lavaan_i <- function(x) {
  if (is.character(x)) {
    x1 <- make.names(x)
    x1[is.na(x)] <- NA
    return(x1)
  } else if (is.factor(x)) {
    x1 <- x
    levels(x1) <- make.names(levels(x))
    return(x1)
  }
  x
}

#' @noRd
# Input:
# - lm_forms: A list of model formulas
# - data: The dataset
# - na.action: For stats::model.frame()
# Output:
# A list:
# - model_matrix: The model matrix for
#     all models. Data only.
# - model_matrices: A list of model
#     matrices, one for each model, with
#     additional information such as
#     terms and contrasts.
# - b_names: Names of the coefficients.
# - terms: A list of the terms objects
#     for the models.
# - coefficients: A list of the
#     `coefficients` objects in
#     summary(). To be used as a
#     template.
# - lm_all: A list of the lm() outputs.
mm_from_lm_forms <- function(
                      lm_forms,
                      data,
                      na.action = "na.pass"
                    ) {
  data <- fix_names_for_lavaan(data)
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
  # merge_model_matrix assumes cases can be matched row-by-row.
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

#' @noRd
# Input:
# - b_names
# Output:
# - A lavaan model syntax
b_names_to_lavaan_model <- function(
                              x,
                              merge = TRUE,
                              null_y = character(0)) {
  tmpfct <- function(x_i, y_i, null_y = y_i) {
    x_i <- x_i[-which(x_i == "(Intercept)")]
    if (y_i %in% null_y) {
      tmp <- "0*"
    } else {
      tmp <- character(0)
    }
    paste0(y_i,
           " ~ ",
           paste0(tmp,
                  x_i,
                  collapse = " + "))
  }
  out0 <- mapply(
              tmpfct,
              x_i = x,
              y_i = names(x),
              MoreArgs = list(null_y = null_y),
              SIMPLIFY = FALSE
            )
  if (merge) {
    out0 <- paste0(out0,
                   collapse = "\n")
  }
  out0
}

#' @noRd
# Input:
# - dv: Name of dv
# - ivs_list: Names of predictors
# - ptable: lavaan::parameterEstimates() output
# Output:
# - A data frame of all columns for the coefficients
lavaan_get_betas_etc <- function(dv, ivs_list, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "~"), , drop = FALSE]
    ivs <- ivs_list[[dv]]
    betas <- ptable_dv[which(ptable_dv$rhs %in% ivs), , drop = FALSE]
    rownames(betas) <- ivs
    betas
  }

#' @noRd
# Input:
# - dv: Name of dv
# - ptable: lavaan::parameterEstimates() output
# Output:
# - A data frame of all columns for the intercept
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

#' @noRd
# Input:
# - dv: Name of dv
# - ptable: lavaan::parameterEstimates() output
# Output:
# - Scalar. The R-square
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
# Input:
# - fit: lavaan output
# - mm: mm_from_lm_forms() output
# - ci_level: The level of significance for the CIs.
# - group_number: Not used. Kept for future extension.
# Output:
# - A list for each dv:
#   - dv: Name of the DV
#   - model: The model formula
#   - ivs: The terms
#   - coefs: The original lavaan coefficient table
#   - coefs_lm: A coefficient table in lm format
#   - rsquare: R-square
lm_from_lavaan_list_for_q <- function(
                                  fit,
                                  mm,
                                  ci_level = .95,
                                  group_number = NULL
                                ) {
  # Assume it has only one group
  ptable <- lavaan::parameterEstimates(fit,
                                       standardized = TRUE,
                                       level = ci_level,
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
  # Null models
  fit_null <- fit_null(
                mm = mm,
                fit = fit
              )
  rsq_test <- rsquare_test(fit = fit,
                           fit_null = fit_null)
  # Combine them
  coefs_list <- mapply(function(x, y) {rbind(x, y)},
                      x = int_list,
                      y = bs_list,
                      SIMPLIFY = FALSE)

  # lm_version
  term_types <- sapply(dvs,
                       numeric_ivs,
                       mm = mm,
                       simplify = FALSE,
                       USE.NAMES = TRUE)

  coefs_lm_list <- mapply(lm_coef_from_lavaan_i,
                          lav_coefs = coefs_list,
                          lm_coefs = mm$coefficients,
                          term_types = term_types,
                          USE.NAMES = TRUE,
                          SIMPLIFY = FALSE)

  # Get all product terms
  # NOTE: No need because they are formed from lm()
  # Generate lm_like_object
  mods <- mapply(to_formula, dv = dvs, ivs = ivs_list)
  # Return an lm_list object
  out <- mapply(function(dv,
                         model,
                         ivs,
                         coefs,
                         coefs_lm,
                         rsq,
                         rsq_test,
                         fit_null_lrt,
                         fit_null) {
                  list(
                        dv = dv,
                        model = model,
                        ivs = ivs,
                        coefs = coefs,
                        coefs_lm = coefs_lm,
                        rsquare = rsq,
                        rsq_test = rsq_test,
                        fit_null_lrt = fit_null_lrt,
                        fit_null = fit_null
                      )
                },
                dv = dvs,
                model = mods,
                ivs = ivs_list,
                coefs = coefs_list,
                rsq = rsq_list,
                rsq_test = rsq_test$pvalues,
                fit_null_lrt = rsq_test$lrt_out,
                fit_null = fit_null,
                coefs_lm = coefs_lm_list,
                SIMPLIFY = FALSE,
                USE.NAMES = TRUE)
  out
}

#' @noRd
# Create a lm-style coefficient table
# with lavaan results
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
#' @noRd
lm_coef_from_lavaan_i <- function(
                        lav_coefs,
                        lm_coefs,
                        term_types
                      ) {
  out <- lm_coefs
  i <- match(rownames(lav_coefs), rownames(out))
  out[i, "Estimate"] <- lav_coefs$est
  out[i, "Std. Error"] <- lav_coefs$se
  out[i, "t value"] <- lav_coefs$z
  out[i, "Pr(>|t|)"] <- lav_coefs$pvalue
  colnames(out)[colnames(out) == "t value"] <- "z value"
  colnames(out)[colnames(out) == "Pr(>|t|)"] <- "Pr(>|z|)"
  ci.lower <- lav_coefs$ci.lower
  ci.upper <- lav_coefs$ci.upper
  betas <- lav_coefs$std.all
  tmp <- which(term_types != "numeric")
  if (length(tmp) > 0) {
    betas[tmp] <- lav_coefs$std.nox[tmp]
  }
  betas[which(rownames(out) == "(Intercept)")] <- NA
  j1 <- which(colnames(out) == "Estimate")
  j2 <- seq(j1 + 1, ncol(out))
  out1 <- cbind(out[, j1, drop = FALSE],
                CI.lo = ci.lower,
                CI.hi = ci.upper,
                betaS = betas,
                out[, j2, drop = FALSE])
  rownames(out1) <- rownames(out)
  out1
}

#' @noRd
# Find the type of each term
# Input:
# - mm: The output of mm_from_lm_forms()
# - y: The name of the dv
# Output:
# - A character vector of the type of each term
numeric_ivs <- function(mm, y) {
  # TODO:
  # - Need to handle special terms, such as product terms
  #   But not urgent as we do not yet support moderation.
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
  b_types
}

#' @noRd
# Fit a null model for each dv
# Input:
# - mm: Output of mm_from_lm_forms
# - ift: lavaan output
# Output:
# - A list of lavaan output
fit_null <- function(
                mm,
                fit
              ) {
  dvs <- names(mm$model_matrices)
  mod_null <- sapply(
                  dvs,
                  function(y) {
                    b_names_to_lavaan_model(
                      x = mm$b_names,
                      null_y = y)
                  },
                  simplify = FALSE,
                  USE.NAMES = TRUE)
  fit_data <- fit@Data
  fit_model <- fit@Model
  fit_sampstats <- fit@SampleStats
  fit_opts <- fit@Options
  fit_pt <- fit@ParTable

  out0 <- sapply(mod_null,
                 function(x) {
                    out <- lavaan::lavaan(
                          model = x,
                          slotData = fit_data,
                          slotSampleStats = fit_sampstats,
                          slotOptions = fit_opts
                        )
                 },
                 simplify = FALSE,
                 USE.NAMES = TRUE)
  out0
}

#' @noRd
# Test R-squares
# Input:
# - The original model
# - A named list of null models
# Output:
# - A named list of lavTestLRT() output
rsquare_test <- function(
                  fit,
                  fit_null,
                  ...
                ) {
  dvs <- names(fit_null)
  tmpfct <- function(fit0, fit1, ...) {
    outi <- lavaan::lavTestLRT(
                  fit0,
                  fit1,
                  ...
                )
    outi
  }
  out0 <- sapply(fit_null,
                 tmpfct,
                 fit1 = fit,
                 simplify = FALSE,
                 USE.NAMES = TRUE)
  pvalues <- sapply(out0,
                    function(x) {
                      x["fit0", "Pr(>Chisq)"]
                    },
                    simplify = TRUE,
                    USE.NAMES = TRUE)
  out0 <- list(lrt_out = out0,
               pvalues = pvalues)
}
