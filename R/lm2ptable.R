#' @title Convert a List of 'lm' Output
#' to a List of 'lavaan'-Like Objects
#'
#' @description It only converts
#' information necessary for
#' [indirect_effect()] and
#' [cond_indirect_effects()] to compute
#' indirect effects or conditional
#' indirect effects.
#'
#' @details Users usually do not need to
#' call this function to use
#' [indirect_effect()] and
#' [cond_indirect_effects()]. These two
#' functions will do the conversion
#' internally if necessary.
#'
#' @return A list with the following
#' arguments:
#'
#' - `est`: A data frame similar to the
#' parameter estimates table of the
#' output of [lavaan::sem()].
#'
#' - `dat`: A data frame with all the
#' data in the regression models merged.
#'
#' - `implied_stats`: A list with two
#' elements: `cov` is the implied
#' covariance matrix and `mean` is the
#' implied means. They are used to
#' compute standardized effects or form
#' levels of moderators.
#'
#' @param outputs A list of `lm`-class
#' objects.
#'
#'
#' @examples
#'
#' data(data_serial)
#' dat <- data_serial
#' lm_m1 <- lm(m1 ~ x + c1 + c2, dat)
#' lm_m2 <- lm(m2 ~ m1 + x + c1 + c2, dat)
#' lm_y <- lm(y ~ m2 + m1 + x + c1 + c2, dat)
#' out <- lm2ptable(list(lm_m1, lm_m2, lm_y))
#' out$est
#'
#'
#' @noRd
#'
#'

lm2ptable <- function(outputs,
                      compute_implied_stats = TRUE,
                      coefs_template = NULL) {
    if (!missing(outputs)) {
        outputs <- auto_lm2list(outputs)
      }
    mm <- merge_model_matrix(outputs)
    if (is.null(coefs_template)) {
      coefs <- lapply(outputs, coef2lor)
    } else {
      coefs <- mapply(coef2lor,
                      x = outputs,
                      coefs_template = coefs_template,
                      SIMPLIFY = FALSE,
                      USE.NAMES = FALSE)
    }
    out <- do.call(rbind, coefs)
    row.names(out) <- NULL
    if (compute_implied_stats) {
      i_s <- data2implied(mm)
    } else {
      i_s <- NULL
    }
    list(est = out,
         data = mm,
         implied_stats = i_s,
         vcov = lm_list_vcov(outputs,
                             est = out),
         df_residual = lm_df_residual(outputs))
  }