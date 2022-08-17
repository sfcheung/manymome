#' @title Join 'lm()' Output to Form an 'lm_list`-Class Object
#'
#' @description The resulting model can be used by [indirect_effect()],
#'  [cond_indirect_effects()], or [cond_indirect()] as a
#'  path method, as if fitted by [lavaan::sem()].
#'
#' @details If a path model with mediation and/or moderation is fitted
#'  by a set of regression models using [lm()], this function can
#'  combine them to an object of the class `lm_list` that
#'  represents a path model, as one fitted by structural equation
#'  model functions such as [lavaan::sem()]. This class of object can
#'  be used by some functions, such as [indirect_effect()],
#'  [cond_indirect_effects()], and [cond_indirect()] as if they were
#'  the output of fitting a path model, with the regression
#'  coefficients treated as path coefficients.
#'
#'  The regression outputs to be combined need to meet
#'  the following requirements:
#'
#'  - All models must be connected to at least one another model.
#'    That is, a regression model must either have (a) at least
#'    on predictor that is the outcome variable of another model, or
#'    (b) its outcome variable the predictor of another model.
#'
#'  - All models must be fitted to the same sample. This implies that
#'    the sample size must be the same in all analysis.
#'
#' @return
#'  It returns an `lm_list`-class object that forms a path model represented
#'  by a set of regression models. This class has a `summary` method
#'  that shows the summary of each regression model stored
#'  (see [summary.lm_list()]), and
#'  a `print` method that prints the models stored (see [print.lm_list()]).
#'
#' @param ... The [lm()] outputs to be grouped in a list.
#'
#'
#' @seealso [summary.lm_list()] and [print.lm_list()] for related methods,
#'  [indirect_effect()] and [cond_indirect_effects()] which accept
#'  `lm_list`-class objects as input.
#'
#' @examples
#'
#'
#'
#' data(data_serial_parallel)
#' lm_m11 <- lm(m11 ~ x + c1 + c2, data_serial_parallel)
#' lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, data_serial_parallel)
#' lm_m2 <- lm(m2 ~ x + c1 + c2, data_serial_parallel)
#' lm_y <- lm(y ~ m11 + m12 + m2 + x + c1 + c2, data_serial_parallel)
#' # Join them to form a lm_list-class object
#' lm_serial_parallel <- lm2list(lm_m11, lm_m12, lm_m2, lm_y)
#' lm_serial_parallel
#' summary(lm_serial_parallel)
#'
#' # Compute indirect effect from x to y through m11 and m12
#' outm11m12 <- cond_indirect(x = "x", y = "y",
#'                            m = c("m11", "m12"),
#'                            fit = lm_serial_parallel)
#' outm11m12
#' # Compute indirect effect from x to y through m11 and m12 with bootstrapping CI
#' # R should be at least 2000 or even 5000 in read study.
#' outm11m12 <- cond_indirect(x = "x", y = "y",
#'                            m = c("m11", "m12"),
#'                            fit = lm_serial_parallel,
#'                            boot_ci = TRUE,
#'                            R = 100,
#'                            seed = 1234)
#' outm11m12
#'
#' @export
#'
#'

lm2list <- function(...) {
    check_lm_consistency(...)
    outputs <- list(...)
    if ((is.list(outputs)) && (length(outputs) == 1)) {
        outputs <- unlist(outputs, recursive = FALSE)
      }
    class(outputs) <- c("lm_list", class(outputs))
    outputs
  }

check_lm_consistency <- function(...) {
    outputs <- list(...)
    if ((is.list(outputs)) && (length(outputs) == 1)) {
        outputs <- unlist(outputs, recursive = FALSE)
      }
    mm_names <- lapply(outputs, get_mm_names)
    if (!check_except(mm_names)) {
        stop(paste0("At least one model has all variables not",
                    " in any other models. The lm models cannot",
                    " represent a typical path model with all",
                    " variables connected to at least one other",
                    " variable."))
      }
    mmi <- lapply(outputs, stats::model.matrix)
    ni <- sapply(mmi, nrow)
    if (length(unique(ni)) != 1) {
        stop(paste0("The data sets used in the lm models",
                    " do not have identical sample size.",
                    " All lm models must be fitted to the",
                    " same sample."))
      }
    if (!check_cases(mmi)) {
        stop(paste0("At least two models have common variables",
                    " that do not have identical data.",
                    " All lm models must be fitted to the",
                    " same sample."))
      }
    return(TRUE)
  }