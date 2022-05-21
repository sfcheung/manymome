#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param ... The [lm()] outputs to be grouped in a list.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + c4 * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' check_path(x = "x", y = "m3", m = c("m1", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m1", "m2"), fit = fit)
#'
#' @export
#'
#'

lm2list <- function(...) {
    check_lm_consistency(...)
    outputs <- list(...)
    class(outputs) <- c("lm_list", class(outputs))
    outputs
  }

check_lm_consistency <- function(...) {
    outputs <- list(...)
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