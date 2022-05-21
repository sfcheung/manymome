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
#' @param outputs A list of `lm` class objects.
#' @param R The number of bootstrap samples. Default is 100.
#' @param seed The seed for the bootstrapping.
#'             Default is `NULL` and seed is not set.
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

lm2boot_out <- function(outputs, R = 100, seed = NULL) {
    out_type <- cond_indirect_check_fit(outputs)
    if (out_type != "lm") {
        stop("'outputs' must be a list of 'lm()' outputs.")
      }
    dat <- merge_model_frame(outputs)
    n <- nrow(dat)
    if (!is.null(seed)) set.seed(seed)
    out0 <- replicate(R, lm_boot2est_i(d = dat,
                                       i = sample.int(n, replace = TRUE),
                                       outputs = outputs), simplify = FALSE)
    class(out0) <- "boot_out"
    out0
  }

lm_boot2est_i <- function(d, i = NULL, outputs) {
    if (!is.null(i)) {
        d_i <- d[i, ]
      } else {
        d_i <- d
      }
    out_i <- lapply(outputs, stats::update, data = d_i)
    lm2ptable(out_i)
  }