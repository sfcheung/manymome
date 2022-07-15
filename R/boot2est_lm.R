#' @title Bootstrapping Estimates for `lm` Outputs
#'
#' @description Generate bootstrapping estimates for models
#'  in a list of 'lm' outputs.
#'
#' @details
#' It do nonparametric bootstrapping to generate bootstrap
#' estimates of the regression coefficients in the regression models
#' of a list of [lm()] outputs, or a `lm_list`-class object
#' formed by [lm2list()]. The stored estimates can be
#' used to [cond_indirect()] and [cond_indirect_effects()]
#' in forming bootstrapping confidence intervals for
#' conditional effects.
#'
#' This approach removes the need to repeat bootstrapping in
#' each call to [cond_indirect()] and [cond_indirect_effects()].
#' It also ensures that the same set of bootstrap samples
#' is used in all subsequent analysis.
#'
#' @return
#' A `boot_out`-class object that can be used for the `boot_out`
#' argument of [cond_indirect()] and [cond_indirect_effects()]
#' for forming bootstrapping confidence intervals.
#'
#' @param outputs A list of `lm` class objects, or
#'  the output of [lm2list()] (i.e., a `lm_list`-class
#'  object).
#' @param R The number of bootstrap samples. Default is 100.
#' @param seed The seed for the bootstrapping.
#'             Default is `NULL` and seed is not set.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' data(data_med_mod_ab1)
#' dat <- data_med_mod_ab1
#' lm_m <- lm(m ~ x*w + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' # In real research, R should be 2000 or even 5000
#' lm_boot_out <- lm2boot_out(lm_out, R = 100, seed = 1234)
#' wlevels <- mod_levels(w = "w", fit = lm_out)
#' wlevels
#' out <- cond_indirect_effects(wlevels = wlevels,
#'                              x = "x",
#'                              y = "y",
#'                              m = "m",
#'                              fit = lm_out,
#'                              boot_ci = TRUE,
#'                              boot_out = lm_boot_out)
#' out
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