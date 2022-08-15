#' @title Bootstrapping Estimates for `lm` Outputs
#'
#' @description Generate bootstrapping estimates for models
#'  in a list of 'lm' outputs.
#'
#' @details
#' This function is for advanced users.
#' [do_boot()] is a function users should
#' try first because [do_boot()] has a general
#' interface for input-specific functions
#' like this one.
#'
#' It does nonparametric bootstrapping to generate bootstrap
#' estimates of the regression coefficients in the regression models
#' of a list of [lm()] outputs, or an `lm_list`-class object
#' created by [lm2list()]. The stored estimates can be
#' used by [indirect_effect()], [cond_indirect_effects()],
#' and related functions
#' in forming bootstrapping confidence intervals for
#' effects such as indirect effect and conditional indirect effects.
#'
#' This approach removes the need to repeat bootstrapping in
#' each call to [indirect_effect()], [cond_indirect_effects()],
#' and related functions.
#' It also ensures that the same set of bootstrap samples
#' is used in all subsequent analyses.
#'
#' @return
#' A `boot_out`-class object that can be used for the `boot_out`
#' argument of [indirect_effect()], [cond_indirect_effects()],
#' and related functions
#' for forming bootstrapping confidence intervals.
#'
#' @param outputs A list of `lm` class objects, or
#'  the output of [lm2list()] (i.e., an `lm_list`-class
#'  object).
#' @param R The number of bootstrap samples. Default is 100.
#' @param seed The seed for the bootstrapping.
#'             Default is `NULL` and seed is not set.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [do_boot()], the general purpose
#'          function that users should try first before
#'          using this function.
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
#' out <- cond_indirect_effects(wlevels = "w",
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

# Generate the function for bootstrapping.
# Return a parameter estimates tables.
#' @noRd

lm_boot2est_i <- function(d, i = NULL, outputs) {
    if (!is.null(i)) {
        d_i <- d[i, ]
      } else {
        d_i <- d
      }
    out_i <- lapply(outputs, stats::update, data = d_i)
    lm2ptable(out_i)
  }
