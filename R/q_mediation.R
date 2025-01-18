#' @title Mediation Models By Regression
#'
#' @description Simple-to-use functions
#' for fitting regression models and
#' testing indirect effects using
#' just one function.
#'
#' @details
#' The family of "q" (quick) functions
#' are for testing mediation effects in
#' common models. These functions do the
#' following in one single call:
#'
#' - Fit the regression models.
#'
#' - Compute and test all the indirect
#'  effects.
#'
#' They are easy-to-use and are suitable
#' for common models which are not too
#' complicated. For now, there are
#' "q" functions for these models:
#'
#' - A simple mediation: One predictor
#' (`x`), one mediator (`m`), one
#' outcome (`y`), and optionally some
#' control variables (`cov`ariates)
#' ([q_simple_mediation()])
#'
#' - A serial mediation model: One
#' predictor (`x`), one or more
#' mediators (`m`), one outcome (`y`),
#' and optionally some control variables
#' (`cov`ariates). The mediators
#' positioned sequentially between `x`
#' and `y` ([q_serial_mediation()]):
#'
#'    - `x -> m1 -> m2 -> ... -> y`
#'
#' - A parallel mediation model: One
#' predictor (`x`), one or more
#' mediators (`m`), one outcome (`y`),
#' and optionally some control variables
#' (`cov`ariates). The mediators
#' positioned in parallel between `x`
#' and `y` ([q_parallel_mediation()]):
#'
#'    - `x -> m1 -> y`
#'
#'    - `x -> m2 -> y`
#'
#'    - ...
#'
#' Users only need to specify the `x`,
#' `m`, and `y` variables, and covariates
#' or control variables, if any (by `cov`),
#' and the functions will automatically
#' identify all indirect effects and
#' total effects.
#'
#' Note that they are *not* intended to
#' be flexible. For models that are
#' different from these common models,
#' it is recommended to fit the models
#' manually, either by structural
#' equation modelling (e.g.,
#' [lavaan::sem()]) or by regression
#' analysis using [stats::lm()] or
#' [lmhelprs::many_lm()]. See
#' <https://sfcheung.github.io/manymome/articles/med_lm.html>
#' for an illustration on how to compute
#' and test indirect effects for an
#' arbitrary mediation model.
#'
#' ## Workflow
#'
#' This is the workflow of the "q"
#' functions:
#'
#' - Do listwise deletion based on all
#' the variables used in the models.
#'
#' - Generate the regression models
#' based on the variables specified.
#'
#' - Fit all the models by OLS regression
#' using [stats::lm()].
#'
#' - Call [all_indirect_paths()] to
#'  identify all indirect paths.
#'
#' - Call [many_indirect_effects()] to
#'  compute all indirect effects and
#'  form their confidence intervals.
#'
#' - Call [total_indirect_effect()] to
#'  compute the total indirect effect.
#'
#' - Return all the results for printing.
#'
#' The output of the "q" functions have
#' a `print` method for
#' printing all the major results.
#'
#' ## Notes
#'
#' ### Flexibility
#'
#' The "q" functions are designed to be
#' easy to use. They are not designed to
#' be flexible. For maximum flexibility,
#' fit the models manually and call
#' functions such as
#' [indirect_effect()] separately. See
#' <https://sfcheung.github.io/manymome/articles/med_lm.html>
#' for illustrations.
#'
#' ### Monte Carlo Confidence Intervals
#'
#' We do not recommend using Monte Carlo
#' confidence intervals for models
#' fitted by regression because the
#' covariances between parameter
#' estimates are assumed to be zero,
#' which may not be the case in some
#' models. Therefore, the "q" functions
#' do not support Monte Carlo confidence
#' intervals. If Monte Carlo intervals
#' are desired, please fit the model by
#' structural equation modeling using
#' [lavaan::sem()].
#'
#' @param x The name of the predictor.
#'
#' @param y The name of the outcome.
#'
#' @param m A character vector of the
#' name(s) of the mediator(s). For
#' a simple mediation model, it must
#' has only one name. For serial and
#' parallel mediation models, it can
#' have one or more names. For a serial
#' mediation models, the direction of
#' the paths go from the first names to
#' the last names. For example,
#' `c("m1", "m3", "m4")` denoted that
#' the path is `m1 -> m3 -> m4`.
#'
#' @param cov The names of the covariates,
#' if any. If it is a character vector,
#' then the outcome (`y`) and all
#' mediators (`m`) regress on all
#' the covariates. If it is a named
#' list of character vectors, then the
#' covariates in an element predict
#' only the variable with the name of this
#' element. For example, `list(m1 = "c1", dv = c("c2", "c3"))`
#' indicates that `c1` predicts `"m1"`,
#' while `c2` and `c3` predicts `"dv"`.
#' Default is `NULL`, no covariates.
#'
#' @param data The data frame. Note that
#' listwise deletion will be used and
#' only cases with no missing data on
#' all variables in the model (e.g.,
#' `x`, `m`, `y` and `cov`) will be
#' retained.
#'
#' @param ci_type The type of the
#' confidence interval. Only `"boot"`,
#' the default, is supported and
#' recommended for models fitted by
#' regression. Therefore, this argument
#' can be omitted for now. Set it to
#' `NULL` and confidence intervals are
#' not formed.
#'
#' @param level The level of confidence
#' of the confidence interval. Default
#' is .95 (for 95% confidence intervals).
#'
#' @param R The number of bootstrap
#' samples. Default is 100. Should be
#' set to 5000 or at least 10000.
#'
#' @param seed The seed for the random
#' number generator. Default is `NULL`.
#' Should nearly always be set to an
#' integer to make the results
#' reproducible.
#'
#' @param boot_type The type of the
#' bootstrap confidence intervals.
#' Default is `"perc"`, percentile
#' confidence interval. Set `"bc"` for
#' bias-corrected confidence interval.
#'
#' @param model The type of model. For
#' [q_mediation()], it can be
#' `"simple"` (simple mediation model),
#' `"serial"` (serial mediation model),
#' or `"parallel"` (parallel mediation
#' model). It is recommended to call
#' the corresponding wrappers directly
#' ([q_simple_mediation()],
#' [q_serial_mediation()], and
#' [q_parallel_mediation()]) instead of
#' call [q_mediation()].
#'
#' @seealso [lmhelprs::many_lm()] for
#' fitting several regression models
#' using model syntax,
#' [indirect_effect()] for computing and
#' testing a specific path,
#' [all_indirect_paths()] for
#' identifying all paths in a model,
#' [many_indirect_effects()] for
#' computing and testing indirect
#' effects along several paths, and
#' [total_indirect_effect()] for
#' computing and testing the total
#' indirect effects.
#'
#' @name q_mediation
NULL

#' @return
#' The function [q_mediation()] returns
#' a `q_mediation` class object, with
#' its `print` method.
#'
#' @describeIn q_mediation The general
#' "q" function for common mediation
#' models. Not to be used directly.
#' @export

q_mediation <- function(x,
                        y,
                        m = NULL,
                        cov = NULL,
                        data = NULL,
                        ci_type = "boot",
                        level = .95,
                        R = 100,
                        seed = NULL,
                        boot_type = c("perc", "bc"),
                        model = NULL) {
  if (is.null(model)) {
    stop("Must specify the model by setting the argument 'model'.")
  }
  boot_type <- match.arg(boot_type)
  # Form the model
  lm_forms <- switch(model,
                     simple = form_models_simple(x = x,
                                                 y = y,
                                                 m = m,
                                                 cov = cov),
                     serial = form_models_serial(x = x,
                                                 y = y,
                                                 m = m,
                                                 cov = cov),
                     parallel = form_models_parallel(x = x,
                                                     y = y,
                                                     m = m,
                                                     cov = cov))

  # Do listwise deletion
  to_delete <- lm_listwise(formulas = lm_forms,
                           data = data)
  if (length(to_delete) > 0) {
    data <- data[-to_delete, , drop = FALSE]
  }

  # Regression analysis
  lm_all <- sapply(c(m, y),
                   function(xx) {NA},
                   simplify = FALSE)
  for (i in c(m, y)) {
    lm_all[[i]] <- eval(bquote(lm(.(stats::as.formula(lm_forms[[i]])),
                                  data = data)))
  }
  lm_all <- lm2list(lm_all)

  # Indirect effect
  paths <- all_indirect_paths(lm_all,
                              x = x,
                              y = y,
                              exclude = unique(unlist(cov)))
  ind_ustd <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE)
  # Store the bootstrap estimates
  ind_with_boot_out <- ind_ustd[[1]]
  ind_stdy <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE,
                                    standardized_y = TRUE,
                                    ci_out = ind_with_boot_out)
  ind_stdx <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE,
                                    standardized_x = TRUE,
                                    ci_out = ind_with_boot_out)
  ind_std0 <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE,
                                    standardized_y = TRUE,
                                    standardized_x = TRUE,
                                    ci_out = ind_with_boot_out)

  # Total indirect effects

  ind_total_ustd <- total_indirect_effect(ind_ustd, x = x, y = y)
  ind_total_stdx <- total_indirect_effect(ind_stdx, x = x, y = y)
  ind_total_stdy <- total_indirect_effect(ind_stdy, x = x, y = y)
  ind_total_std0 <- total_indirect_effect(ind_std0, x = x, y = y)

  # Combine the output
  out <- list(lm_out = lm_all,
              lm_form = lm_forms,
              ind_out = list(ustd = ind_ustd,
                             stdx = ind_stdx,
                             stdy = ind_stdy,
                             stdxy = ind_std0),
              ind_total = list(ustd = ind_total_ustd,
                               stdx = ind_total_stdx,
                               stdy = ind_total_stdy,
                               stdxy = ind_total_std0))
  model_class <- switch(model,
                        simple = "q_simple_mediation",
                        serial = "q_serial_mediation",
                        parallel = "q_parallel_mediation")
  class(out) <- c(model_class,
                  "q_mediation",
                  class(out))
  return(out)
}

#' @return
#' The function [q_simple_mediation()] returns
#' a `q_simple_mediation` class object, which
#' is a subclass of `q_mediation`.
#'
#' @examples
#'
#' # ===== Simple mediation
#'
#' # Set R to 5000 or 10000 in real studies
#' out <- q_simple_mediation(x = "x",
#'                           y = "y",
#'                           m = "m",
#'                           cov = c("c2", "c1"),
#'                           data = data_med,
#'                           R = 100,
#'                           seed = 1234)
#' out
#'
#' # Different control variables for m and y
#' # out <- q_simple_mediation(x = "x",
#' #                           y = "y",
#' #                           m = "m",
#' #                           cov = list(m = "c1",
#' #                                      y = c("c1", "c2")),
#' #                           data = data_med,
#' #                           R = 100,
#' #                           seed = 1234)
#'
#' @describeIn q_mediation A wrapper of [q_mediation()] for
#'  simple mediation models (a model with only one mediator).
#' @export

q_simple_mediation <- function(x,
                               y,
                               m = NULL,
                               cov = NULL,
                               data = NULL,
                               ci_type = "boot",
                               level = .95,
                               R = 100,
                               seed = NULL,
                               boot_type = c("perc", "bc")) {
  if (ci_type != "boot") {
    stop("Monte Carlo confidence interval is not recommended",
         " for models fitted by regression.")
  }
  out <- q_mediation(x = x,
                     y = y,
                     m = m,
                     cov = cov,
                     data = data,
                     ci_type = ci_type,
                     level = level,
                     R = R,
                     seed = seed,
                     boot_type = boot_type,
                     model = "simple")
  return(out)
}

#' @return
#' The function [q_serial_mediation()] returns
#' a `q_serial_mediation` class object, which
#' is a subclass of `q_mediation`.
#'
#' @examples
#'
#' # ===== Serial mediation
#'
#' # Set R to 5000 or 10000 in real studies
#' out <- q_serial_mediation(x = "x",
#'                           y = "y",
#'                           m = c("m1", "m2"),
#'                           cov = c("c2", "c1"),
#'                           data = data_serial,
#'                           R = 100,
#'                           seed = 1234)
#' out
#'
#' # Different control variables for m and y
#' # out <- q_serial_mediation(x = "x",
#' #                           y = "y",
#' #                           m = c("m1", "m2"),
#' #                           cov = list(m1 = "c1",
#' #                                      m2 = c("c2", "c1"),
#' #                                      y = "c2"),
#' #                           data = data_serial,
#' #                           R = 100,
#' #                           seed = 1234)
#'
#' @describeIn q_mediation A wrapper of [q_mediation()] for
#'  serial mediation models.
#' @export

q_serial_mediation <- function(x,
                               y,
                               m = NULL,
                               cov = NULL,
                               data = NULL,
                               ci_type = "boot",
                               level = .95,
                               R = 100,
                               seed = NULL,
                               boot_type = c("perc", "bc")) {
  if (ci_type != "boot") {
    stop("Monte Carlo confidence interval is not recommended",
         " for models fitted by regression.")
  }
  out <- q_mediation(x = x,
                     y = y,
                     m = m,
                     cov = cov,
                     data = data,
                     ci_type = ci_type,
                     level = level,
                     R = R,
                     seed = seed,
                     boot_type = boot_type,
                     model = "serial")
  return(out)
}

#' @return
#' The function [q_parallel_mediation()] returns
#' a `q_parallel_mediation` class object, which
#' is a subclass of `q_mediation`.
#'
#' @examples
#'
#' # ===== Parallel mediation
#'
# Set R to 5000 or 10000 in real studies
#' out <- q_parallel_mediation(x = "x",
#'                             y = "y",
#'                             m = c("m1", "m2"),
#'                             cov = c("c2", "c1"),
#'                             data = data_parallel,
#'                             R = 100,
#'                             seed = 1234)
#' out
#' # Different control variables for m and y
#' # out <- q_parallel_mediation(x = "x",
#' #                             y = "y",
#' #                             m = c("m1", "m2"),
#' #                             cov = list(m1 = "c1",
#' #                                        m2 = c("c2", "c1"),
#' #                                        y = "c2"),
#' #                             data = data_parallel,
#' #                             R = 100,
#' #                             seed = 1234)
#' # out
#'
#' @describeIn q_mediation A wrapper of [q_mediation()] for
#'  parallel mediation models.
#' @export

q_parallel_mediation <- function(x,
                                 y,
                                 m = NULL,
                                 cov = NULL,
                                 data = NULL,
                                 ci_type = "boot",
                                 level = .95,
                                 R = 100,
                                 seed = NULL,
                                 boot_type = c("perc", "bc")) {
  if (ci_type != "boot") {
    stop("Monte Carlo confidence interval is not recommended",
         " for models fitted by regression.")
  }
  out <- q_mediation(x = x,
                     y = y,
                     m = m,
                     cov = cov,
                     data = data,
                     ci_type = ci_type,
                     level = level,
                     R = R,
                     seed = seed,
                     boot_type = boot_type,
                     model = "parallel")
  return(out)
}

# Helpers


#' @noRd
form_models_simple <- function(x,
                               y,
                               m,
                               cov) {
  if ((length(x) != 1) ||
      (length(y) != 1) ||
      (length(m) != 1)) {
    stop("The model must have exactly one 'x', one 'y', and one 'm'.")
  }
  if (is.list(cov)) {
    cov_m <- cov[[m]]
    cov_y <- cov[[y]]
  } else {
    cov_m <- cov
    cov_y <- cov
  }
  lm_m_form <- paste(m,
                     "~",
                     paste(c(x, cov_m),
                           collapse = " + "))
  lm_y_form <- paste(y,
                     "~",
                     paste(c(m, x, cov_y),
                           collapse = " + "))
  forms <- c(lm_m_form,
             lm_y_form)
  names(forms) <- c(m, y)
  return(forms)
}
# form_models_simple("iv", "dv", "m1", c("c1", "w"))
# form_models_simple("iv", "dv", "m1", list(dv = c("c1", "w"), m1 = "c2"))
# form_models_simple("iv", "dv", "m1", list(dv = c("c1", "w"), m2 = "c2"))
# form_models_simple("iv", "dv", c("m1", "m2"), list(dv = c("c1", "w"), m2 = "c2"))

#' @noRd
form_models_serial <- function(x,
                               y,
                               m,
                               cov) {
  if ((length(x) != 1) ||
      (length(y) != 1)) {
    stop("The model must have exactly one 'x' and one 'y'.")
  }
  if (is.null(m) || (length(m) == 0)) {
    stop("The serial mediation model must have at least one mediator.")
  }
  if (is.list(cov)) {
    cov_m <- sapply(m,
                    function(xx) cov[[xx]],
                    simplify = FALSE)
    cov_y <- cov[[y]]
  } else {
    cov_m <- sapply(m,
                    function(x) {cov},
                    simplify = FALSE)
    cov_y <- cov
  }
  # For serial
  for (i in seq_along(m)) {
    if (i == 1) next
    cov_m[[i]] <- c(m[seq(1, i - 1)], cov_m[[i]])
  }
  tmpfct <- function(m,
                     x,
                     cov_m) {
              paste(m,
                     "~",
                     paste(c(x, cov_m),
                           collapse = " + "))
            }
  lm_m_form <- mapply(tmpfct,
                      m = m,
                      cov_m = cov_m,
                      MoreArgs = list(x = x))
  lm_y_form <- paste(y,
                     "~",
                     paste(c(m, x, cov_y),
                           collapse = " + "))
  names(lm_y_form) <- y
  forms <- c(lm_m_form,
             lm_y_form)
  return(forms)
}

# form_models_serial("iv", "dv", c("m1", "m3"), c("c1", "w"))
# form_models_serial("iv", "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))
# form_models_serial(c("iv", "iv2"), "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))


#' @noRd
form_models_parallel <- function(x,
                                 y,
                                 m,
                                 cov) {
  if ((length(x) != 1) ||
      (length(y) != 1)) {
    stop("The model must have exactly one 'x' and one 'y'.")
  }
  if (is.null(m) || (length(m) == 0)) {
    stop("The parallel mediation model must have at least one mediator.")
  }
  if (is.list(cov)) {
    cov_m <- sapply(m,
                    function(xx) cov[[xx]],
                    simplify = FALSE)
    cov_y <- cov[[y]]
  } else {
    cov_m <- sapply(m,
                    function(x) {cov},
                    simplify = FALSE)
    cov_y <- cov
  }
  tmpfct <- function(m,
                     x,
                     cov_m) {
              paste(m,
                    "~",
                    paste(c(x, cov_m),
                          collapse = " + "))
            }
  lm_m_form <- mapply(tmpfct,
                      m = m,
                      cov_m = cov_m,
                      MoreArgs = list(x = x))
  lm_y_form <- paste(y,
                     "~",
                     paste(c(m, x, cov_y),
                           collapse = " + "))
  names(lm_y_form) <- y
  forms <- c(lm_m_form,
             lm_y_form)
  return(forms)
}

# form_models_parallel("iv", "dv", c("m1", "m3"), c("c1", "w"))
# form_models_parallel("iv", "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))
# form_models_parallel(c("iv", "iv2"), "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))
