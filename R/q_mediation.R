#' @title Mediation Models By Regression
#' or SEM
#'
#' @description Simple-to-use functions
#' for fitting linear models by regression
#' or structural equation modeling and
#' testing indirect effects, using
#' just one function.
#'
#' @details
#' The family of "q" (quick) functions
#' are for testing mediation effects in
#' common models. These functions do the
#' following in one single call:
#'
#' - Fit the linear models.
#'
#' - Compute and test all the indirect
#'  effects.
#'
#' They are easy-to-use and are suitable
#' for common models with mediators.
#' For now, there are
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
#' - An arbitrary mediation model: One
#' predictor (`x`), one or more
#' mediators (`m`), one outcome (`y`),
#' and optionally some control variables
#' (`cov`ariates). The mediators
#' positioned in an arbitrary form
#' between `x`
#' and `y`, as long as there are no
#' feedback loops ([q_mediation()]).
#' For example:
#'
#'    - `x -> m1`
#'
#'    - `m1 -> m21 -> y`
#'
#'    - `m1 -> m22 -> y`
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
#' be flexible. For more complex models,
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
#' ## Specifying a Model of an Arbitrary Form
#'
#' If a custom model is to be estimated,
#' instead of setting `model` to a name
#' of the form (`"simple"`,`"serial"`,
#' or `"parallel"`), set `model to the
#' paths between `x` and `y`. It can
#' take one of the following two forms:
#'
#' A character vector, each element a
#' string of a path, with variable names
#' connected by `"->"` (the spaces are
#' optional):
#'
#' \preformatted{c("x -> m11 -> m12 -> y",
#'   "x -> m2 -> y")}
#'
#' A list of character vectors, each
#' vector is a vector of names representing
#' a path, going from the first element
#' to the last element:
#'
#' \preformatted{list(c("x", "m11, "m12", "y"),
#'      c("x", "m2", "y")}
#'
#' The two forms above specify the same
#' model.
#'
#' Paths not included are fixed to zero
#' (i.e., does not "exist" in the model).
#' A path can be specified more than once
#' if this can enhance readability.
#' For example:
#'
#' \preformatted{c("x1 -> m1 -> m21 -> y1",
#'   "x1 -> m1 -> m22 -> y1")}
#'
#' The path `"x1 -> m1"` appears twice,
#' to indicate two different pathways from
#' `x1` to `y1`.
#'
#' ## Workflow
#'
#' The coefficients of the model can be
#' estimated by one of these two
#' methods: OLS (ordinary least squares)
#' regression (setting `fit_method` to
#' `"regression"` or `"lm"`), or path
#' analysis (SEM, structural equation
#' modeling, by setting `fit_method` to
#' `"sem"` or `"lavaan"`).
#'
#' ### Regression
#'
#' This is the workflow of the "q"
#' functions when estimating the
#' coefficients by regression:
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
#' ### Path Analysis
#'
#' This is the workflow of the "q"
#' functions when estimating the
#' coefficients by path analysis (SEM):
#'
#' - By default, cases with missing
#' data only on the mediators and the
#' outcome variable will be retained,
#' and full information maximum
#' likelihood (FIML) will be used to
#' estimate the coefficients.
#' (Controlled by `missing`, default
#' to `"fiml"`) using [lavaan::sem()].
#'
#' - Generate the SEM (`lavaan`) model
#' syntax based on the model specified.
#'
#' - Fit the model by path analysis
#' using [lavaan::sem()].
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
#' ## Testing the Indirect Effects
#'
#' Two methods are available for testing
#' the indirect effects: nonparametric
#' bootstrap confidence intervals
#' (`ci_type` set to `"boot"`) and
#' Monte Carlo confidence intervals
#' (`ci_type` set to `"mc"`).
#'
#' If the coefficients are estimated by
#' OLS regression, only nonparametric
#' bootstrap confidence intervals are
#' supported.
#'
#' If the coefficients are estimated by
#' path analysis (SEM), then both methods
#' are supported.
#'
#' ## Printing the Results
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
#' intervals if OLS regression is used.
#'
#' @param x For [q_mediation()],
#' [q_simple_mediation()],
#' [q_serial_mediation()], and
#' [q_parallel_mediation()], it is the
#' name of the predictor. For the
#' `print` method of these
#' functions, `x` is the output of these
#' functions.
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
#' @param boot_ci Logical. Whether
#' bootstrap confidence interval will be
#' formed. Default is `TRUE`.
#'
#' @param mc_ci Logical. Whether
#' Monte Carlo confidence interval will be
#' formed. Default is `FALSE`. Only
#' supported if `fit_method` is
#' `"sem"` or `"lavaan"`.
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
#' @param ci_type The type of
#' confidence intervals to be formed.
#' Can be either `"boot"` (bootstrapping)
#' or `"mc"` (Monte Carlo). If not
#' supplied or is `NULL`, will check
#' other arguments
#' (e.g, `boot_ci` and `mc_ci`). If
#' supplied, will override `boot_ci`
#' and `mc_ci`. If `fit_method` is
#' `"regression"` or `"lm"`, then only
#' `"boot"` is supported.
#'
#' @param boot_type The type of the
#' bootstrap confidence intervals.
#' Default is `"perc"`, percentile
#' confidence interval. Set `"bc"` for
#' bias-corrected confidence interval.
#' Ignored if `ci_type` is not `"boot"`.
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
#' @param fit_method How the model is
#' to be fitted. If set to `"lm"` or
#' `"regression"`,
#' linear regression will be used
#' (fitted by [stats::lm()]). If set
#' to `"sem"` or `"lavaan"`, structural
#' equation modeling will be used and
#' the model will be fitted by
#' [lavaan::sem()]. Default is `"lm"`.
#'
#' @param missing If `fit_method` is
#' set to `"sem"` or `"lavaan"`, this
#' argument determine how missing data
#' is handled. The default value is
#' `"fiml"` and full information maximum
#' likelihood will be used to handle
#' missing data. Please refer to
#' [lavaan::lavOptions] for other options.
#'
#' @param fixed.x If `fit_method` is
#' set to `"sem"` or `"lavaan"`, this
#' determines whether the observed
#' predictors ("x" variables, including
#' control variables) are treated as
#' fixed variables or random variables.
#' Default is `TRUE`, to mimic the
#' same implicit setting in
#' regression fitted by [stats::lm()].
#'
#' @param sem_args If `fit_method` is
#' set to `"sem"` or `"lavaan"`, this
#' is a named list of arguments to be
#' passed to [lavaan::sem()]. Arguments
#' listed here will not override
#' `missing` and `fixed.x`.
#'
#' @param na.action This argument is
#' no longer supported because using it
#' with `missing` is confusing. Use
#' only `missing` to specify how missing
#' data is to be handled when `fit_method`
#' is set to `"sem"` or `"lavaan"`. If
#' listwise deletion is preferred, set
#' `missing` to `"listwise"`.
#'
#' @param parallel If `TRUE`, default,
#' parallel processing will be used when
#' doing bootstrapping.
#'
#' @param ncores Integer. The number of
#' CPU cores to use when `parallel` is
#' `TRUE`. Default is the number of
#' non-logical cores minus one (one
#' minimum). Will raise an error if
#' greater than the number of cores
#' detected by
#' [parallel::detectCores()]. If
#' `ncores` is set, it will override
#' `make_cluster_args` in [do_boot()].
#'
#' @param progress Logical. Display
#' progress or not.
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
#' @author Idea to fit a model by
#' structural equation modeling by
#' Rong wei Sun <https://orcid.org/0000-0003-0034-1422>,
#' implemented by
#' Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @name q_mediation
NULL

#' @return
#' The function [q_mediation()] returns
#' a `q_mediation` class object, with
#' its `print` method.
#'
#' @examples
#'
#' # ===== A user-specified mediation model
#'
#' # Set R to 5000 or 10000 in real studies
#' # Remove 'parallel' or set it to TRUE for faster bootstrapping
#' # Remove 'progress' or set it to TRUE to see a progress bar
#'
#' out <- q_mediation(x = "x1",
#'                    y = "y1",
#'                    model = c("x1 -> m11 -> m2 -> y1",
#'                              "x1 -> m12 -> m2 -> y1"),
#'                    cov = c("c2", "c1"),
#'                    data = data_med_complicated,
#'                    R = 40,
#'                    seed = 1234,
#'                    parallel = FALSE,
#'                    progress = FALSE)
#' # Suppressed printing of p-values due to the small R
#' # Remove `pvalue = FALSE` when R is large
#' print(out,
#'       pvalue = FALSE)
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
                        boot_ci = TRUE,
                        mc_ci = FALSE,
                        level = .95,
                        R = 100,
                        seed = NULL,
                        ci_type = NULL,
                        boot_type = c("perc", "bc"),
                        model = NULL,
                        fit_method = c("lm", "regression", "sem", "lavaan"),
                        missing = "fiml",
                        fixed.x = TRUE,
                        sem_args = list(),
                        na.action = NA,
                        parallel = TRUE,
                        ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                        progress = TRUE) {
  if (is.null(model)) {
    stop("Must specify the model by setting the argument 'model'.")
  }
  if (!is.null(ci_type)) {
    ci_type <- match.arg(ci_type, c("boot", "mc"))
    if (ci_type == "boot") {
      boot_ci <- TRUE
      mc_ci <- FALSE
    } else {
      boot_ci <- FALSE
      mc_ci <- TRUE
    }
  } else {
    if (boot_ci && mc_ci) {
      stop("boot_ci and mc_ci cannot be both TRUE.")
    }
    if (boot_ci) {
      mc_ci <- FALSE
      ci_type <- "boot"
    } else if (mc_ci) {
      boot_ci <- FALSE
      ci_type <- "mc"
    } else {
      ci_type <- NULL
      boot_ci <- FALSE
      mc_ci <- FALSE
    }
  }
  boot_type <- match.arg(boot_type)
  fit_method <- match.arg(fit_method)
  if (fit_method == "sem") {
    fit_method <- "lavaan"
  }
  if (fit_method == "regression") {
    fit_method <- "lm"
  }

  # ==== Sanity checks ====

  if ((fit_method == "lm") &&
      isTRUE(ci_type == "mc")) {
    stop("Models fitted by regression does not support",
         "Monte Carlo confidence intervals.")
  }

  # ===== Form the model =====

  if (isTRUE(model %in% c("simple", "serial", "parallel"))) {
    model_type <- "standard"
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
  } else {
    model_type <- "user"
    tmp1 <- paths_to_models(model)
    lm_forms <- form_models_paths(tmp1,
                              cov = cov)
  }

  # ==== Do listwise deletion (lm only) ====

  if (fit_method == "lm") {
    to_delete <- lm_listwise(formulas = lm_forms,
                            data = data)
    if (length(to_delete) > 0) {
      data <- data[-to_delete, , drop = FALSE]
    }
  }

  # ==== Fit Model ====

  sem_model <- NULL
  mm <- NULL
  lm_out_lav <- NULL
  lm_all_x <- character(0)
  x_miss <- integer(0)

  if (fit_method == "lm") {

    # ==== Regression analysis ====

    dvs <- names(lm_forms)
    lm_all <- sapply(dvs,
                    function(xx) {NA},
                    simplify = FALSE)
    for (i in dvs) {
      lm_all[[i]] <- eval(bquote(lm(.(stats::as.formula(lm_forms[[i]])),
                                    data = data)))
    }
    lm_all <- lm2list(lm_all)

  } else if (fit_method == "lavaan") {

    # ==== SEM by lavaan ====

    # Keep the name `lm_all` for backward compatibility

    # Always pass all the cases. Let missing do the listwise,
    # if na.action is na.omit
    mm <- mm_from_lm_forms(
            lm_forms,
            data = data,
            na.action = "na.pass"
          )
    sem_model <- b_names_to_lavaan_model(mm$b_names)

    sem_args1 <- utils::modifyList(
                    sem_args,
                    list(model = sem_model,
                         data = mm$model_matrix,
                         meanstructure = TRUE,
                         warn = FALSE,
                         fixed.x = fixed.x,
                         missing = missing)
                  )

    lm_all <- do.call(lavaan::sem,
                      sem_args1)

    fixed.x <- lavaan::lavTech(lm_all, "fixed.x")
    lm_all_x <- lavaan::lavNames(lm_all, "ov.x")
    x_miss <- sum(
                !stats::complete.cases(
                  mm$model_matrix[, lm_all_x, drop = FALSE]
                )
              )
    lm_out_lav <- lm_from_lavaan_list_for_q(
                    fit = lm_all,
                    mm = mm
                  )

  } else {
    # This block should not be reached
    stop("Something's wrong. The fit method is not valid.")
  }

  # ==== do_* ====

  do_ci <- (boot_ci || mc_ci)

  if (progress && do_ci) {
    cat("- Generate ",
        switch(ci_type,
               mc = "Monte Carlo",
               boot = "bootstrap"),
        " estimates ....\n",
        sep = "")
  }

  if (do_ci) {
    ci_out <- switch(ci_type,
                      boot = do_boot(
                                fit = lm_all,
                                R = R,
                                seed = seed,
                                parallel = parallel,
                                progress = progress,
                                ncores = ncores
                              ),
                      mc = do_mc(
                                fit = lm_all,
                                R = R,
                                seed = seed,
                                parallel = parallel,
                                progress = progress,
                                ncores = ncores
                              ),
                      NULL
                  )
  } else {
    ci_out <- NULL
  }

  # ==== Indirect effect ====

  paths <- all_indirect_paths(lm_all,
                              x = x,
                              y = y,
                              exclude = unique(unlist(cov)))

  has_indirect_path <- (length(paths) > 0)

  if (has_indirect_path) {

    if (progress) {
      cat("- Compute unstandardized indirect effect(s) ....\n")
    }

    ind_ustd <- many_indirect_effects(paths = paths,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      ci_out = ci_out)

    # ==== Store the bootstrap estimates ====

    # ind_with_ci_out <- ind_ustd[[1]]

    if (progress) {
      cat("- Compute standardized-y indirect effect(s) ....\n")
    }

    ind_stdy <- many_indirect_effects(paths = paths,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      standardized_y = TRUE,
                                      ci_out = ci_out)
    if (progress) {
      cat("- Compute standardized-x indirect effect(s) ....\n")
    }

    ind_stdx <- many_indirect_effects(paths = paths,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      standardized_x = TRUE,
                                      ci_out = ci_out)

    if (progress) {
      cat("- Compute standardized-x-and-y indirect effect(s) ....\n")
    }

    ind_std0 <- many_indirect_effects(paths = paths,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      standardized_y = TRUE,
                                      standardized_x = TRUE,
                                      ci_out = ci_out)

  } else {

    if (progress) {
      cat("- No indirect path from ",
          x,
          " to ",
          y,
          " in the model. Skip the computation of indirect effects ...\n",
          sep = "")
    }

    ind_ustd <- NULL
    ind_stdy <- NULL
    ind_stdx <- NULL
    ind_std0 <- NULL

  }

  # ==== Total indirect effects ====

  if (has_indirect_path) {

    if (progress) {
      cat("- Compute total indirect effect(s) ....\n")
    }

    ind_total_ustd <- total_indirect_effect(ind_ustd, x = x, y = y)
    ind_total_stdx <- total_indirect_effect(ind_stdx, x = x, y = y)
    ind_total_stdy <- total_indirect_effect(ind_stdy, x = x, y = y)
    ind_total_std0 <- total_indirect_effect(ind_std0, x = x, y = y)

  } else {

    ind_total_ustd <- NULL
    ind_total_stdx <- NULL
    ind_total_stdy <- NULL
    ind_total_std0 <- NULL

  }

  # ==== Direct effects ====

  has_direct_path <- check_path(
                        x = x,
                        y = y,
                        fit = lm_all
                      )

  if (has_direct_path) {
    direct_path <- list(path = list(x = x,
                                    y = y,
                                    m = NULL))
    names(direct_path) <- paste(x, "->", y)

    if (progress) {
      cat("- Compute the direct effect ....\n")
    }

    dir_ustd <- many_indirect_effects(paths = direct_path,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      ci_out = ci_out)

    if (progress) {
      cat("- Compute the standardized-y direct effect ....\n")
    }

    dir_stdy <- many_indirect_effects(paths = direct_path,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      standardized_y = TRUE,
                                      ci_out = ci_out)

    if (progress) {
      cat("- Compute the standardized-x direct effect ....\n")
    }

    dir_stdx <- many_indirect_effects(paths = direct_path,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      standardized_x = TRUE,
                                      ci_out = ci_out)

    if (progress) {
      cat("- Compute the standardized-x-and-y direct effect ....\n")
    }

    dir_std0 <- many_indirect_effects(paths = direct_path,
                                      fit = lm_all,
                                      R = R,
                                      ci_type = ci_type,
                                      boot_type = boot_type,
                                      level = level,
                                      seed = seed,
                                      progress = progress,
                                      ncores = ncores,
                                      parallel = parallel,
                                      standardized_y = TRUE,
                                      standardized_x = TRUE,
                                      ci_out = ci_out)
  } else {

    if (progress) {
      cat("- No direct path from ",
          x,
          " to ",
          y,
          " in the model. Skip the computation of direct effect ...\n",
          sep = "")
    }

    dir_ustd <- NULL
    dir_stdy <- NULL
    dir_stdx <- NULL
    dir_std0 <- NULL
  }

  # ==== Combine the output ====

  if (progress) {
    cat("Computation completed.\n")
  }

  out <- list(lm_out = lm_all,
              lm_form = lm_forms,
              ind_out = list(ustd = ind_ustd,
                             stdx = ind_stdx,
                             stdy = ind_stdy,
                             stdxy = ind_std0),
              ind_total = list(ustd = ind_total_ustd,
                               stdx = ind_total_stdx,
                               stdy = ind_total_stdy,
                               stdxy = ind_total_std0),
              dir_out = list(ustd = dir_ustd,
                             stdx = dir_stdx,
                             stdy = dir_stdy,
                             stdxy = dir_std0),
              call = match.call(),
              model = model,
              x = x,
              y = y,
              m = m,
              fit_method = fit_method,
              sem_args = sem_args,
              sem_model = sem_model,
              lm_out_lav = lm_out_lav,
              model_matrices = mm,
              fixed.x = fixed.x,
              missing = missing,
              x_miss = x_miss,
              lm_all_x = lm_all_x,
              model_type = model_type,
              ci_type = ci_type)
  if (model_type == "standard") {
    model_class <- switch(model,
                          simple = "q_simple_mediation",
                          serial = "q_serial_mediation",
                          parallel = "q_parallel_mediation")
  } else {
    model_class <- "q_user_mediation"
  }
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
#' # Remove 'parallel' or set it to TRUE for faster bootstrapping
#' # Remove 'progress' or set it to TRUE to see a progress bar
#'
#' out <- q_simple_mediation(x = "x",
#'                           y = "y",
#'                           m = "m",
#'                           cov = c("c2", "c1"),
#'                           data = data_med,
#'                           R = 20,
#'                           seed = 1234,
#'                           parallel = FALSE,
#'                           progress = FALSE)
#' # Suppressed printing of p-values due to the small R
#' # Remove `pvalue = FALSE` when R is large
#' print(out,
#'       pvalue = FALSE)
#'
#' # # Different control variables for m and y
#' # out <- q_simple_mediation(x = "x",
#' #                           y = "y",
#' #                           m = "m",
#' #                           cov = list(m = "c1",
#' #                                      y = c("c1", "c2")),
#' #                           data = data_med,
#' #                           R = 100,
#' #                           seed = 1234,
#' #                           parallel = FALSE,
#' #                           progress = FALSE)
#' # out
#'
#' @describeIn q_mediation A wrapper of [q_mediation()] for
#'  simple mediation models (a model with only one mediator).
#' @export

q_simple_mediation <- function(x,
                               y,
                               m = NULL,
                               cov = NULL,
                               data = NULL,
                               boot_ci = TRUE,
                               mc_ci = FALSE,
                               level = .95,
                               R = 100,
                               seed = NULL,
                               ci_type = NULL,
                               boot_type = c("perc", "bc"),
                               fit_method = c("lm", "regression", "sem", "lavaan"),
                               missing = "fiml",
                               fixed.x = TRUE,
                               sem_args = list(),
                               na.action = NA,
                               parallel = TRUE,
                               ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                               progress = TRUE) {
  boot_type <- match.arg(boot_type)
  fit_method <- match.arg(fit_method)
  if (fit_method == "sem") {
    fit_method <- "lavaan"
  }
  if (fit_method == "regression") {
    fit_method <- "lm"
  }
  out <- q_mediation(x = x,
                     y = y,
                     m = m,
                     cov = cov,
                     data = data,
                     boot_ci = boot_ci,
                     mc_ci = mc_ci,
                     level = level,
                     R = R,
                     seed = seed,
                     ci_type = ci_type,
                     boot_type = boot_type,
                     model = "simple",
                     fit_method = fit_method,
                     missing = missing,
                     fixed.x = fixed.x,
                     sem_args = sem_args,
                     na.action = na.action,
                     parallel = parallel,
                     ncores = ncores,
                     progress = progress)
  out$call <- match.call()
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
#' # Remove 'parallel' or set it to TRUE for faster bootstrapping
#' # Remove 'progress' or set it to TRUE to see a progress bar
#'
#' # out <- q_serial_mediation(x = "x",
#' #                           y = "y",
#' #                           m = c("m1", "m2"),
#' #                           cov = c("c2", "c1"),
#' #                           data = data_serial,
#' #                           R = 40,
#' #                           seed = 1234,
#' #                           parallel = FALSE,
#' #                           progress = FALSE)
#'
#' # # Suppressed printing of p-values due to the small R
#' # # Remove `pvalue = FALSE` when R is large
#' # print(out,
#' #       pvalue = FALSE)
#'
#' # # Different control variables for m and y
#' # out <- q_serial_mediation(x = "x",
#' #                           y = "y",
#' #                           m = c("m1", "m2"),
#' #                           cov = list(m1 = "c1",
#' #                                      m2 = c("c2", "c1"),
#' #                                      y = "c2"),
#' #                           data = data_serial,
#' #                           R = 100,
#' #                           seed = 1234,
#' #                           parallel = FALSE,
#' #                           progress = FALSE)
#' # out
#'
#' @describeIn q_mediation A wrapper of [q_mediation()] for
#'  serial mediation models.
#' @export

q_serial_mediation <- function(x,
                               y,
                               m = NULL,
                               cov = NULL,
                               data = NULL,
                               boot_ci = TRUE,
                               mc_ci = FALSE,
                               level = .95,
                               R = 100,
                               seed = NULL,
                               ci_type = NULL,
                               boot_type = c("perc", "bc"),
                               fit_method = c("lm", "regression", "sem", "lavaan"),
                               missing = "fiml",
                               fixed.x = TRUE,
                               sem_args = list(),
                               na.action = NA,
                               parallel = TRUE,
                               ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                               progress = TRUE) {
  boot_type <- match.arg(boot_type)
  fit_method <- match.arg(fit_method)
  if (fit_method == "sem") {
    fit_method <- "lavaan"
  }
  if (fit_method == "regression") {
    fit_method <- "lm"
  }
  out <- q_mediation(x = x,
                     y = y,
                     m = m,
                     cov = cov,
                     data = data,
                     boot_ci = boot_ci,
                     mc_ci = mc_ci,
                     level = level,
                     R = R,
                     seed = seed,
                     ci_type = ci_type,
                     boot_type = boot_type,
                     model = "serial",
                     fit_method = fit_method,
                     missing = missing,
                     fixed.x = fixed.x,
                     sem_args = sem_args,
                     na.action = na.action,
                     parallel = parallel,
                     ncores = ncores,
                     progress = progress)
  out$call <- match.call()
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
#' # Set R to 5000 or 10000 in real studies
#' # Remove 'parallel' or set it to TRUE for faster bootstrapping
#' # Remove 'progress' or set it to TRUE to see a progress bar
#'
#' # out <- q_parallel_mediation(x = "x",
#' #                             y = "y",
#' #                             m = c("m1", "m2"),
#' #                             cov = c("c2", "c1"),
#' #                             data = data_parallel,
#' #                             R = 40,
#' #                             seed = 1234,
#' #                             parallel = FALSE,
#' #                             progress = FALSE)
#' # # Suppressed printing of p-values due to the small R
#' # # Remove `pvalue = FALSE` when R is large
#' # print(out,
#' #       pvalue = FALSE)
#'
#' # # Different control variables for m and y
#' # out <- q_parallel_mediation(x = "x",
#' #                             y = "y",
#' #                             m = c("m1", "m2"),
#' #                             cov = list(m1 = "c1",
#' #                                        m2 = c("c2", "c1"),
#' #                                        y = "c2"),
#' #                             data = data_parallel,
#' #                             R = 100,
#' #                             seed = 1234,
#' #                             parallel = FALSE,
#' #                             progress = FALSE)
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
                                 boot_ci = TRUE,
                                 mc_ci = FALSE,
                                 level = .95,
                                 R = 100,
                                 seed = NULL,
                                 ci_type = NULL,
                                 boot_type = c("perc", "bc"),
                                 fit_method = c("lm", "regression", "sem", "lavaan"),
                                 missing = "fiml",
                                 fixed.x = TRUE,
                                 sem_args = list(),
                                 na.action = NA,
                                 parallel = TRUE,
                                 ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
                                 progress = TRUE) {
  boot_type <- match.arg(boot_type)
  fit_method <- match.arg(fit_method)
  if (fit_method == "sem") {
    fit_method <- "lavaan"
  }
  if (fit_method == "regression") {
    fit_method <- "lm"
  }
  out <- q_mediation(x = x,
                     y = y,
                     m = m,
                     cov = cov,
                     data = data,
                     boot_ci = boot_ci,
                     mc_ci = mc_ci,
                     level = level,
                     R = R,
                     seed = seed,
                     ci_type = ci_type,
                     boot_type = boot_type,
                     model = "parallel",
                     fit_method = fit_method,
                     missing = missing,
                     fixed.x = fixed.x,
                     sem_args = sem_args,
                     na.action = na.action,
                     parallel = parallel,
                     ncores = ncores,
                     progress = progress)
  out$call <- match.call()
  return(out)
}

# Helpers


#' @noRd
form_models_simple <- function(x,
                               y,
                               m,
                               cov = NULL) {
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
                               cov = NULL) {
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
                                 cov = NULL) {
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

#' @describeIn q_mediation The `print` method of the outputs
#' of [q_mediation()], [q_simple_mediation()],
#' [q_serial_mediation()], and [q_parallel_mediation()].
#'
#' @param digits Number of digits to
#' display. Default is 4.
#'
#' @param annotation Logical. Whether
#' the annotation after the table of
#' effects is to be printed. Default is
#' `TRUE.`
#'
#' @param pvalue Logical. If `TRUE`,
#' asymmetric *p*-values based on
#' bootstrapping will be printed if
#' available. Default is `TRUE`.
#'
#' @param pvalue_digits Number of decimal
#' places to display for the *p*-values.
#' Default is 4.
#'
#' @param se Logical. If `TRUE` and
#' confidence intervals are available,
#' the standard errors of the estimates
#' are also printed. They are simply the
#' standard deviations of the bootstrap
#' estimates. Default is `TRUE`.
#'
#' @param for_each_path Logical. If
#' `TRUE`, each of the paths will be
#' printed individually, using the
#' `print`-method of the output of
#' [indirect_effect()]. Default is
#' `FALSE`.
#'
#' @param se_ci Logical. If `TRUE` and
#' confidence interval has not been
#' computed, the function will try
#' to compute them from stored
#' standard error if the original
#' standard error is to be used.
#' Ignored
#' if confidence interval has already
#' been computed. Default
#' is `TRUE`.
#'
#' @param wrap_computation Logical.
#' If `TRUE`, the default, long
#' computational symbols and values
#' will be wrapped to fit to the screen
#' width.
#'
#' @param lm_beta  If `TRUE`,
#' when printing the regression results
#' of [stats::lm()],
#' standardized coefficients are
#' computed and included in the
#' printout. Only numeric variables will
#' be computed, and any derived terms,
#' such as product terms, will be formed
#' *after* standardization. Default
#' is `TRUE`.
#'
#' @param lm_ci If `TRUE`,
#' when printing the regression results
#' of [stats::lm()],
#' confidence
#' interval based on *t* statistic
#' and standard error will be computed
#' and added to the output. Default is
#' `TRUE`.
#'
#' @param lm_ci_level The level of confidence
#' of the confidence interval. Ignored
#' if `lm_ci` is not `TRUE`.
#'
#' @param sem_style How the for the
#' model is to be printed if the model
#' is fitted by structural equation
#' modeling (using `lavaan`). Default
#' is `"lm"` and the results will be
#' printed in a style similar to that
#' of [summary()] output of [stats::lm()].
#' If `"lavaan"`, the results will be
#' printed in usual `lavaan` style.
#'
#' @param ... Other arguments. If
#' `for_each_path` is `TRUE`, they
#' will be passed to the print method
#' of the output of [indirect_effect()].
#' Ignored otherwise.
#'
#' @export

print.q_mediation <- function(x,
                              digits = 4,
                              annotation = TRUE,
                              pvalue = TRUE,
                              pvalue_digits = 4,
                              se = TRUE,
                              for_each_path = FALSE,
                              se_ci = TRUE,
                              wrap_computation = TRUE,
                              lm_ci = TRUE,
                              lm_beta = TRUE,
                              lm_ci_level = .95,
                              sem_style = c("lm", "lavaan"),
                              ...) {

  fit_method <- x$fit_method

  sem_style <- match.arg(sem_style)

  # ==== Print basic information ====

  if (x$model_type == "standard") {
    model_name <- switch(x$model,
                        simple = "Simple Mediation Model",
                        serial = "Serial Mediation Model",
                        parallel = "Parallel Mediation Model")
  } else {
    model_name <- "User-Specified Model"
  }
  cat("\n", "=============== ", model_name, " ===============", "\n", sep = "")
  cat("\nCall:\n")
  cat("\n")
  print(x$call)

  cat("\n")
  cat("===================================================\n")
  cat("|                Basic Information                |\n")
  cat("===================================================\n")
  cat("\nPredictor(x):", x$x)
  cat("\nOutcome(y):", x$y)
  cat("\nMediator(s)(m):", paste0(x$m, collapse = ", "))
  cat("\nModel:", model_name)
  cat("\n")

  if (fit_method == "lm") {
    cat("\n")
    cat("The regression models fitted:")
    cat("\n")
    cat("\n")
    cat(x$lm_form,
        sep = "\n")

    n <- stats::nobs(x$lm_out[[1]])
    cat("\n")
    cat("The number of cases included:", n, "\n")
  }

  if (fit_method == "lavaan") {
    cat("\n")
    cat("The path model fitted:")
    cat("\n")
    cat("\n")
    cat(x$sem_model,
        "\n")
    fit_missing <- x$lm_out@Options$missing
    missing_str <- switch(
        fit_missing,
        ml = "FIML (full information maximum likelihood)",
        fiml = "FIML (full information maximum likelihood)",
        listwise = "Listwise deletion",
        ml.x = "FIML (full information maximum likelihood) (cases with missing x kept)",
        paste(fit_missing, "(See the help page of lavaan on this optoin)")
      )
    ntotal <- lavaan::lavInspect(
                x$lm_out,
                "ntotal"
              )
    norig <- lavaan::lavInspect(
                x$lm_out,
                "norig"
              )
    miss_patterns <- lavaan::lavInspect(
                x$lm_out,
                "patterns"
              )
    n_patterns <- nrow(miss_patterns)
    has_no_na <- (max(rowSums(miss_patterns)) == ncol(miss_patterns))
    cat("\n")
    cat("The original number of cases:", norig, "\n")
    cat("The number of cases in the analysis:", ntotal, "\n")

    if ((x$x_miss > 0) &&
        x$fixed.x &&
        (fit_missing %in% c("ml", "fiml"))) {
      cat(x$x_miss, "case(s) deleted due to missing data on 'x' variable(s).\n")
      cat("The x-variable(s) in the lavaan model:",
          paste0(x$lm_all_x, collapse = ", "),
          "\n")
      cat("To include these cases, set the following arguments:\n",
          "missing = 'fiml.x'\n",
          "fixed.x = FALSE\n")
    }

    cat("Missing data handling:",
        missing_str,
        "\n")

    if (((n_patterns == 1) &&
         has_no_na &&
         (fit_missing != "listwise")) ||
        (norig == ntotal)) {
      cat("No missing data in this analysis.\n")
    }

  }

  # ==== Print path coefficients ====

  if (fit_method == "lm") {

    # ==== Print the regression results ====

    cat("\n")
    cat("===================================================\n")
    cat("|               Regression Results                |\n")
    cat("===================================================\n")

    tmp <- tryCatch(utils::capture.output(print(summary(x$lm_out,
                                                        betaselect = lm_beta,
                                                        ci = lm_ci,
                                                        level = lm_ci_level),
                                                digits_decimal = digits)),
                    error = function(e) e)
    if (inherits(tmp, "error")) {
      tmp <- utils::capture.output(print(summary(x$lm_out),
                                        digits = digits))
    }
    i <- grepl("^<environment:", tmp)
    # tmp <- tmp[!i]
    tmp[i] <- ""

    cat(tmp,
        sep = "\n")
  }

  if (fit_method == "lavaan") {

    # ==== Print path analysis results ====

    cat("\n")
    cat("===================================================\n")
    cat("|             Path Analysis Results               |\n")
    cat("===================================================\n")
    cat("\n")

    print(x$lm_out)

    # TODO:
    # - Need to improve the printout for users
    #   not familiar with lavaan.

    if (sem_style == "lm") {

      # ==== Print lm style ====

      print_lavaan_as_lm(mm = x$model_matrices,
                         fit = x$lm_out,
                         lm_out_lav = x$lm_out_lav,
                         digits = digits,
                         pvalue_digits = pvalue_digits,
                         lm_ci = lm_ci,
                         lm_beta = lm_beta,
                         lm_ci_level = lm_ci_level)

    } else {

      # ==== Print lavaan style ====

      est <- lavaan::parameterEstimates(
              x$lm_out,
              level = lm_ci_level,
              rsquare = TRUE,
              output = "text",
              standardized = lm_beta
            )

      i_var <- (est$op == "~~") &
              (est$lhs == est$rhs)

      est <- est[!i_var, ]

      cat("\nParameter Estimates:\n")

      print(est)
    }

  }

  # ==== Print indirect effects ====

  opt_width <- getOption("width")

  if (!is.null(x$ind_out$ustd) ||
      !is.null(x$ind_out$stdx) ||
      !is.null(x$ind_out$stdy) ||
      !is.null(x$ind_out$stdxy)) {
    cat("\n")
    cat("===================================================\n")
    cat("|             Indirect Effect Results             |\n")
    cat("===================================================\n")
  }

  if (!is.null(x$ind_out$ustd)) {
    # cat("\n")
    # cat("===== Indirect Effect(s) =====")
    # cat("\n")
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_out$ustd,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  if (!is.null(x$ind_out$stdx)) {
    # cat("\n")
    # cat("===== Indirect Effect(s): Predictor (", x$x, ") Standardized =====",
    #     sep = "")
    # cat("\n")
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_out$stdx,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  if (!is.null(x$ind_out$stdy)) {
    # cat("\n")
    # cat("===== Indirect Effect(s): Outcome (", x$y, ") Standardized =====",
    #     sep = "")
    # cat("\n")
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_out$stdy,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  if (!is.null(x$ind_out$stdxy)) {
    # cat("\n")
    # cat("===== Indirect Effect(s): Both Predictor (", x$x,
    #     ") and Outcome (", x$y, ") Standardized =====",
    #     sep = "")
    # cat("\n")
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_out$stdxy,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  # ==== Print total effects ====

  print_total <- (isTRUE(x$model != "simple"))

  if ((!is.null(x$ind_total$ustd) ||
       !is.null(x$ind_total$stdx) ||
       !is.null(x$ind_total$stdy) ||
       !is.null(x$ind_total$stdxy)) && print_total) {
    cat("\n")
    cat("===================================================\n")
    cat("|          Total Indirect Effect Results          |\n")
    cat("===================================================\n")
  }

  if (!is.null(x$ind_total$ustd) && print_total) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_total$ustd,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          se_ci = se_ci,
          wrap_computation = wrap_computation,
          ...)
  }

  if (!is.null(x$ind_total$stdx) && print_total) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_total$stdx,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          se_ci = se_ci,
          wrap_computation = wrap_computation,
          ...)
  }

  if (!is.null(x$ind_total$stdy) && print_total) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_total$stdy,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          se_ci = se_ci,
          wrap_computation = wrap_computation,
          ...)
  }

  if (!is.null(x$ind_total$stdxy) && print_total) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$ind_total$stdxy,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          se_ci = se_ci,
          wrap_computation = wrap_computation,
          ...)
  }

  # ==== Print direct effects ====

  print_direct <- !is.null(x$dir_out$ustd) ||
                  !is.null(x$dir_out$stdx) ||
                  !is.null(x$dir_out$stdy) ||
                  !is.null(x$dir_out$stdxy)

  print_direct_std <- !is.null(x$dir_out$stdx) ||
                      !is.null(x$dir_out$stdy) ||
                      !is.null(x$dir_out$stdxy)

  if (print_direct) {
    cat("\n")
    cat("===================================================\n")
    cat("|              Direct Effect Results              |\n")
    cat("===================================================\n")
  }

  if (!is.null(x$dir_out$ustd)) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$dir_out$ustd,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  if (!is.null(x$dir_out$stdx)) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$dir_out$stdx,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  if (!is.null(x$dir_out$stdy)) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$dir_out$stdy,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  if (!is.null(x$dir_out$stdxy)) {
    cat("\n", strrep("-", ceiling(opt_width * .8)), "\n", sep = "")
    print(x$dir_out$stdxy,
          digits = digits,
          annotation = annotation,
          pvalue = pvalue,
          pvalue_digits = pvalue_digits,
          se = se,
          for_each_path = for_each_path,
          ...)
  }

  # ==== Print Notes ====

  str_note <- character(0)

  ci_type <- x$ci_type

  if (is.null(ci_type)) {
    ci_type <- "none"
  }

  ci_name_lower <- switch(
                    ci_type,
                    boot = "bootstrap",
                    mc = "Monte Carlo",
                    "")
  ci_name_upper <- switch(
                      ci_type,
                      boot = "Bootstrap",
                      mc = "Monte Carlo",
                      "")

  t_stat_name <- switch(fit_method,
                        lm = "OLS t-statistc",
                        lavaan = "z-statistc")

  if (print_direct &&
      (ci_type != "none")) {
    str_note <- c(str_note,
             strwrap(paste("- For reference, the",
                           ci_name_lower,
                           "confidence interval",
                           "(and",
                           ci_name_lower,
                           "p-value, if requested) of the",
                           "(unstandardized) direct effect is also reported.",
                           "The",
                           ci_name_lower,
                           "p-value and the",
                           t_stat_name,
                           "p-value",
                           "can be different."),
                exdent = 2))
  }

  if (print_direct_std) {
    str_note <- c(str_note,
             strwrap(paste("- For the direct effects with either 'x'-variable or",
                           "'y'-variable, or both, standardized, it is",
                           "recommended to use the",
                           ci_name_lower,
                           "confidence intervals,",
                           "which take into account the sampling error of",
                           "the sample standard deviations."),
                exdent = 2))
  }

  if (pvalue && (ci_type == "boot")) {
    str_note <- c(str_note,
             strwrap(paste("- The asymmetric bootstrap value for an effect",
                      "is the same whether x and/or y is/are",
                      "standardized."),
                exdent = 2))
  }
  if (length(str_note) > 0) {
    cat("\n")
    cat("===================================================\n")
    cat("|                      Notes                      |\n")
    cat("===================================================\n")
    cat("\n")
    cat(str_note,
        sep = "\n")
  }

  invisible(x)
}

#' @noRd
print_lavaan_as_lm <- function(
                         mm,
                         fit,
                         lm_out_lav,
                         digits = 4,
                         pvalue_digits = 4,
                         lm_ci,
                         lm_beta,
                         lm_ci_level) {
  out0 <- lm_from_lavaan_list_for_q(
                    fit = fit,
                    mm = mm,
                    ci_level = lm_ci_level,
                    rsq_test = FALSE
                  )
  dvs <- names(out0)
  for (i in seq_along(out0)) {
    tmp <- paste0("Predicting ", dvs[i], " :")
    a <- nchar(tmp)
    cat("\n",
        strrep("-", a), "\n",
        tmp, "\n",
        strrep("-", a), "\n")

    # ==== Print model =====

    tmp <- utils::capture.output(print(lm_out_lav[[i]]$model))
    j <- grepl("<environment", tmp, fixed = TRUE)
    tmp <- tmp[!j]
    cat("\nModel:\n", tmp, "\n")

    # ==== Print coefficients =====

    out_i <- out0[[i]]$coefs_lm
    if (!lm_beta) {
      b <- which(colnames(out0[[i]]$coefs_lm) == "betaS")
      if (length(b) > 0) {
        out_i <- out_i[, -b]
      }
    }
    if (!lm_ci) {
      b <- match(c("CI.lo", "CI.hi"), colnames(out0[[i]]$coefs_lm))
      b <- b[!is.na(b)]
      if (length(b) > 0) {
        out_i <- out_i[, -b]
      }
    }
    i_p <- grepl("Pr(>", colnames(out_i), fixed = TRUE)
    out_i[, !i_p] <- round(out_i[, !i_p], digits)
    out_i[, i_p] <- round(out_i[, i_p], pvalue_digits)
    stats::printCoefmat(out_i,
                 digits = digits,
                 na.print = strrep("-", digits))

    # ==== R-squared =====

    names(lm_out_lav[[i]])
    rsq <- unname(lm_out_lav[[i]]$rsquare)
    cat("\nR-square: ",
        formatC(rsq,
                digits = digits,
                format = "f"),
        "\n")

    # ==== LRT for R-squared =====

    rsq_lrt <- lm_out_lav[[i]]$fit_null_lrt
    tmp <- utils::capture.output(print(rsq_lrt))
    j <- grepl("Chi-Squared", tmp, fixed = TRUE)
    tmp[j] <- paste(tmp[j], "for the R-square")
    cat(tmp,
        sep = "\n")

    # ==== Notes =====

    cat("\n")

    if (lm_beta) {
      term_types <- lm_out_lav[[i]]$term_types
      vars_std <- c(dvs[i], names(term_types)[term_types == "numeric"])
      tmp <- strwrap(paste0("- BetaS are standardized coefficients with (a) ",
                            "only numeric variables standardized and (b) ",
                            "product terms formed after standardization. ",
                            "Variable(s) standardized is/are: ",
                            paste0(vars_std, collapse = ", ")),
                      exdent = 2)
      cat(tmp,
          sep = "\n")
    }
    if (lm_ci) {
      tmp0 <- paste0(formatC(lm_ci_level * 100,
                             digits = 1,
                             format = "f"),
                      "%")
      tmp <- strwrap(paste0("- CI.lo and CI.hi are the ", tmp0,
                            " confidence levels of 'Estimate' ",
                            "computed from the z values and ",
                            "standard errors."),
                      exdent = 2)
      cat(tmp,
          sep = "\n")
    }
  }
}
