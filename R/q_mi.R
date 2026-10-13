#' @noRd
q_mi_ci <- function(ci_type, boot_ci, mc_ci, boot_default) {
  if (identical(ci_type, "boot") ||
      (is.null(ci_type) && !boot_default && isTRUE(boot_ci))) {
    stop("MI supports Monte Carlo intervals, not bootstrap intervals. Use ci_type = 'mc'.")
  }
  if (is.null(ci_type) && (boot_ci || mc_ci)) return("mc")
  ci_type
}

#' @noRd
q_mi_mediation <- function(x, y, m, cov, model, model_type, lm_forms,
                           data, fixed.x, sem_args, mice_args, mi_vars,
                           mi_seed, ci_type, level, R, seed, parallel,
                           ncores, progress, original_call) {
  if (!requireNamespace("lavaan.mi", quietly = TRUE)) {
    stop("Install 'lavaan.mi' to use missing = 'mi'.")
  }
  if (!is.list(mice_args) ||
      (length(mice_args) && (is.null(names(mice_args)) ||
                             any(!nzchar(names(mice_args))) ||
                             anyDuplicated(names(mice_args))))) {
    stop("'mice_args' must be a named list.")
  }
  if (!is.list(sem_args) ||
      (length(sem_args) && (is.null(names(sem_args)) ||
                            any(!nzchar(names(sem_args))) ||
                            anyDuplicated(names(sem_args))))) {
    stop("'sem_args' must be a named list.")
  }
  if (!is.null(mice_args$data)) stop("Supply raw data through 'data', not mice_args$data.")
  if (!is.null(mi_seed) && !is.null(mice_args$seed)) {
    stop("Specify the imputation seed in only one of mi_seed and mice_args$seed.")
  }
  if (!is.null(mi_vars) && (!is.character(mi_vars) || anyNA(mi_vars) ||
                           anyDuplicated(mi_vars))) {
    stop("'mi_vars' must be NULL or unique variable names.")
  }
  reserved <- c("model", "data", "missing", "ordered", "group", "cluster",
                "sampling.weights", "FUN", "parallel", "omit.imps")
  used <- reserved[vapply(reserved, function(z) !is.null(sem_args[[z]]), logical(1))]
  if (length(used)) stop("Unsupported sem_args for MI: ", paste(used, collapse = ", "))
  if (!is.null(sem_args$estimator) &&
      !toupper(sem_args$estimator) %in% c("ML", "MLR")) {
    stop("MI quick functions currently support only ML and MLR.")
  }
  vars <- unique(unlist(lapply(lm_forms, function(z) all.vars(stats::as.formula(z)))))
  raw <- is.data.frame(data)
  imp <- NULL
  if (raw) {
    if (!requireNamespace("mice", quietly = TRUE)) stop("Install 'mice' to impute raw data.")
    selected <- unique(c(vars, mi_vars))
    if (anyDuplicated(names(data)) || !all(selected %in% names(data))) {
      stop("Raw data must contain unique column names and all model/mi_vars variables.")
    }
    imp_data <- data[, selected, drop = FALSE]
    if (!all(vapply(imp_data[vars], is.numeric, logical(1)))) {
      stop("MI quick model variables must be numeric; numeric binary predictors are allowed.")
    }
    # Extra variables enter imputation only; they do not adjust the path equations.
    args <- utils::modifyList(list(m = 20L, printFlag = progress), mice_args)
    args$data <- imp_data
    if (is.null(args$seed)) args$seed <- if (is.null(mi_seed)) seed else mi_seed
    if (is.null(args$seed)) args$seed <- NA
    imp <- do.call(mice::mice, args)
    datasets <- mice::complete(imp, action = "all")
    source <- "mice (generated internally)"
  } else if (inherits(data, "mids")) {
    if (!requireNamespace("mice", quietly = TRUE)) stop("Install 'mice' to read a mids object.")
    if (length(mice_args) || !is.null(mi_seed) || !is.null(mi_vars)) {
      stop("mice_args, mi_seed and mi_vars apply only to raw data.")
    }
    imp <- data
    datasets <- mice::complete(imp, action = "all")
    source <- "supplied mids object"
  } else if (is.list(data) && all(vapply(data, is.data.frame, logical(1)))) {
    if (length(mice_args) || !is.null(mi_seed) || !is.null(mi_vars)) {
      stop("mice_args, mi_seed and mi_vars apply only to raw data.")
    }
    datasets <- data
    source <- "supplied imputed data frames"
  } else stop("For MI, 'data' must be a data frame, mids object, or list of completed data frames.")
  if (length(datasets) < 2L) stop("MI requires at least two imputed data sets.")
  n <- nrow(datasets[[1]])
  valid <- vapply(datasets, function(d) {
    !anyDuplicated(names(d)) && all(vars %in% names(d)) && nrow(d) == n &&
      all(vapply(d[vars], is.numeric, logical(1))) &&
      all(vapply(d[vars], function(z) all(is.finite(z)), logical(1)))
  }, logical(1))
  if (!all(valid)) {
    stop("Each imputed data set must have the same case count and complete, finite numeric model variables.")
  }
  matrices <- lapply(datasets, function(d) mm_from_lm_forms(lm_forms, data = d))
  sem_model <- b_names_to_lavaan_model(matrices[[1]]$b_names)
  if (!all(vapply(matrices, function(z) {
    identical(z$b_names, matrices[[1]]$b_names)
  }, logical(1)))) stop("Model columns differ across imputations.")
  args <- utils::modifyList(sem_args,
             list(model = sem_model, data = lapply(matrices, function(z) z$model_matrix),
                  meanstructure = TRUE, fixed.x = fixed.x))
  fit <- withCallingHandlers(do.call(lavaan.mi::sem.mi, args),
      warning = function(w) {
        # Critical pooling methods are dispatched explicitly by manymome.
        # Avoid requiring users to attach a second package for one-call use.
        if (grepl("package:lavaan.mi.*not found", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      })
  # Match the omissions used by manymome's pooled covariance extraction.
  usable <- vapply(fit@convergence, function(z) isTRUE(z[1]) && isTRUE(z[2]), logical(1))
  if (sum(usable) < 2L) stop("Fewer than two imputations converged with standard errors.")
  if (!all(usable)) warning(sum(!usable), " imputation(s) omitted due to nonconvergence or unavailable SEs.")
  ci_out <- if (identical(ci_type, "mc")) {
    do_mc(fit = fit, R = R, seed = seed, progress = progress)
  } else NULL
  paths <- all_indirect_paths(fit, x = x, y = y, exclude = unique(unlist(cov)))
  effects <- function(paths) {
    settings <- list(ustd = c(FALSE, FALSE), stdx = c(TRUE, FALSE),
                     stdy = c(FALSE, TRUE), stdxy = c(TRUE, TRUE))
    lapply(settings, function(s) {
      many_indirect_effects(paths = paths, fit = fit, ci_type = ci_type,
                            ci_out = ci_out, standardized_x = s[1],
                            standardized_y = s[2], level = level, R = R,
                            seed = seed, parallel = parallel, ncores = ncores,
                            progress = progress)
    })
  }
  ind <- if (length(paths)) effects(paths) else list(ustd=NULL, stdx=NULL, stdy=NULL, stdxy=NULL)
  total <- lapply(ind, function(z) if (is.null(z)) NULL else total_indirect_effect(z, x=x, y=y))
  direct <- if (check_path(x=x, y=y, fit=fit)) {
    p <- list(list(x=x, y=y, m=NULL)); names(p) <- paste(x, "->", y)
    effects(p)
  } else list(ustd=NULL, stdx=NULL, stdy=NULL, stdxy=NULL)
  out <- list(lm_out=fit, lm_form=lm_forms, sem_model=sem_model,
              ind_out=ind, ind_total=total, dir_out=direct,
              cond_ind_out=list(ustd=NULL, stdx=NULL, stdy=NULL, stdxy=NULL),
              call=original_call, model=model, model_type=model_type,
              x=x, y=y, m=m, fit_method="lavaan.mi", missing="mi",
              fixed.x=fixed.x, ci_type=ci_type, indicators=NULL,
              imputation=imp, imputed_data=datasets,
              mi_info=list(source=source, requested=length(datasets),
                           used=sum(usable), omitted=which(!usable), n=n),
              sem_args=sem_args, mice_args=mice_args, mi_vars=mi_vars)
  subclass <- if (model_type == "standard") paste0("q_", model, "_mediation") else "q_user_mediation"
  class(out) <- c(subclass, "q_mediation", "list")
  out
}

#' @noRd
print_q_mi <- function(x, digits, annotation, pvalue, pvalue_digits, se,
                       for_each_path, ...) {
  cat("\n=== Mediation with multiple imputation ===\n\n")
  print(x$call)
  cat("\nImputation source:", x$mi_info$source,
      "\nCases per imputation:", x$mi_info$n,
      "\nImputations supplied:", x$mi_info$requested,
      "\nImputations used:", x$mi_info$used, "\n")
  if (length(x$mi_info$omitted)) cat("Omitted imputations:", x$mi_info$omitted, "\n")
  cat("\nPath model:\n", x$sem_model, "\n")
  cat("\nPooled path estimates (Rubin pooling; lavaan.mi):\n")
  pt <- lavaan.mi::parameterEstimates.mi(x$lm_out, omit.imps=c("no.conv", "no.se"))
  print(pt[pt$op == "~", , drop=FALSE], digits=digits)
  for (type in names(x$ind_out)) {
    if (!is.null(x$ind_out[[type]])) {
      cat("\nIndirect effects:", type, "\n")
      print(x$ind_out[[type]], digits=digits, annotation=annotation,
            pvalue=pvalue, pvalue_digits=pvalue_digits, se=se,
            for_each_path=for_each_path, ...)
      if (!is.null(x$ind_total[[type]])) {
        cat("\nTotal indirect effect:", type, "\n")
        print(x$ind_total[[type]], digits=digits, annotation=annotation,
              pvalue=pvalue, pvalue_digits=pvalue_digits, se=se, ...)
      }
    }
    if (!is.null(x$dir_out[[type]])) {
      cat("\nDirect effect:", type, "\n")
      print(x$dir_out[[type]], digits=digits, annotation=annotation,
            pvalue=pvalue, pvalue_digits=pvalue_digits, se=se, ...)
    }
  }
  cat("\nMI uses Monte Carlo intervals when requested; bootstrap is not supported.\n",
      "Pooled standardized path tables, R-square tests and quick plots are not yet provided.\n",
      "Inspect $imputation for mice diagnostics and $imputed_data for completed data.\n", sep="")
  invisible(x)
}
