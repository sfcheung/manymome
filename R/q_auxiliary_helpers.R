#' @noRd
check_q_auxiliary <- function(aux, data, fit_method, missing,
                              indicators, moderators, indicator_method,
                              sem_args) {
  if (is.null(aux)) return(invisible(NULL))
  if (!is.character(aux) || !length(aux) || anyNA(aux) ||
      any(!nzchar(aux)) || anyDuplicated(aux)) {
    stop("'aux' must be NULL or a nonempty character vector of unique variable names.")
  }
  if (fit_method != "lavaan" ||
      !missing %in% c("fiml", "ml", "fiml.x", "ml.x")) {
    stop("'aux' is supported only for SEM with FIML (fit_method = 'sem').")
  }
  if (!is.null(indicators) || !is.null(moderators) ||
      indicator_method == "sam") {
    stop("'aux' currently supports observed-variable models without indicators or moderators.")
  }
  if (!is.data.frame(data) || anyDuplicated(names(data)) ||
      !all(aux %in% names(data))) {
    stop("'data' must be a data frame with unique column names and all variables in 'aux'.")
  }
  if (any(make.names(aux) != aux)) {
    stop("Names in 'aux' must be syntactically valid R variable names.")
  }
  bad <- vapply(data[aux], function(z) {
    !is.numeric(z) || is.matrix(z) || any(is.infinite(z)) ||
      length(unique(z[!is.na(z)])) < 2L
  }, logical(1))
  if (any(bad)) {
    stop("Auxiliary variables must be numeric, finite when observed, and nonconstant: ",
         paste(aux[bad], collapse = ", "))
  }
  unsupported <- c("ordered", "group", "cluster", "sampling.weights")
  used <- unsupported[vapply(unsupported, function(z) {
    !is.null(sem_args[[z]]) && !identical(sem_args[[z]], FALSE)
  }, logical(1))]
  if (length(used)) {
    stop("'aux' does not currently support these sem_args: ",
         paste(used, collapse = ", "))
  }
  if (!is.null(sem_args$estimator) &&
      !toupper(sem_args$estimator) %in% c("ML", "MLR")) {
    stop("'aux' currently supports only ML and MLR estimators.")
  }
  invisible(NULL)
}

#' @noRd
add_q_auxiliary_data <- function(mm, aux, data) {
  modeled <- unique(unlist(lapply(mm$terms, all.vars)))
  if (any(!vapply(data[modeled], is.numeric, logical(1)))) {
    stop("With 'aux', model variables and covariates must also be numeric.")
  }
  overlap <- intersect(aux, c(modeled, names(mm$model_matrix)))
  if (length(overlap)) {
    stop("Variables in 'aux' must not also be model variables or covariates: ",
         paste(overlap, collapse = ", "))
  }
  # Preserve the original cases, including missing auxiliary values.
  rows <- match(rownames(mm$lm_all[[1]]$model), rownames(data))
  if (anyNA(rows) || length(rows) != nrow(data)) {
    stop("Cannot match auxiliary variables to the model's original cases.")
  }
  mm$model_matrix[aux] <- data[rows, aux, drop = FALSE]
  mm
}
