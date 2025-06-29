# Functions developed for q_* functions.

#' @noRd
# Check if all variables in a model is in the dataset.
# Input:
# - A character vector of formulas presented as strings.
# - A data frame.
# Output:
# - Logical. Whether all variables are in the dataset.

check_vars_lm <- function(formulas,
                          data) {
  my_call <- match.call()
  # Some lines adapted from lmhelprs::many_lm()
  all_vars <- sapply(formulas,
                     function(x) {
                        all.vars(stats::as.formula(x))
                     },
                     USE.NAMES = FALSE,
                     simplify = FALSE)
  all_vars <- unique(unlist(all_vars))
  data_to_use <- eval(my_call$data,
                      envir = parent.frame())
  data_vars <- colnames(data_to_use)
  if (all(all_vars %in% data_vars)) {
    return(TRUE)
  } else {
    out <- FALSE
    not_found <- setdiff(all_vars, data_vars)
    attr(out, "not_in_data") <- not_found
    return(out)
  }
}

#' @noRd
# Do listwise selection based on the
# variables used in a model.
# Input:
# - A character vector of formulas presented as strings.
# - A data frame.
# Output:
# - An integer vector to cases to remove. If no case removed,
#   it ia a zero-length integer vector

lm_listwise <- function(formulas,
                        data) {
  my_call <- match.call()
  # Some lines adapted from lmhelprs::many_lm()
  all_vars <- sapply(formulas,
                     function(x) {
                        all.vars(stats::as.formula(x))
                     },
                     USE.NAMES = FALSE,
                     simplify = FALSE)
  all_vars <- unique(unlist(all_vars))

  # All variables in the dataset?
  all_in_data <- check_vars_lm(formulas = formulas,
                               data = data)
  if (isFALSE(all_in_data)) {
    not_in_data <- attr(all_in_data, "not_in_data")
    stop(paste(not_in_data, collapse = ","),
         " in the model but not in the dataset.")
  }
  data_listwise <- stats::na.omit(eval(my_call$data,
                                  envir = parent.frame())[, all_vars, drop = FALSE])
  data_omitted <- attr(data_listwise,
                       "na.action")
  if (!is.null(data_omitted)) {
      omitted_listwise <- as.integer(data_omitted)
    } else {
      omitted_listwise <- integer(0)
    }
  return(omitted_listwise)
}


#' @noRd
# Input:
# - paths: Of one of the following forms
# c("x1 -> m11 -> m12 -> y1",
#   "x1 -> m2 -> y1")
# OR
# list(
#   c("x1", "m11"),
#   c("m11", "m12", "y1"),
#   c("m11", "m2", "y1")
# )
# Output:
# A named list of predictors
paths_to_models <- function(paths) {
  if (!is.list(paths)) {
    paths <- parse_paths(paths)
  }
  all_vars <- unique(unlist(paths))
  from_to_all <- lapply(paths,
                        to_direct)
  from_to_all <- do.call(rbind,
                         from_to_all)
  from_to_all <- unique(from_to_all)
  to_vars <- unique(from_to_all[, "to"])
  out0 <- sapply(to_vars,
                 function(x) {
                  i <- (from_to_all[, "to"] == x)
                  out <- from_to_all[i, "from", drop = TRUE]
                  unname(out)
                 },
                 simplify = FALSE)
  out0
}

#' @noRd
# Input:
# - x: One character vector of this form
# "x1 -> m11 -> m12 -> y1"
# - Output:
# - A character vector of this form:
# c("x1", "m11", "m12", "y1"),
parse_paths <- function(x) {
  out0 <- strsplit(x,
                   "->",
                   fixed = TRUE)
  out0 <- lapply(out0,
                 function(x) trimws(x))
  out0
}

#' @noRd
# Input:
# - A character vector of this form:
# c("x1", "m11", "m12", "y1")
# Output:
# A matrix of this form:
# "x1" "m11"
# "m11" "m12"
# "m12" "y1"
to_direct <- function(x) {
  if (length(x) == 2) {
    out <- matrix(x,
                  nrow = 1,
                  ncol = 2)
    colnames(out) <- c("from", "to")
    return(out)
  }
  x_from <- x[-length(x)]
  x_to <- x[-1]
  out <- cbind(from = x_from,
               to = x_to)
  out
}

#' @noRd
# Input:
# - from_to: A named list of iv names
# - cov: A character vector or a named list of vectors
# # Output:
# - A named vector of lm formulas
form_models_paths <- function(from_to,
                              cov = NULL) {
  from_to_new <- from_to
  dvs <- names(from_to_new)
  if (!is.null(cov)) {
    if (is.list(cov)) {
      for (xx in names(cov)) {
        if (xx %in% dvs) {
          from_to_new[[xx]] <- c(from_to_new[[xx]], cov[[xx]])
        }
      }
    } else {
      for (xx in dvs) {
        from_to_new[[xx]] <- c(from_to_new[[xx]], cov)
      }
    }
  }
  f0 <- function(y, x) {
          out0 <- paste0(x,
                         collapse = " + ")
          out1 <- paste0(y,
                         " ~ ",
                         out0)
          out1
        }
  out0 <- mapply(f0,
                 y = dvs,
                 x = from_to_new,
                 SIMPLIFY = TRUE,
                 USE.NAMES = TRUE)
  out0
}