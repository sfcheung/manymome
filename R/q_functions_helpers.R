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
