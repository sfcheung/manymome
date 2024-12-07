
#' @title Check the number of outcome variable
#'
#' @details
#' If only one outcome variable, return it.
#' If more than one outcome variable, return an error.
#' @noRd

get_one_response <- function(fit) {
  fit_type <- cond_indirect_check_fit(fit)
  switch(fit_type,
         lavaan = get_one_response_lavaan(fit = fit),
         lm = get_one_response_lm(fit = fit),
         lavaan.mi = get_one_response_lavaan_mi(fit = fit))
}

#' @noRd

get_one_response <- function(fit) {
  fit_type <- cond_indirect_check_fit(fit)
  # TODO
  # - Test get_one_response_lavaan_mi() after the issue
  #   with lavaan.mi object is fixed.
  switch(fit_type,
         lavaan = get_one_response_lavaan(fit = fit),
         lm = get_one_response_lm(fit = fit),
         lavaan.mi = get_one_response_lavaan_mi(fit = fit))
}

#' @noRd

get_one_response_lavaan <- function(fit) {
  ptable <- lavaan::parameterTable(fit)
  ys <- ptable[ptable$op == "~", "lhs"]
  ys <- unique(ys)
  if (length(ys) == 1) {
      return(ys)
    } else {
      stop("The fit object has more than one outcome variable.")
    }
}

#' @noRd

get_one_response_lm <- function(fit) {
  p <- length(fit)
  if (p == 1) {
    out <- get_response(fit[[1]])
    return(out)
  } else {
    stop("The fit object has more than one outcome variable.")
  }
}

#' @noRd

get_one_response_lavaan_mi <- function(fit) {
  ptable <- lavaan::parameterTable(fit)
  ys <- ptable[ptable$op == "~", "lhs"]
  ys <- unique(ys)
  if (length(ys) == 1) {
      return(ys)
    } else {
      stop("The fit object has more than one outcome variable.")
    }
}
