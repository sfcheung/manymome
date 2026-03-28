#' @noRd
# Check whether all indicators are in the data
# Check whether all factors are NOT in the data

check_indicators <- function(
  indicators,
  data
) {
  all_vars <- unname(unlist(indicators))
  inds_not_in_dat <- setdiff(all_vars, colnames(data))
  if (length(inds_not_in_dat) > 0) {
    stop("Some indicators no in the data: ",
         paste0(inds_not_in_dat, collapse = ","))
  }
  all_lvs <- names(indicators)
  lvs_in_dat <- intersect(all_lvs, colnames(data))
  if (length(lvs_in_dat) > 0) {
    stop("'Factors' should not be in the data: ",
         paste0(lvs_in_dat, collapse = ","))
  }
  TRUE
}

#' @noRd
# Generate the model syntax for measurement part

measurement_syntax <- function(
  indicators,
  collapse = TRUE
) {
  out0 <- lapply(
              indicators,
              \(x) paste0(x, collapse = " + ")
            )
  out1 <- paste0(
            names(out0),
            " =~ ",
            out0
          )
  if (collapse) {
    out1 <- paste0(out1, collapse = "\n")
  }
  out1
}

#' @noRd
# Compute scale scores
scale_scores <- function(
  indicators,
  data,
  score_fun = mean,
  score_args = list()
) {
  # Repeat score_fun if necessary
  # Repeat score_args if necessary
  fnames <- names(indicators)
  if (!setequal(names(score_args), fnames)) {
    # score_args is for all factors
    score_args <- replicate(
                    length(fnames),
                    score_args,
                    simplify = FALSE
                  )
    names(score_args) <- fnames
  } else {
   # score_args for each factor
    score_args <- score_args[fnames]
  }
  if (!setequal(names(score_fun), fnames)) {
    # score_args is for all factors
    score_fun <- replicate(
                    length(fnames),
                    score_fun,
                    simplify = FALSE
                  )
    names(score_fun) <- fnames
  } else {
   # score_args for each factor
    score_fun <- score_fun[fnames]
  }
  f <- function(
        xx,
        score_fun,
        score_args
      ) {
    datxx <- data[, xx, drop = FALSE]
    out <- apply(
            datxx,
            MARGIN = 1,
            FUN = \(x) {
                if (all(is.na(x))) {
                  return(NA)
                }
                do.call(
                  score_fun,
                  c(list(x), score_args)
                )
              }
            )
    out
  }
  out0 <- mapply(
            FUN = f,
            xx = indicators,
            score_fun = score_fun,
            score_args = score_args,
            SIMPLIFY = FALSE
          )
  data.frame(out0, check.names = FALSE)
}