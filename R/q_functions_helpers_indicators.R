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

