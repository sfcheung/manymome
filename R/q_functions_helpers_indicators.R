#' @noRd
# Check whether all indicators are in the data
# Check whether all factors are NOT in the data

check_indicators <- function(
  indicators,
  data
) {
  indicators <- strip_minus(indicators)
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
# Remove leading negative signs
strip_minus <- function(indicators) {
  out <- sapply(
    indicators,
    function(x) {
        gsub("^-", "", x)
      },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  out
}

#' @noRd
# Select reverse items
reverse_indicators <- function(
  indicators
) {
  out <- sapply(
    indicators,
    function(x) {
        grepv("^-", x)
      },
    USE.NAMES = TRUE,
    simplify = FALSE
  )
  out2 <- strip_minus(out)
  out2
}


#' @noRd
# Generate the model syntax for measurement part

measurement_syntax <- function(
  indicators,
  collapse = TRUE
) {
  indicators <- strip_minus(indicators)
  out0 <- lapply(
              indicators,
              function (x) {paste0(x, collapse = " + ")}
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
# Reverse scores
reverse_scores <- function(
  x,
  data
) {
  for (xx in x) {
    data <- reverse_scores_i(x, data = data)
  }
  data
}
reverse_scores_i <- function(
  x,
  data
) {
  # TODO:
  # - Do reverse coding
  #   Not urgent: No impact on the coefficients
  data[, x] <- -data[, x, drop = TRUE]
  data
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
  indicators_org <- indicators
  indicators <- strip_minus(indicators)
  # ==== Process reverse indicators, if any ====
  indicators_rev <- reverse_indicators(indicators_org)
  indicators_rev <- unlist(indicators_rev)

  # ==== Reverse scores ====
  if (length(indicators_rev) > 0) {
    data <- reverse_scores(
              indicators_rev,
              data = data
            )
  }

  # ==== Process arguments ====

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
            FUN = function (x) {
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