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

#' @noRd
scale_reliability <- function(
  indicators,
  data,
  method = c("sem", "omega")
) {
  method <- match.arg(method)
  # TODO:
  # - Allow additional arguments
  scale_reliability_i <- switch(
    method,
    sem = scale_reliability_i_sem,
    omega = scale_reliability_i_omega
  )
  out0 <- sapply(
    indicators,
    FUN = scale_reliability_i,
    data = data,
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  out1a <- sapply(out0,
              function(x) x$reliability
            )
  out1b <- lapply(out0,
              function(x) x$full_output
            )
  out1c <- lapply(out0,
              function(x) x$loadings
            )
  out1 <- list(
            reliability = out1a,
            full_output = out1b,
            loadings = out1c
          )
  out1
}

#' @noRd
scale_reliability_i_sem <- function(
  indicators_i,
  data,
  rel_args = list(),
  cfa_args = list()
) {

  # ==== Get reverse indicators ====

  ind <- strip_minus(list(indicators_i))[[1]]
  ind_rev <- reverse_indicators(list(indicators_i))[[1]]
  ind_key <- vector("integer", length(ind))
  ind_key[] <- 1
  names(ind_key) <- ind
  ind_key[ind_rev] <- -1

  dat_i <- data[, ind, drop = FALSE]

  # ==== Reverse scores ====

  if (length(ind_rev) > 0) {
    dat_i <- reverse_scores(
              ind_rev,
              data = dat_i
            )
  }

  # ==== Fit CFA ====

  # TODO:
  # - Allow additional arguments
  # - Handle two-item scale
  # - Handle ordinal variables
  if (length(ind) == 2) {
    mod <- paste0("f =~ ",
                paste0("a*", ind, collapse = " + "))
  } else {
    mod <- paste0("f =~ ",
                paste0(ind, collapse = " + "))
  }
  cfa_args0 <- list(
      std.lv = TRUE,
      missing = "listwise"
    )
  cfa_args1 <- utils::modifyList(
                  cfa_args0,
                  cfa_args
                )
  fit <- tryCatch(do.call(
            lavaan::cfa,
            c(list(model = mod,
                   data = dat_i),
              cfa_args1)
          ), error = function(e) e)

  fit_ok <- inherits(fit, "lavaan")
  if (fit_ok) {
    fit_ok <- isTRUE(suppressWarnings(
                lavaan::lavInspect(fit, "post.check")
              ))
  }

  # ==== Store the loadings ====

  if (fit_ok) {
    loadings <- methods::getMethod("coef",
                      signature = "lavaan",
                      where = asNamespace("lavaan"))(fit)
    i <- grepl("^f=~", names(loadings))
    loadings <- loadings[i]
    names(loadings) <- gsub("^f=~", "", names(loadings))
    loadings <- loadings[ind]
  } else {
    loadings <- rep(NA, length(ind))
    names(loadings) <- ind
  }

  # ==== Compute reliability ====

  if (fit_ok) {
    out0 <- tryCatch(do.call(semTools::compRelSEM,
              c(list(fit,
                    simplify = TRUE),
                rel_args)
            ), error = function(e) e)
  } else {
    out0 <- NA
  }

  # ==== Prepare the output ====

  if (inherits(out0, "error0") ||
      is.na(out0)) {
    reliability <- NA
  } else {
    reliability <- as.numeric(out0)
  }
  out1 <- list(
      reliability = reliability,
      full_output = out0,
      loadings = loadings
    )
  out1
}

#' @noRd
scale_reliability_i_omega <- function(
  indicators_i,
  data
) {
  ind <- strip_minus(list(indicators_i))[[1]]
  ind_rev <- reverse_indicators(list(indicators_i))[[1]]
  ind_key <- vector("integer", length(ind))
  ind_key[] <- 1
  names(ind_key) <- ind
  ind_key[ind_rev] <- -1
  dat_i <- data[, ind, drop = FALSE]
  # TODO:
  # - Allow additional arguments
  out0 <- suppressMessages(suppressWarnings(
    tryCatch(psych::omega(
      m = dat_i,
      nfactors = 1,
      key = ind_key
    ), error = function(e) e)
  ))
  if (inherits(out0, "error")) {
    reliability <- NA
    out0 <- NA
  } else {
    reliability <- out0$omega.tot
  }
  out1 <- list(
      reliability = reliability,
      full_output = out0
    )
  out1
}