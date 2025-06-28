skip("WIP")

skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: model", {

#' @noRd
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
parse_paths <- function(x) {
  out0 <- strsplit(x,
                   "->",
                   fixed = TRUE)
  out0 <- lapply(out0,
                 function(x) trimws(x))
  out0
}
#' @noRd
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
# - A named list of lm fromulas
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
                 SIMPLIFY = FALSE,
                 USE.NAMES = TRUE)
  out0
}

tmp1 <- paths_to_models(c("x1 -> m11 -> m12 -> y1",
                          "x1 -> m2 -> y1"))
tmp2 <- paths_to_models(list(
                            c("x1", "m11"),
                            c("m11", "m12", "y1"),
                            c("m11", "m2", "y1")
                          ))

out1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> m2 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )

out2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = list(c("x1", "m11"),
                         c("m11", "m12", "y1"),
                         c("m11", "m2", "y1")),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )

})

