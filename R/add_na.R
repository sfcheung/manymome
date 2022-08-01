#' @noRd

add_na <- function(data, prop = .15, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)
    n <- nrow(data)
    nstar <- nrow(data) * ncol(data)
    i <- sample(nstar, round(nstar * prop))
    i <- lapply(i, function(x) c((x %% n) + 1, ceiling(x / n)))
    for (j in i) {
        data[j[1], j[2]] <- NA
      }
    data
  }

