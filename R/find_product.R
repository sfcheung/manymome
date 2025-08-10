
#' @title Find Product Terms
#'
#' @description Find columns in a data
#' frame or matrix that are the products
#' of two or more other columns.
#'
#' @details This function finds probable
#' product terms by comparing its values
#' to the products of other columns. It
#' was developed for identifying columns
#' that are the products of two columns.
#' It may be able to identify columns
#' that are the products of three or
#' more columns but there is no
#' guarantee.
#'
#' @return [find_all_products()] returns
#' a named list. For each element, the
#' name is the column name of a product
#' term, and the content is a vector of
#' the names of the columns used for
#' form the product term. If no column
#' is a product of other column, it
#' returns a names list of zero length.
#'
#' [find_product()] returns a vector of
#' two elements. If the target column is
#' a product of two other columns, this
#' vector contains the names of these
#' two columns. Otherwise, this is a
#' vector of `NA`s.
#'
#' @param data The data frame to be
#' searched.
#'
#' @param target The column to be
#' checked if it is a product of other
#' columns.
#'
#' @param expand Whether the function
#' will attempt to expand a lower order
#' term to their components. Default is
#' `TRUE`.
#'
#'@noRd

find_product <- function(data, target) {
    if (is.list(data) && !is.data.frame(data)) {
        ngroups <- length(data)
        # Aasume all groups have the same variables
        data <- do.call(rbind, data)
      } else {
        ngroups <- 1
      }
    a_col <- data[, target]
    out <- c(NA, NA)
    q <- 0
    for (i in colnames(data)) {
        if (!is.vector(data[, i], mode = "numeric")) next
        for (j in colnames(data)) {
            q <- q + 1
            if (!is.vector(data[, j], mode = "numeric")) next
            xy <- data[, i] * data[, j]
            target_xy <- all.equal(a_col, xy)
            if (isTRUE(target_xy)) {
              out <- c(i, j)
              break
              }
          }
        if (isTRUE(target_xy)) break
      }
    out
  }

#'@noRd

find_all_products <- function(
                      data,
                      expand = TRUE,
                      skip_indicators = TRUE,
                      fit = NULL) {
    if (is.list(data) && !is.data.frame(data)) {
        ngroups <- length(data)
        data <- do.call(rbind, data)
      } else {
        ngroups <- 1
      }
    if (skip_indicators &&
        (inherits(fit, "lavaan") ||
         inherits(fit, "lavaan.mi"))) {
      ov_ind <- lavaan::lavNames(
                    fit,
                    type = "ov.ind")
      tmp <- setdiff(colnames(data), ov_ind)
      data <- data[, tmp]
      if (ncol(data) < 2) {
        return(list())
      }
    }
    out <- sapply(colnames(data),
                  find_product, data = data,
                  USE.NAMES = TRUE,
                  simplify = FALSE)
    out <- out[sapply(out, function(x) !all(is.na(x)))]
    # Remove variables that are a product of themselves
    if (length(out) > 0) {
        out <- out[sapply(out, function(x) x[1] != x[2])]
      }
    # Remove a * b == a
    if (length(out) > 0) {
        tmpfct <- function(xy, x) {
              !isTRUE(x %in% xy)
            }
        out <- out[mapply(FUN = tmpfct,
                          xy = out,
                          x = names(out))]
      }
    if (expand) {
        out <- expand2lower(out)
      }
    out
  }

#'@noRd

expand2lower_i <- function(x, full_list) {
    to_append <- NULL
    for (y in seq_len(length(x))) {
        if (x[y] %in% names(full_list)) {
            to_append <- c(to_append, full_list[[x[y]]])
            x <- x[-y]
          }
      }
    c(x, to_append)
  }

#'@noRd

expand2lower <- function(full_list) {
    out <- full_list
    while (any(unlist(out) %in% names(full_list))) {
        out <- sapply(out, expand2lower_i, full_list = out)
      }
    out
  }