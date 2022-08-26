#' @title Extraction Methods for a
#' 'wlevels'-class Object
#'
#' @description For subsetting a
#' 'wlevels'-class object. Attributes
#' related to the levels will be
#' preserved if appropriate.
#'
#' @details Customized `[` for
#' 'wlevels'-class objects, to ensure
#' that these operations work as they
#' would be on a data frame object,
#' while information specific to a
#' `wlevels`-class object modified
#' correctly.
#'
#' The assignment methods `[<-`
#' and `[[<-` for
#' `wlevels`-class objects will raise an
#' error. This class of objects should
#' be created by [mod_levels()] or
#' related functions.
#'
#' Subsetting the output of
#' [mod_levels()] is possible but not
#' recommended. It is more reliable to
#' generate the levels using
#' [mod_levels()] and related functions.
#' Nevertheless, there are situations in
#' which subsetting is preferred.
#'
#' @return A 'wlevels'-class object. See
#' [mod_levels()] and
#' [merge_mod_levels()] for details on
#' this class.
#'
#' @param x A 'wlevels'-class object.
#'
#' @param i A numeric vector of row
#' number(s), a character vector of row
#' name(s), or a logical vector of
#' row(s) to be selected.
#'
#' @param j A numeric vector of column
#' number(s), a character vector of
#' column name(s), or a logical vector
#' of column(s) to be selected.
#'
#' @param drop Whether dropping a
#' dimension if it only have one
#' row/column.
#'
#' @param value Ignored.
#'
#' @seealso [mod_levels()],
#' [mod_levels_list()], and
#' [merge_mod_levels()]
#'
#'
#' @name subsetting_wlevels
NULL

#' @rdname subsetting_wlevels
#' @examples
#'
#' data(data_med_mod_ab)
#' dat <- data_med_mod_ab
#' # Form the levels from a list of lm() outputs
#' lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w2 + x + w1 + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' w1_levels <- mod_levels(lm_out, w = "w1")
#' w1_levels
#' w1_levels[2, ]
#' w1_levels[c(2, 3), ]
#'
#' dat <- data_med_mod_serial_cat
#' lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat)
#' lm_y <- lm(y ~ m1 + x + w1 + c1 + c2, dat)
#' lm_out <- lm2list(lm_m1, lm_y)
#' w1gp_levels <- mod_levels(lm_out, w = "w1")
#' w1gp_levels
#' w1gp_levels[2, ]
#' w1gp_levels[3, ]
#'
#' merged_levels <- merge_mod_levels(w1_levels, w1gp_levels)
#' merged_levels
#'
#' merged_levels[4:6, ]
#' merged_levels[1:3, c(2, 3)]
#' merged_levels[c(1, 4, 7), 1, drop = FALSE]
#'
#' @export

`[.wlevels` <-
    function(x, i, j, drop = if (missing(i)) TRUE else length(j) == 1)
  {
    out <- NextMethod()
    if (is.null(dim(out))) {
        if (!missing(j)) {
            if (missing(i)) {
                rnames <- rownames(attr(x, "wlevels"))
              } else {
                rnames <- rownames(attr(x, "wlevels"))[i]
              }
            names(out) <- rnames
            return(out)
          } else {
            out <- data.frame(out)
            colnames(out) <- colnames(x)
            if (is.character(i)) {
                i0 <- match(i, rownames(x))
              } else {
                i0 <- i
              }
            rownames(out) <- rownames(x)[i]
          }
      }
      cnames <- colnames(out)
      if (!check_gp_columns(x, cnames)) {
          stop("Indicators of group moderators should be selected together.")
        }
      wvars_out <- wvars_from_columns(x, out)
      wvars_x <- attr(x, "wvars")
      if (!is.null(wvars_x)) {
          attr(out, "wvars") <- attr(x, "wvars")[wvars_out]
        }
      wname_x <- attr(x, "wname")
      if (!is.null(wname_x)) {
          attr(out, "wname") <- wname_x
        }
      i_out <- match(rownames(out), rownames(x))
      wlevels_out <- attr(x, "wlevels")[i_out, , drop = FALSE]
      attr(out, "wlevels") <- wlevels_out
      w_type_x <- attr(x, "w_type")
      if (length(w_type_x) == 1) {
          attr(out, "w_type") <- w_type_x
        } else {
          attr(out, "w_type") <- w_type_x[wvars_out]
        }
      class0 <- class(out)
      class0 <- class0[!(class0 %in% "wlevels")]
      class(out) <- c("wlevels", class0)
      return(out)
  }

#' @rdname subsetting_wlevels
#' @export

`[<-.wlevels` <- function(x, i, j, value) {
    stop("Changing values of a 'wlevels'-object is not allowed.")
  }

#' @rdname subsetting_wlevels
#' @export

`[[<-.wlevels` <- function(x, i, j, value) {
    stop("Changing values of a 'wlevels'-object is not allowed.")
  }

#' @noRd

check_gp_columns <- function(x, i) {
    if (missing(i)) return(FALSE)
    wvars <- attr(x, "wvars")
    if (is.null(wvars)) {
        wvars <- list(colnames(x))
        names(wvars) <- attr(x, "wname")
      }
    if (is.character(i)) {
        i <- match(i, colnames(x))
        if (any(is.na(i))) {
            stop("At least one column name is invalid.")
          }
      }
    cnames <- colnames(x)[i]
    if (any(is.na(cnames))) {
        stop("At least one column number is invalid.")
      }
    tmp <- sapply(wvars, function(x) {
                if (any(x %in% cnames)) {
                    return(all(x %in% cnames))
                  } else {
                    return(TRUE)
                  }
              })
    if (all(tmp)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

#' @noRd

wvars_from_columns <- function(x, y) {
    wvars <- attr(x, "wvars")
    if (is.null(wvars)) {
        wvars <- colnames(x)
      }
    cnames <- colnames(y)
    tmp <- sapply(wvars, function(xx) {
                if (any(xx %in% cnames)) {
                    return(all(xx %in% cnames))
                  } else {
                    return(FALSE)
                  }
              })
    names(tmp)[tmp]

  }
