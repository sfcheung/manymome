#' @title Extraction Methods for 'cond_indirect_effects'
#'
#' @description For subsetting a 'cond_indirect_effects'-class object.
#'
#' @details Customized `[` and `[[` for 'cond_indirect_effects'-class objects.
#'
#' @return A 'cond_indirect_effects'-class object.
#'
#' @param x A 'cond_indirect_effects'-class object.
#' @param i A numeric vector of row number(s),
#'          a character vector of row name(s),
#'          or a logical vector of row(s) to be selected.
#' @param j A numeric vector of column number(s),
#'          a character vector of column name(s), or
#'          a logical vector of column(s) to be selected.
#' @param drop Whether dropping a dimension if it only have one row/column.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @name subsetting_cond_indirect_effects
NULL

#' @rdname subsetting_cond_indirect_effects
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ x  + w1 + x:w1
#' m2 ~ m1
#' y  ~ m2 + x + w4 + m2:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' # Examples for cond_indirect():
#'
#' # Create levels of w1 and w4
#' w1levels <- mod_levels("w1", fit = fit)
#' w1levels
#' w4levels <- mod_levels("w4", fit = fit)
#' w4levels
#' w1w4levels <- merge_mod_levels(w1levels, w4levels)
#'
#' # Conditional effects from x to m1 when w1 is equal to each of the levels
#' out1 <- cond_indirect_effects(x = "x", y = "m1",
#'                       wlevels = w1levels, fit = fit)
#' out1[2, ]
#'
#' # Conditional Indirect effect from x1 through m1 to y,
#' # when w1 is equal to each of the levels
#' out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
#'                       wlevels = w1w4levels, fit = fit)
#' out2[c(1, 3), ]
#'
#' @export

`[.cond_indirect_effects` <-
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
            return(data.frame(out))
          }
      } else {
        fo <- attr(x, "full_output")[i]
        attr(out, "full_output") <- fo
        wl <- attr(x, "wlevels")[i, ]
        attr(wl, "wlevels") <- attr(wl, "wlevels")[i, ]
        attr(out, "wlevels") <- wl
        return(out)
      }
  }