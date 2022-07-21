#' @title One Line Title
#'
#' @description Find the product term of two variables in a model.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param x Character. Variable name.
#' @param y Character. Variable name.
#' @param operator Character. The string used to indicate a product term.
#'                 Default is `":"`, used in both [lm()] and [lavaan::sem()]
#'                 for observed variables.
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`, the
#'            default, it will be generated from `fit`. If supplied,
#'            `fit` will ge ignored.
#'
#' @examples
#' \donttest{
#' }
#' @noRd
#'
#'

get_prod <- function(x,
                     y,
                     operator = ":",
                     fit = NULL,
                     est = NULL) {
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    if (inherits(fit, "lavaan")) {
        all_prods <- find_all_products(lavaan::lavInspect(fit, "data"))
        all_prods_names <- names(all_prods)
      } else {
        all_prods <- NA
      }
    i_rhs <- (est$lhs == y) &
             (est$op == "~")
    if (isTRUE(any(i_rhs))) {
        y_rhs <- est[i_rhs, "rhs"]
      } else {
        return(NA)
      }
    if (!identical(all_prods, NA)) {
        i_prod_rhs <- y_rhs %in% all_prods_names
      } else {
        i_prod_rhs <- grepl(operator, y_rhs)
      }
    if (isTRUE(any(i_prod_rhs))) {
        prod_rhs <- y_rhs[i_prod_rhs]
      } else {
        return(NA)
      }
    if (!identical(all_prods, NA)) {
        tmp <- all_prods[prod_rhs]
        i_prod_x1 <- sapply(unname(tmp), function(xx) x %in% xx)
        if (isTRUE(any(i_prod_x1))) {
            prod_x1 <- prod_rhs[i_prod_x1]
            tmp2 <- tmp[i_prod_x1]
            w1 <- sapply(unname(tmp2), function(xx) {
                              xx[!(xx %in% x)]
                            })
            prod_x <- prod_x1
            w <- w1
          } else {
            prod_x <- NULL
            w <- NULL
          }
      } else {
        i_prod_x1 <- grepl(paste0(x, operator), prod_rhs)
        i_prod_x2 <- grepl(paste0(operator, x), prod_rhs)
        if (isTRUE(any(i_prod_x1))) {
            prod_x1 <- prod_rhs[i_prod_x1]
            w1 <- gsub(paste0(x, operator), "", prod_x1)
          } else {
            prod_x1 <- NULL
            w1 <- NULL
          }
        if (isTRUE(any(i_prod_x2))) {
            prod_x2 <- prod_rhs[i_prod_x2]
            w2 <- gsub(paste0(operator, x), "", prod_x2)
          } else {
            prod_x2 <- NULL
            w2 <- NULL
          }
        prod_x <- c(prod_x1, prod_x2)
        w <- c(w1, w2)
      }
    b_prod <- sapply(prod_x, function(x) get_b(x = x,
                                          y = y,
                                          est = est))
    out <- list(prod = prod_x,
                b = b_prod,
                w = w,
                x = x,
                y = y)
    out
  }
