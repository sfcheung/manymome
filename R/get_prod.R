#' @title Product Terms (if Any)
#' Along a Path
#'
#' @description Identify the product
#' term(s), if any, along a path in
#' a model and return the term(s),
#' with the variables involved and
#' the coefficient(s) of the term(s).
#'
#' @details
#' This function is used
#' by several functions in `manymome`
#' to identify product terms along
#' a path. If possible, this is done
#' by numerically checking whether a
#' column in a dataset is the product
#' of two other columns. If not possible
#' (e.g., the "product term" is the
#' "product" of two latent variables,
#' formed by the products of indicators),
#' then it requires the user to specify
#' an operator.
#'
#' The detailed workflow of this function
#' can be found in the
#' article [https://sfcheung.github.io/manymome/articles/get_prod.html](https://sfcheung.github.io/manymome/articles/get_prod.html)
#'
#' This function is not intended to be used
#' by users. It is exported such that
#' advanced users or developers can use
#' it.
#'
#' @param x Character. Variable name.
#'
#' @param y Character. Variable name.
#'
#' @param operator Character. The string
#' used to indicate a product term.
#' Default is `":"`, used in both [lm()]
#' and [lavaan::sem()] for observed
#' variables.
#'
#' @param fit The fit object. Currently
#' only supports a
#' [lavaan::lavaan-class] object.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [semTools::runMI()] or
#' its wrapper, such as [semTools::sem.mi()].
#'
#' @param est The output of
#' [lavaan::parameterEstimates()]. If
#' `NULL`, the default, it will be
#' generated from `fit`. If supplied,
#' `fit` will ge ignored.
#'
#' @param data Data frame (optional). If
#' supplied, it will be used to identify
#' the product terms.
#'
#' @param expand Whether products of
#' more than two terms will be searched.
#' `FALSE` by default.
#'
#' @return
#' If at least one product term is found,
#' it returns a list with these elements:
#'
#' - `prod`: The names of the product terms
#'    found.
#'
#' - `b`: The coefficients of these
#'    product terms.
#'
#' - `w`: The variable, other than
#'  `x`, in each product term.
#'
#' - `x`: The `x`-variable, that is,
#'  where the path starts.
#'
#' - `y`: The `y`-variable, that is,
#'  where the path ends.
#'
#' It returns `NA` if no product term
#' is found along the path.
#'
#' @examples
#'
#' dat <- modmed_x1m3w4y1
#' library(lavaan)
#' mod <-
#' "
#' m1 ~ x   + w1 + x:w1
#' m2 ~ m1  + w2 + m1:w2
#' m3 ~ m2
#' y  ~ m3  + w4 + m3:w4 + x + w3 + x:w3 + x:w4
#' "
#' fit <- sem(model = mod,
#'            data = dat,
#'            meanstructure = TRUE,
#'            fixed.x = FALSE)
#'
#' # One product term
#' get_prod(x = "x", y = "m1", fit = fit)
#' # Two product terms
#' get_prod(x = "x", y = "y", fit = fit)
#' # No product term
#' get_prod(x = "m2", y = "m3", fit = fit)
#'
#' @export

get_prod <- function(x,
                     y,
                     operator = ":",
                     fit = NULL,
                     est = NULL,
                     data = NULL,
                     expand = FALSE) {
    if (is.null(est)) {
      est <- lav_est(fit, se = FALSE, ci = FALSE)
    }
    all_prods <- NA
    if (inherits(fit, "lavaan") || inherits(fit, "lavaan.mi")) {
        all_prods <- find_all_products(lav_data_used(fit, drop_colon = FALSE),
                                       expand = expand)
        all_prods_names <- names(all_prods)
      }
    if (!is.null(data)) {
        all_prods <- find_all_products(data,
                                       expand = expand)
        all_prods_names <- names(all_prods)
      }
    if (!is.null(est$group) ||
        isTRUE(suppressWarnings(max(est$group) > 1))) {
        ngroups <- max(est$group)
      } else {
        ngroups <- 1
      }
    if (ngroups > 1) {

      }
    i_rhs <- (est$lhs == y) &
             (est$op == "~")
    if (isTRUE(any(i_rhs))) {
        y_rhs <- unique(est[i_rhs, "rhs"])
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
                                          est = est),
                     simplify = FALSE,
                     USE.NAMES = TRUE)
    # Handle multigroup models
    if (ngroups == 1) {
        b_prod <- unlist(b_prod)
      } else {
        b_prod <- b_prod
      }
    out <- list(prod = prod_x,
                b = b_prod,
                w = w,
                x = x,
                y = y)
    out
  }
