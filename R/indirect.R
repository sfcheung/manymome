#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param x Character. The name of predictor at the start of the pathway.
#' @param y Character. The name of the outcome variable at
#'          the end of the pathway.
#' @param m A vector of the variable names of the
#'          moderators. The pathway goes from the first
#'          mediator successively to the last mediator. If
#'          `NULL`, the default, the pathway goes from `x`
#'          to `y`.
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param est The output of [lavaan::parameterEstimates()]. If `NULL`, the
#'            default, it will be generated from `fit`. If supplied,
#'            `fit` will ge ignored.
#' @param wvalues A numeric vector of named elements. The names are the variable
#'                names of the moderators, and the values are the values to
#'                which the moderators will be set to. Default is `NULL`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#' @export
#'
#'

indirect <- function(x,
                     y,
                     m = NULL,
                     fit = NULL,
                     est = NULL,
                     wvalues = NULL) {
    if (is.null(est)) {
      est <- lavaan::parameterEstimates(fit)
    }
    # if (is.null(m)) {
    #     out <- get_b(x, y, fit)
    #     return(out)
    #   }
    y0 <- y
    p <- length(m) + 1
    bs <- rep(NA, p)
    xs <- c(x, m)
    ys <- c(m, y)
    bs <- mapply(get_b,
                 x = xs,
                 y = ys,
                 MoreArgs = list(est = est))
    bs_org <- bs
    prods <- mapply(get_prod,
                    x = xs,
                    y = ys,
                    MoreArgs = list(est = est),
                    SIMPLIFY = FALSE)
    names(prods) <- ys
    if (!is.null(wvalues)) {
        tmpfct <- function(xi) {
            if (all(is.na(xi))) return(0)
            b_i <- xi$b
            w_i <- xi$w
            wvalues_i <- wvalues[w_i]
            sum(b_i * wvalues_i)
          }
        b_cond <- sapply(prods, tmpfct)
        bs <- bs + b_cond
      } else {
        b_cond <- rep(NA, length(bs))
      }
    b_all <- prod(bs)
    names(b_all) <- "indirect"
    return(c(b_all, bs))
  }
