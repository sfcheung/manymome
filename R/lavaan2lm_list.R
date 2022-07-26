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
#' @param arg1 Argument description.
#' @param ... Additional arguments.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. Advance online publication. *Health Psychology*.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [functionname()]
#'
#' @family relatedfunctions
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
#' @noRd

lm_from_lavaan_list <- function(fit) {
    ptable <- lavaan::parameterTable(fit)
    # Get all dvs (ov.nox, lv.ox)
    dvs <- c(lavaan::lavNames(fit, "ov.nox"),
             lavaan::lavNames(fit, "lv.nox"))
    dat <- lavaan::lavInspect(fit, "data")
    # Get all ivs
    ivs_list <- sapply(dvs, lavaan_get_ivs,
                      ptable = ptable,
                      simplify = FALSE)
    # Get all betas
    bs_list <- sapply(dvs, lavaan_get_betas,
                      ivs_list = ivs_list,
                      ptable = ptable,
                      simplify = FALSE)
    # Get all intercepts
    int_list <- sapply(dvs, lavaan_get_intercepts,
                       ptable = ptable,
                       simplify = FALSE)
    # Get all product terms
    prods <- find_all_products(dat, expand = TRUE)
    # Update product terms
    ivs_list1 <- sapply(ivs_list, to_product,
                        prods = prods,
                        simplify = FALSE)
    # Update beta names
    bs_list1 <- mapply(function(x, y) {
                            names(y) <- x
                            y
                          },
                      x = ivs_list1, y = bs_list,
                      SIMPLIFY = FALSE,
                      USE.NAMES = TRUE)
    # Generate lm_like_object
    mods <- mapply(to_formula, dv = dvs, ivs = ivs_list1)
    # Return an lm_list object
    out <- mapply(lm_from_lavaan,
                  model = mods,
                  betas = bs_list1,
                  intercept = int_list,
                  SIMPLIFY = FALSE,
                  USE.NAMES = TRUE)
    class(out) <- c("lm_from_lavaan_list", class(out))
    out
  }

#' @noRd

lavaan_get_ivs <- function(dv, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "~"), ]
    ptable_iv0 <- ptable_dv$rhs
    ptable_iv0
  }

#' @noRd

lavaan_get_betas <- function(dv, ivs_list, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "~"), ]
    ivs <- ivs_list[[dv]]
    betas <- ptable_dv$est[which(ptable_dv$rhs %in% ivs)]
    names(betas) <- ivs
    betas
  }

#' @noRd

lavaan_get_intercepts <- function(dv, ptable) {
    ptable_dv <- ptable[(ptable$lhs == dv) & (ptable$op == "~1"), ]
    if (nrow(ptable_dv) == 0) {
        out <- 0
      } else {
        out <- ptable_dv[, "est"]
      }
    names(out) <- "(Intercept)"
    out
  }



#' @noRd

to_product <- function(ivs, prods) {
    prod_names <- names(prods)
    out <- ivs
    for (i in seq_along(ivs)) {
        if (ivs[i] %in% prod_names) {
            prod_comp <- prods[[which(ivs[i] == prod_names)]]
            out[i] <- paste0(prod_comp, collapse = ":")
          }
      }
    out
  }

#' @noRd

to_formula <- function(dv, ivs) {
    # TO-FIX: Handle intercept
    out <- paste(dv, "~", paste(ivs, collapse = " + "))
    stats::as.formula(out)
  }

#' @noRd

lm_from_lavaan <- function(model, betas, intercept) {
    out <- list(model = model,
                coefficients = betas,
                intercept = intercept)
    class(out) <- c("lm_from_lavaan", class(out))
    out
  }

#' @export

terms.lm_from_lavaan <- function(x, ...) {
    stats::terms(x$model)
  }

#' @export

coef.lm_from_lavaan <- function(object, ...) {
    c(object$intercept, object$coefficients)
  }

#' @export

predict.lm_from_lavaan <- function(object, newdata, ...) {
    tivs <- stats::delete.response(stats::terms(object))
    bs <- stats::coef(object)
    dat0 <- model.frame(tivs, newdata)
    mm <- stats::model.matrix(tivs, dat0) # No need for contrasts.arg
    p <- length(bs)
    out <- mm[, names(bs)] %*% matrix(bs, nrow = p, ncol = 1)
    unname(out[, 1])
  }

