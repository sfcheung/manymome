#' @title Convert a 'lavaan' Model to an 'lm_from_lavaan_list'-Class Object
#'
#' @description Converts the regression models in a `lavaan`` model to an
#'   `lm_from_lavaan_list`-class object.
#'
#' @details It identifies all dependent variables in a `lavaan` model
#'   and creates a `lm_from_lavaan`-class object for each of them.
#'
#' @return
#' A `lm_from_lavaan_list`-class object, which is a list of `lm_from_lavaan`
#' objects. It has a `predict`-method ([predict.lm_from_lavaan_list()]) for
#' computing the predicted values from one variable to another.
#'
#' @param fit A `lavaan`-class object, usually the output of [lavaan::lavaan()]
#'            or its wrappers.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#' library(lavaan)
#' data(data_med)
#' mod <-
#' "
#' m ~ a * x + c1 + c2
#' y ~ b * m + x + c1 + c2
#' "
#' fit <- sem(mod, data_med, fixed.x = FALSE)
#' fit_list <- lm_from_lavaan_list(fit)
#' tmp <- data.frame(x = 1, c1 = 2, c2 = 3, m = 4)
#' predict(fit_list, x = "x", y = "y", m = "m", newdata = tmp)
#'
#' @export


lm_from_lavaan_list <- function(fit) {
    ptable <- lavaan::parameterTable(fit)
    # Get all dvs (ov.nox, lv.ox)
    # dvs <- c(lavaan::lavNames(fit, "ov.nox"),
    #          lavaan::lavNames(fit, "lv.nox"))
    dvs <- lavaan_get_dvs(ptable)
    # dat <- lavaan::lavInspect(fit, "data")
    dat <- lav_data_used(fit)
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

#' @title Model Terms of a 'lm_from_lavaan'-Class Object
#'
#' @description Extract the terms object rom a `lm_from_lavaan`-class
#'  object.
#'
#' @details A `lm_from_lavaan`-class object converts a regression model
#'  for a variable in a `lavaan`` model to a `formula` object.
#'  This function simply calls [stats::terms()] on the `formula` object
#'  to extract the predictors of a variable.
#'
#' @return
#' A `terms`-class object. See [terms.object] for details.
#'
#' @param x A 'lm_from_lavaan'-class object.
#' @param ... Additional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [terms.object], [lm_from_lavaan_list()]
#'
#' @examples
#' library(lavaan)
#' data(data_med)
#' mod <-
#' "
#' m ~ a * x + c1 + c2
#' y ~ b * m + x + c1 + c2
#' "
#' fit <- sem(mod, data_med, fixed.x = FALSE)
#' fit_list <- lm_from_lavaan_list(fit)
#' terms(fit_list$m)
#' terms(fit_list$y)
#'
#' @export

terms.lm_from_lavaan <- function(x, ...) {
    stats::terms(x$model)
  }

#' @title Coefficients of a 'lm_from_lavaan'-Class Object
#'
#' @description Returns the path coefficients of the terms in
#'  a `lm_from_lavaan`-class object.
#'
#' @details A `lm_from_lavaan`-class object converts a regression model
#'  for a variable in a `lavaan`` model to a `formula` object.
#'  This function simply extracts the path coefficients estimates.
#'  Intercept is always included, and set to zero if mean structure
#'  is not in the source `lavaan` model.
#'
#' @return
#' A numeric vector of the path coefficients.
#'
#' @param object A 'lm_from_lavaan'-class object.
#' @param ... Additional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_from_lavaan_list()]
#'
#' @examples
#' library(lavaan)
#' data(data_med)
#' mod <-
#' "
#' m ~ a * x + c1 + c2
#' y ~ b * m + x + c1 + c2
#' "
#' fit <- sem(mod, data_med, fixed.x = FALSE)
#' fit_list <- lm_from_lavaan_list(fit)
#' coef(fit_list$m)
#' coef(fit_list$y)
#'
#' @export

coef.lm_from_lavaan <- function(object, ...) {
    c(object$intercept, object$coefficients)
  }

#' @title Predicted Values of a 'lm_from_lavaan'-Class Object
#'
#' @description Compute the predicted values based on
#'  the model stored in a 'lm_from_lavaan`-class object.
#'
#' @details A `lm_from_lavaan`-class object converts a regression model
#'  for a variable in a `lavaan`` model to a `formula` object.
#'  This function uses the stored model to compute predicted
#'  values using user-supplied data.
#'
#' @return
#' A numeric vector of the predicted values, with length equal to
#' the number of rows of user-supplied data.
#'
#' @param object A 'lm_from_lavaan'-class object.
#' @param newdata Required. A data frame of the new data. It must be a
#' data frame.
#' @param ... Additional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_from_lavaan_list()]
#'
#' @examples
#' library(lavaan)
#' data(data_med)
#' mod <-
#' "
#' m ~ a * x + c1 + c2
#' y ~ b * m + x + c1 + c2
#' "
#' fit <- sem(mod, data_med, fixed.x = FALSE)
#' fit_list <- lm_from_lavaan_list(fit)
#' tmp <- data.frame(x = 1, c1 = 2, c2 = 3, m = 4)
#' predict(fit_list$m, newdata = tmp)
#' predict(fit_list$y, newdata = tmp)
#'
#' @export

predict.lm_from_lavaan <- function(object, newdata, ...) {
    if (missing(newdata)) {
        stop("This method for lm_from_lavaan requires new data.")
      }
    tivs <- stats::delete.response(stats::terms(object))
    bs <- stats::coef(object)
    dat0 <- stats::model.frame(tivs, newdata)
    mm <- stats::model.matrix(tivs, dat0) # No need for contrasts.arg
    p <- length(bs)
    out <- mm[, names(bs)] %*% matrix(bs, nrow = p, ncol = 1)
    unname(out[, 1])
  }

#' @title Predicted Values of a 'lm_from_lavaan_list'-Class Object
#'
#' @description Compute the predicted values based on
#'  the models stored in a 'lm_from_lavaan_list`-class object.
#'
#' @details A `lm_from_lavaan_list`-class object is a list
#'  of `lm_from_lavaan`-class objects.
#'
#' @return
#' A numeric vector of the predicted values, with length equal to
#' the number of rows of user-supplied data.
#'
#' @param object A 'lm_from_lavaan'-class object.
#' @param x The variable name at the start of a pathway.
#' @param y The variable name at the end of a pathway.
#' @param m Optional. The mediator(s) from `x` to `y`.
#'          A numeric vector of the names of the mediators.
#'          The path goes from the first element to the last element.
#'          For example, if `m = c("m1", "m2")`, then the path
#'          is `x -> m1 -> m2 -> y`.
#' @param newdata Required. A data frame of the new data. It must be a
#' data frame.
#' @param ... Additional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm_from_lavaan_list()]
#'
#' @examples
#' library(lavaan)
#' data(data_med)
#' mod <-
#' "
#' m ~ a * x + c1 + c2
#' y ~ b * m + x + c1 + c2
#' "
#' fit <- sem(mod, data_med, fixed.x = FALSE)
#' fit_list <- lm_from_lavaan_list(fit)
#' tmp <- data.frame(x = 1, c1 = 2, c2 = 3, m = 4)
#' predict(fit_list, x = "x", y = "y", m = "m", newdata = tmp)
#'
#' @export

predict.lm_from_lavaan_list <- function(object,
                                        x = NULL,
                                        y = NULL,
                                        m = NULL,
                                        newdata, ...) {
    ptable <- lm_from_lavaan_list2ptable(object)
    if (!check_path(x = x, y = y, m = m, est = ptable)) {
        stop(paste0("The path from ",
                    dQuote(x),
                    " to ",
                    dQuote(y),
                    " through ",
                    paste0(dQuote(m), collapse = ", "),
                    " is invalid."))
      }
    ys <- c(m, y)
    newdata_i <- newdata
    for (yi in ys) {
        yi_hat <- stats::predict(object[[yi]], newdata = newdata_i)
        newdata_i[, yi] <- yi_hat
      }
    newdata_i[, y]
  }

#' @noRd

lm_from_lavaan_list2ptable <- function(x) {
    vars <- sapply(x, get_variables,
                simplify = FALSE)
    dvs <- names(x)
    ptable <- mapply(cbind, dvs, "~", vars,
                     SIMPLIFY = FALSE)
    ptable <- do.call(rbind, ptable)
    ptable <- as.data.frame(ptable)
    colnames(ptable) <- c("lhs", "op", "rhs")
    ptable
  }

#' @noRd

get_variables <- function(x) {
    tmp <- attr(stats::terms(x), "factors")
    id <- (colSums(tmp) == 1)
    colnames(tmp)[id]
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

#' @noRd

lavaan_get_dvs <- function(ptable) {
    i <- ptable$op == "~"
    out <- unique(ptable$lhs[i])
    out
  }