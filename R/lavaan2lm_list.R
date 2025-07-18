#' @title 'lavaan'-class to
#' 'lm_from_lavaan_list'-Class
#'
#' @description Converts the regression
#' models in a `lavaan`-class model to
#' an `lm_from_lavaan_list`-class
#' object.
#'
#' @details It identifies all dependent
#' variables in a `lavaan` model and
#' creates an `lm_from_lavaan`-class
#' object for each of them.
#'
#' This is an advanced helper used by
#' [plot.cond_indirect_effects()].
#' Exported for advanced users and
#' developers.
#'
#' @return An
#' `lm_from_lavaan_list`-class object,
#' which is a list of `lm_from_lavaan`
#' objects. It has a `predict`-method
#' ([predict.lm_from_lavaan_list()]) for
#' computing the predicted values from
#' one variable to another.
#'
#' @param fit A `lavaan`-class object,
#' usually the output of
#' [lavaan::lavaan()] or its wrappers.
#'
#'
#' @seealso [predict.lm_from_lavaan_list]
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
    ngroups <- lavaan::lavTech(fit, "ngroups")
    if (ngroups > 1) {
        group_numbers <- seq_len(ngroups)
        group_labels <- lavaan::lavTech(fit, "group.label")
        out <- lapply(group_numbers,
                      function(x) {
                          lm_from_lavaan_list_i(fit = fit,
                                                group_number = x)
                        })
        names(out) <- group_labels
        class(out) <- c("lm_from_lavaan_list", class(out))
      } else {
        out <- lm_from_lavaan_list_i(fit = fit)
      }
    out
  }

#' @noRd

lm_from_lavaan_list_i <- function(fit,
                                  group_number = NULL) {
    ptable <- lav_ptable(fit)
    if ("group" %in% colnames(ptable)) {
        tmp <- unique(ptable$group)
        if (length(tmp) > 1) {
            if (is.null(group_number)) {
                stop("The model has more than one groups but group_number not set.")
              }
            if (!(group_number %in% tmp)) {
                stop("group_number is", group_number,
                     "but group numbers in the model are",
                     paste0(tmp, collapse = ", "))
              }
            ptable <- ptable[ptable$group == group_number, ]
          }
      }
    # Get all dvs (ov.nox, lv.ox)
    dvs <- lavaan_get_dvs(ptable)
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

#' @title Model Terms of an
#' 'lm_from_lavaan'-Class Object
#'
#' @description It extracts the terms
#' object from an `lm_from_lavaan`-class
#' object.
#'
#' @details A method for
#' `lm_from_lavaan`-class that converts
#' a regression model for a variable in
#' a `lavaan` model to a `formula`
#' object. This function simply calls
#' [stats::terms()] on the `formula`
#' object to extract the predictors of a
#' variable.
#'
#' @return A `terms`-class object. See
#' [terms.object] for details.
#'
#' @param x An 'lm_from_lavaan'-class
#' object.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
#'
#' @seealso [terms.object],
#' [lm_from_lavaan_list()]
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

#' @title Coefficients of an
#' 'lm_from_lavaan'-Class Object
#'
#' @description Returns the path
#' coefficients of the terms in an
#' `lm_from_lavaan`-class object.
#'
#' @details An `lm_from_lavaan`-class
#' object converts a regression model
#' for a variable in a `lavaan`-class
#' object to a `formula`-class object.
#' This function simply extracts the
#' path coefficients estimates.
#' Intercept is always included, and set
#' to zero if mean structure is not in
#' the source `lavaan`-class object.
#'
#' This is an advanced helper used by
#' [plot.cond_indirect_effects()].
#' Exported for advanced users and
#' developers.
#'
#' @return A numeric vector of the path
#' coefficients.
#'
#' @param object A
#' 'lm_from_lavaan'-class object.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
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

#' @title Predicted Values of a
#' 'lm_from_lavaan'-Class Object
#'
#' @description Compute the predicted
#' values based on the model stored in a
#' 'lm_from_lavaan`-class object.
#'
#' @details An `lm_from_lavaan`-class
#' method that converts a regression
#' model for a variable in a `lavaan`
#' model to a `formula` object. This
#' function uses the stored model to
#' compute predicted values using
#' user-supplied data.
#'
#' This is an advanced helper used by
#' [plot.cond_indirect_effects()].
#' Exported for advanced users and
#' developers.
#'
#' @return A numeric vector of the
#' predicted values, with length equal
#' to the number of rows of
#' user-supplied data.
#'
#' @param object A
#' 'lm_from_lavaan'-class object.
#'
#' @param newdata Required. A data frame
#' of the new data. It must be a data
#' frame.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
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
    chk <- fix_product_term_order(from = names(bs),
                                  to = colnames(mm))
    if (!is.null(chk)) {
      tmp <- names(bs)
      tmp[chk$i] <- chk$new_names
      names(bs) <- tmp
    }
    out <- mm[, names(bs)] %*% matrix(bs, nrow = p, ncol = 1)
    unname(out[, 1])
  }

#' @title Predicted Values of an
#' 'lm_from_lavaan_list'-Class Object
#'
#' @description It computes the
#' predicted values based on the models
#' stored in an
#' 'lm_from_lavaan_list`-class object.
#'
#' @details An
#'  `lm_from_lavaan_list`-class object
#'  is a list of `lm_from_lavaan`-class
#'  objects.
#'
#' This is an advanced helper used by
#' [plot.cond_indirect_effects()].
#' Exported for advanced users and
#' developers.
#'
#' @return A numeric vector of the
#' predicted values, with length equal
#' to the number of rows of
#' user-supplied data.
#'
#' @param object A
#' 'lm_from_lavaan'-class object.
#'
#' @param x The variable name at the
#' start of a path.
#'
#' @param y The variable name at the end
#' of a path.
#'
#' @param m Optional. The mediator(s)
#' from `x` to `y`. A numeric vector of
#' the names of the mediators. The path
#' goes from the first element to the
#' last element. For example, if `m =
#' c("m1", "m2")`, then the path is `x
#' -> m1 -> m2 -> y`.
#'
#' @param newdata Required. A data frame
#' of the new data. It must be a data
#' frame.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
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

#' @noRd

fix_product_term_order <- function(
                            from,
                            to) {
  to_change <- setdiff(
                  from,
                  to
                )
  if (length(to_change) == 0) {
    return(NULL)
  }
  xw <- lapply(to_change,
               function(x) {
                  strsplit(x,
                           split = ":",
                           fixed = TRUE)[[1]]
               })
  to_comps <- sapply(to,
               function(x) {
                  strsplit(x,
                           split = ":",
                           fixed = TRUE)[[1]]
               },
               simplify = FALSE,
               USE.NAMES = TRUE)
  tmpfct <- function(xw_i) {
    for (i in seq_along(to_comps)) {
      if (setequal(xw_i, to_comps[[i]])) {
        return(names(to_comps[i]))
      }
    }
  }
  xw_new <- sapply(xw,
                   tmpfct)
  i <- match(to_change, from)
  list(i = i,
       new_names = xw_new)
}
