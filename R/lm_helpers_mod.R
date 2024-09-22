#' @title VCOVs of a List of 'lm'-Objects
#'
#' @description Compute the variance-covariance
#' matrices of estimates.
#'
#' @details
#' - It only supports `lm`-objects.
#'
#' - It does not support multiple-group models.
#'
#' @return
#' A list of numeric matrices, one for
#' each response variable (`y`) in the list
#' of models. The name of each element
#' of the list is the name of the
#' response variable.
#'
#' @param object A list of `lm`-objects,
#' such as the output of `lm2list()`.
#'
#' @noRd

lm_list_vcov <- function(object,
                         est = NULL) {
    vcov0 <- lapply(object,
                    stats::vcov)
    if (is.null(est)) {
        coefs <- lapply(object, coef2lor)
        est <- do.call(rbind, coefs)
      }
    ys <- sapply(object,
                 get_response)
    names(vcov0) <- ys
    est$uid <- seq_len(nrow(est))
    for (yy in ys) {
        vcov1 <- vcov0[[yy]]
        vcov_names <- colnames(vcov1)
        j <- est$lhs == yy
        i <- match(vcov_names, est[j, "rhs"])
        k <- est[j, "uid"][i]
        l <- match("(Intercept)", vcov_names)
        k[l] <- est[j & est$op == "~1", "uid"]
        m <- order(k)
        vcov2 <- vcov1[m, m]
        vcov0[[yy]] <- vcov2
      }
    vcov0
  }

#' @title Standard Error of a Conditional Effect
#'
#' @description Compute the standard error
#' of a simple effect conditioned on specific
#' values of the moderator.
#'
#' @details
#' - It supports only a path with no mediator.
#'
#' @param xi An output of [get_prod()].
#'
#' @param est_vcov The variance-covariance
#' matrix of all the parameters in a model.
#'
#' @param wvalues A vector of the values
#' of the moderators.
#'
#' @noRd

cond_se <- function(xi,
                    est_vcov,
                    wvalues) {
    if (all(is.na(xi))) return(0)
    if (is.null(xi$prod)) return(0)
    prod_i <- xi$prod
    b_i <- xi$b
    w_i <- xi$w
    if (is.list(w_i)) {
        w_i0 <- sapply(w_i, paste0, collapse = ":")
      } else {
        w_i0 <- w_i
      }
    wvalues_i <- mapply(function(b1, w1, wvalues) {
                      prod(wvalues[w1])
                    },
                    b1 = b_i,
                    w1 = w_i,
                    MoreArgs = list(wvalues = wvalues))
    wv_na <- is.na(wvalues_i)
    if (isTRUE(any(wv_na))) {
        wvalues_i[wv_na] <- 0
        names(wvalues_i) <- w_i0
      }
    yi <- xi$y
    est_vcov_i <- est_vcov[[yi]][c(xi$x, prod_i), c(xi$x, prod_i), drop = FALSE]
    b0 <- matrix(c(1, wvalues_i),
                 ncol = 1)
    out <- t(b0) %*% est_vcov_i %*% b0
    out <- sqrt(as.numeric(out))
    out
  }

#' @title Residual Degrees of Freedom of a List of 'lm'-Objects
#'
#' @description Extract the residual
#' degrees of freedom of each of the
#' models in a list of 'lm'-object.
#'
#' @details
#' - Support only `lm` objects.
#'
#' @param object A list of `lm`-objects.
#'
#' @return
#' A named numeric vector of the
#' residual dfs.
#'
#' @noRd

lm_df_residual <- function(object) {
    out <- sapply(object,
                  stats::df.residual)
    ys <- sapply(object,
                 get_response)
    names(out) <- ys
    out
  }
