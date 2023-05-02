#' @noRd

lav_implied_all <- function(fit) {
    type <- NA
    if (inherits(fit, "lavaan")) {
        type <- "lavaan"
      }
    if (inherits(fit, "lavaan.mi")) {
        type <- "lavaan.mi"
      }
    if (isTRUE(is.na(type))) {
        stop("Object is not of a supported type.")
      }
    out <- switch(type,
                  lavaan = lav_implied_all_lavaan(fit),
                  lavaan.mi = lav_implied_all_lavaan_mi(fit))
    out
  }

#' @noRd

lav_implied_all_lavaan <- function(fit) {
    ovnames <- lavaan::lavNames(fit, "ov")
    lvnames <- lavaan::lavNames(fit, "lv")
    allnames <- c(ovnames, lvnames)
    if (lavaan::lavInspect(fit, "meanstructure")) {
        if (length(lvnames) > 0) {
            out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                        mean = c(lavaan::lavInspect(fit, "mean.ov"),
                                  lavaan::lavInspect(fit, "mean.lv")))
          } else {
            out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                        mean = c(lavaan::lavInspect(fit, "mean.ov")))
          }
      } else {
        out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                    mean = stats::setNames(rep(NA, length(allnames)),
                                           allnames))
      }
    out
  }

#' @noRd

lav_implied_all_lavaan_mi <- function(fit) {
    est0 <- methods::getMethod("coef",
              signature = "lavaan.mi",
              where = asNamespace("semTools"))(fit)
    out <- get_implied_i_lavaan_mi(est0 = est0,
                                   fit = fit)
    out
  }

#' @title Get VCOV from a Fit Object
#'
#' @description Works on `lavaan` and `lavaan.mi` objects.
#'
#' @noRd

get_vcov <- function(object) {
    type <- NA
    if (inherits(object, "lavaan")) {
        type <- "lavaan"
      }
    if (inherits(object, "lavaan.mi")) {
        type <- "lavaan.mi"
      }
    if (isTRUE(is.na(type))) {
        stop("Object is not of a supported type.")
      }
    out <- switch(type,
                  lavaan = get_vcov_lavaan(object),
                  lavaan.mi = get_vcov_lavaan_mi(object))
    out
  }

#' @noRd

get_vcov_lavaan <- function(object) {
    lavaan::lavInspect(object, "vcov")
  }

#' @noRd

get_vcov_lavaan_mi <- function(object) {
    methods::getMethod("vcov",
        signature = "lavaan.mi",
        where = asNamespace("semTools"))(object,
                                        type = "pooled",
                                        scale.W = TRUE,
                                        omit.imps = c("no.conv", "no.se"))
  }

#' @noRd

lav_est <- function(fit, ...) {
    type <- NA
    if (inherits(fit, "lavaan")) {
        type <- "lavaan"
      }
    if (inherits(fit, "lavaan.mi")) {
        type <- "lavaan.mi"
      }
    if (isTRUE(is.na(type))) {
        stop("Object is not of a supported type.")
      }
    out <- switch(type,
                  lavaan = lav_est_lavaan(fit, ...),
                  lavaan.mi = lav_est_lavaan_mi(fit, ...))
    out
  }

#' @noRd

lav_est_lavaan <- function(fit, ...) {
    lavaan::parameterEstimates(fit, ...)
  }

#' @noRd

lav_est_lavaan_mi <- function(fit, ...) {
    out0 <- methods::getMethod("summary",
            signature = "lavaan.mi",
            where = asNamespace("semTools"))(fit,
                                            output = "data.frame",
                                            ...)
    ptable <- as.data.frame(fit@ParTable)
    if (!is.null(ptable$est)) {
        out0$est <- NULL
        out <- merge(out0, ptable[, c("lhs", "op", "rhs", "est")])
      } else {
        out <- out0
      }
    out
  }

#' @noRd

lav_ptable <- function(fit) {
    type <- NA
    if (inherits(fit, "lavaan")) {
        type <- "lavaan"
      }
    if (inherits(fit, "lavaan.mi")) {
        type <- "lavaan.mi"
      }
    if (isTRUE(is.na(type))) {
        stop("Object is not of a supported type.")
      }
    out <- switch(type,
                  lavaan = lav_ptable_lavaan(fit),
                  lavaan.mi = lav_ptable_lavaan_mi(fit))
    out
  }

#' @noRd

lav_ptable_lavaan <- function(fit) {
    lavaan::parameterTable(fit)
  }

#' @noRd

lav_ptable_lavaan_mi <- function(fit, ...) {
    out <- lavaan::parameterTable(fit)
    coef_mi <- methods::getMethod("coef",
          signature = "lavaan.mi",
          where = asNamespace("semTools"))(fit)
    se_mi <- sqrt(diag(get_vcov(fit)))
    id_free <- out$free > 0
    out$est <- NA
    out$se <- NA
    out$est[id_free] <- coef_mi
    out$se[id_free] <- se_mi
    out
  }


