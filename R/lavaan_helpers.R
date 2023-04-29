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
    out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                mean = c(lavaan::lavInspect(fit, "mean.ov"),
                          lavaan::lavInspect(fit, "mean.lv")))
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
