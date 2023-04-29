lav_implied_all <- function(fit) {
    out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                mean = c(lavaan::lavInspect(fit, "mean.ov"),
                          lavaan::lavInspect(fit, "mean.lv")))
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
