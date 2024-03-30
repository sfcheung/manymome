#' @noRd

lav_implied_all <- function(fit,
                            group_number = NULL) {
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
                  lavaan = lav_implied_all_lavaan(fit,
                                                  group_number = group_number),
                  lavaan.mi = lav_implied_all_lavaan_mi(fit,
                                                        group_number = group_number))
    out
  }

#' @noRd

lav_implied_all_lavaan <- function(fit,
                                   group_number = NULL) {
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

lav_implied_all_lavaan_mi <- function(fit,
                                      group_number = NULL) {
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

lav_est <- function(fit,
                    ...,
                    est_df = NULL) {
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
                  lavaan = lav_est_lavaan(fit,
                                          ...,
                                          est_df = est_df),
                  lavaan.mi = lav_est_lavaan_mi(fit,
                                                ...,
                                                est_df = est_df))
    out
  }

#' @noRd

lav_est_lavaan <- function(fit,
                           ...,
                           est_df = NULL) {
    if (is.null(est_df)) {
        return(lavaan::parameterEstimates(fit, ...))
      } else {
        return(est_df)
      }
  }

#' @noRd

lav_est_lavaan_mi <- function(fit,
                              ...,
                              est_df = NULL) {
    if (is.null(est_df)) {
        est_df <- methods::getMethod("summary",
                signature = "lavaan.mi",
                where = asNamespace("semTools"))(fit,
                                                output = "data.frame",
                                                ...)
      }
    ptable <- as.data.frame(fit@ParTable)
    if (!is.null(ptable$est)) {
        est_df$est <- NULL
        out <- merge(est_df, ptable[, c("lhs", "op", "rhs", "est")],
                     sort = FALSE)
        return(out)
      } else {
        return(est_df)
      }
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

#' @noRd

implied_stats_group_i <- function(object,
                                  group_number = group_number) {
    if (is.null(group_number)) {
        return(object)
      }
    out <- lapply(object, function(x) {
                 if (is.list(x)) {
                     x[[group_number]]
                   } else {
                     x
                   }
              })
    out
  }

#' @noRd

group_labels_and_numbers <- function(groups = NULL,
                                     fit) {
    if (!inherits(fit, "lavaan")) {
        stop("The argument 'fit' must be a lavaan object.")
      }
    group_labels_all <- lavaan::lavInspect(fit, "group.label")
    group_numbers_all <- seq_len(lavaan::lavInspect(fit, "ngroups"))
    if (is.null(groups)) {
        out <- list(label = group_labels_all,
                    number = group_numbers_all)
        return(out)
      }
    if (is.numeric(groups)) {
        if (!all(groups %in% group_numbers_all)) {
            stop("Group numbers not among the numbers in the fit object.")
          }
        group_numbers <- groups
        group_labels <- group_labels_all[group_numbers]
      }
    if (is.character(groups)) {
        if (!all(groups %in% group_labels_all)) {
            stop("Group label(s) not among the labels in the fit object.")
          }
        group_labels <- groups
        group_numbers <- match(groups, group_labels_all)
      }
    list(label = group_labels,
         number = group_numbers)
  }

#' @noRd
# Check if a cond_indirect_effects-class object has wlevels.

has_wlevels <- function(object) {
    if (!is.null(attr(object, "wlevels"))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

#' @noRd
# Check if a cond_indirect_effects-class object has groups.

has_groups <- function(object) {
    if (isTRUE("group" %in% tolower(colnames(object)))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }