#' @noRd

lav_implied_all <- function(fit,
                            group_number = NULL,
                            ovnames = NULL,
                            lvnames = NULL) {
    if (is.null(ovnames)) {
      ovnames <- lavaan::lavNames(fit, "ov")
    }
    if (is.null(lvnames)) {
      lvnames <- lavaan::lavNames(fit, "lv")
    }
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
                                                  group_number = group_number,
                                                  ovnames = ovnames,
                                                  lvnames = lvnames),
                  lavaan.mi = lav_implied_all_lavaan_mi(fit,
                                                        group_number = group_number,
                                                        ovnames = ovnames,
                                                        lvnames = lvnames))
    out
  }

#' @noRd

lav_implied_all_lavaan <- function(fit,
                                   group_number = NULL,
                                   ovnames = NULL,
                                   lvnames = NULL) {
    if (is.null(ovnames)) {
      ovnames <- lavaan::lavNames(fit, "ov")
    }
    if (is.null(lvnames)) {
      lvnames <- lavaan::lavNames(fit, "lv")
    }
    allnames <- c(ovnames, lvnames)
    ngroups <- lavaan::lavTech(fit, "ngroups")
    if (lavaan::lavInspect(fit, "meanstructure")) {
        if (length(lvnames) > 0) {
            implied_ov <- lavaan::lavInspect(fit, "mean.ov")
            implied_lv <- lavaan::lavInspect(fit, "mean.lv")
            if (ngroups > 1) {
              implied_means <- mapply(c,
                                      implied_ov,
                                      implied_lv,
                                      SIMPLIFY = FALSE)
            } else {
              implied_means <- c(implied_ov, implied_lv)
            }
            out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                        mean = implied_means,
                        mean_lv = implied_lv)
          } else {
            out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                        mean = unclass(lavaan::lavInspect(fit, "mean.ov")))
          }
      } else {
        if (ngroups > 1) {
          tmp <- replicate(ngroups,
                          stats::setNames(rep(NA, length(allnames)),
                                            allnames),
                          simplify = FALSE)
          names(tmp) <- lavaan::lavTech(fit, "group.label")
        } else {
          tmp <- stats::setNames(rep(NA, length(allnames)),
                                            allnames)
        }
        out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                    mean = tmp)
      }
  }

#' @noRd

lav_implied_all_lavaan_mi <- function(fit,
                                      group_number = NULL,
                                      ovnames = NULL,
                                      lvnames = NULL) {
    if (is.null(ovnames)) {
      ovnames <- lavaan::lavNames(fit, "ov")
    }
    if (is.null(lvnames)) {
      lvnames <- lavaan::lavNames(fit, "lv")
    }
    est0 <- methods::getMethod("coef",
              signature = "lavaan.mi",
              where = asNamespace("lavaan.mi"))(fit)
    out <- get_implied_i_lavaan_mi(est0 = est0,
                                   fit = fit,
                                   ovnames = ovnames,
                                   lvnames = lvnames)
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
        where = asNamespace("lavaan.mi"))(object,
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
        est_df <- lavaan.mi::parameterEstimates.mi(fit)
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
          where = asNamespace("lavaan.mi"))(fit)
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

group_labels_and_numbers_cond <- function(object,
                                          group_label_name = "Group",
                                          group_number_name = "Group_ID") {
    if (!inherits(object, "cond_indirect_effects")) {
        stop("Object must be a cond_indirect_effects-class object.")
      }
    group_labels <- unique(object[, group_label_name, drop = TRUE])
    group_numbers <- unique(object[, group_number_name, drop = TRUE])
    list(label = group_labels,
         number = group_numbers)
  }

#' @noRd
# Check if a cond_indirect_effects-class object has wlevels.

cond_indirect_effects_has_wlevels <- function(object) {
    if (!is.null(attr(object, "wlevels"))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

#' @noRd
# Check if a cond_indirect_effects-class object has groups.

cond_indirect_effects_has_groups <- function(object) {
    if (isTRUE("group" %in% tolower(colnames(object)))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

#' @noRd
# Check if a cond_indirect_effects-class object has groups.

indirect_list_has_groups <- function(object) {
    if (!is.list(object)) {
        stop("Object must be a list or an indirect_list.")
      }
    tmp <- sapply(object, indirect_has_groups)
    if (!(!all(tmp) || all(tmp))) {
        stop("Some effects are group-specific but some are not.")
      }
    if (all(tmp)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

#' @noRd
# Check if an indirect-class object has groups.

indirect_has_groups <- function(object) {
    if (isTRUE(is.numeric(object$group_number))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

#' @noRd

group_labels_and_numbers_list <- function(object) {
    if (!is.list(object)) {
        stop("Object must be a list or an indirect_list.")
      }
    group_labels <- unique(sapply(object, function(xx) xx$group_label))
    group_numbers <- unique(sapply(object, function(xx) xx$group_number))
    list(label = group_labels,
         number = group_numbers)
  }

#' @noRd
# Check if a cond_indirect_effects-class object has latent x- or y-variables.

cond_indirect_effects_has_x_y <- function(object) {
    fit <- attr(object, "fit")
    fit_type <- cond_indirect_check_fit(fit)
    if (isFALSE(fit_type %in% c("lavaan", "lavaan.mi"))) {
        out <- list(x_latent = NA,
                    y_latent = NA)
        return(out)
      } else {
        fit_lav <- lavaan::lavNames(fit, "lv")
        if (length(fit_lav) == 0) {
            out <- list(x_latent = NA,
                        y_latent = NA)
            return(out)
          }
        full_output <- attr(object, "full_output")[[1]]
        fit_x <- full_output$x
        fit_y <- full_output$y
        out <- list(x_latent = isTRUE(fit_x %in% fit_lav),
                    y_latent = isTRUE(fit_y %in% fit_lav))
        return(out)
      }
  }
