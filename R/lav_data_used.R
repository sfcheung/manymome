#' @title Extract data used in a lavaan
#' fit object.
#'
#' @param fit The fit object. Currently
#' only supports a
#' [lavaan::lavaan-class] object.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [lavaan.mi::lavaan.mi()] or
#' its wrapper, such as [lavaan.mi::sem.mi()].
#'
#' @param drop_colon Logical. Whether
#' product terms (of the form "x:y")
#' will be dropped. Default is `TRUE`.
#'
#'
#' @examples
#' \donttest{
#' }
#' @noRd
#'
#'

lav_data_used <- function(fit,
                          drop_colon = TRUE,
                          drop_list_single_group = TRUE,
                          add_lv = TRUE) {
    # Return a named list of N matrices if ngroups > 1
    # TODO: lav_data_used_lavaan_mi()
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
                  lavaan = lav_data_used_lavaan(fit = fit,
                                                drop_colon = drop_colon,
                                                drop_list_single_group = drop_list_single_group,
                                                add_lv = add_lv),
                  lavaan.mi = lav_data_used_lavaan_mi(fit = fit,
                                                      drop_colon = drop_colon))
    out
  }

#' @noRd

lav_data_used_lavaan <- function(fit,
                                 drop_colon = TRUE,
                                 drop_list_single_group = TRUE,
                                 add_lv = TRUE) {
    # Return a named list of N matrices if ngroups > 1
    dat <- lavaan::lavInspect(fit, "data",
                              drop.list.single.group = FALSE)
    i_excluded <- lavaan::lavInspect(fit, "empty.idx",
                                     drop.list.single.group = FALSE)
    if (add_lv) {
      if (length(lavaan::lavNames(fit, "lv")) > 0) {
        dat_lv <- gen_pseudo_raw_lv_data(fit)
        dat <- mapply(
          function(x, y) {
            cbind(x, y)
          },
          x = dat,
          y = dat_lv,
          SIMPLIFY = FALSE
        )
      }
    }
    ngp <- lavaan::lavTech(fit, "ngroups")
    if (drop_colon) {
        for (k in seq_len(ngp)) {
            dat_i <- dat[[k]]
            vnames <- colnames(dat_i)
            vraw <- vnames[!grepl(":", colnames(dat_i))]
            vcolon <- apply(expand.grid(vraw, vraw), 1, paste0, collapse = ":")
            vkeep <- vnames[!(vnames %in% vcolon)]
            dat[[k]] <- dat_i[, vkeep]
          }
      }
    for (k in seq_len(ngp)) {
        if (length(i_excluded[[k]]) > 0) {
            dat[[k]] <- dat[[k]][-i_excluded[[k]], ]
          }
      }
    if (drop_list_single_group && (ngp == 1)) {
        dat <- dat[[1]]
      }
    return(dat)
  }

#' @noRd

lav_data_used_lavaan_mi <- function(fit,
                                    drop_colon = TRUE,
                                    skip_indicators = TRUE) {
    # TODOs: Return a named list of N matrices if ngroups > 1
    dat_list <- fit@DataList
    ovnames <- lavaan::lavNames(fit, "ov")
    fit_org <- lavaan_from_lavaam_mi(fit, data = FALSE)
    dat_common <- keep_identical_cells(dat_list)
    # dat_common <- dat_common[, colnames(dat_common) %in% ovnames]
    fit_tmp <- lavaan::lavaan(model = fit_org,
                              data = dat_list[[1]],
                              do.fit = FALSE)
    dat_tmp <- lavaan::lavInspect(fit_tmp, "data")
    # TODO:
    # - Fix the following when implied latent variable means are supported
    # if (length(lavaan::lavNames(fit, "lv")) > 0) {
    #   dat_lv <- gen_pseudo_raw_lv_data(fit)
    #   dat <- mapply(
    #     function(x, y) {
    #       cbind(x, y)
    #     },
    #     x = dat,
    #     y = dat_lv,
    #     SIMPLIFY = FALSE
    #   )
    # }
    all_prods <- find_all_products(dat_tmp,
                                   expand = TRUE,
                                   skip_indicators = skip_indicators,
                                   fit = fit_tmp)
    dat_tmp <- clear_prods(dat_tmp,
                           all_prods = all_prods)
    vused <- intersect(colnames(dat_tmp), colnames(dat_common))
    for (i in vused) {
        dat_tmp[, i] <- dat_common[, i]
      }
    dat <- form_prods(dat_tmp, prods = all_prods)
    i_excluded <- which(apply(dat, 1, function(x) all(is.na(x))))
    vnames <- colnames(dat)
    if (drop_colon) {
        vraw <- vnames[!grepl(":", colnames(dat))]
        vcolon <- apply(expand.grid(vraw, vraw), 1, paste0, collapse = ":")
        vkeep <- vnames[!(vnames %in% vcolon)]
        dat <- dat[, vkeep]
      }
    if (length(i_excluded) > 0) {
        return(dat[-i_excluded, ])
      } else {
        return(dat)
      }
  }

#' Compare two data sets and set to NA for discrepancy
#' @noRd

keep_identical_cells_i <- function(x1, x2) {
    a <- x1 != x2
    a[is.na(a)] <- FALSE
    p <- ncol(x1)
    for (i in seq_len(p)) {
        x1[a[, i, drop = TRUE], i] <- NA
      }
    x1
  }

#' Compare data sets and keep only values identical across data sets.
#' @noRd

keep_identical_cells <- function(x) {
    out <- Reduce(keep_identical_cells_i, x)
    out
  }

#' Create a lavaan object based on a lavaan.mi object
#' @noRd

lavaan_from_lavaam_mi <- function(fit_mi, data = TRUE) {
    fit_tmp <- methods::new("lavaan",
                  version = as.character(utils::packageVersion("lavaan")))
    fit_tmp@Model <- fit_mi@Model
    if (data) {
        fit_tmp@Data <- fit_mi@Data
      }
    fit_tmp@ParTable <- fit_mi@ParTableList[[1]]
    fit_tmp@pta <- fit_mi@pta
    fit_tmp@Options <- fit_mi@Options
    fit_tmp
  }

#' @noRd

clear_prods <- function(x,
                        all_prods = NULL) {
    if (is.null(all_prods)) {
      all_prods <- find_all_products(x, expand = TRUE)
    }
    if (isTRUE(all(is.na(all_prods)))) {
        return(x)
      } else {
        x[, names(all_prods)] <- NA
        return(x)
      }
 }

#' @noRd

form_prods <- function(x, prods = NA) {
    if (isFALSE(all(is.na(prods)))) {
        pnames <- names(prods)
        for (i in seq_len(length(prods))) {
            x[, pnames[i]] <- apply(x[, prods[[i]]], 1, prod)
          }
        return(x)
      } else {
        return(x)
      }
  }

#' @noRd
get_lv_means_and_cov <- function(fit) {
  lv_means <- lavaan::lavInspect(
                fit,
                "mean.lv",
                drop.list.single.group = FALSE)
  lv_cov <- lavaan::lavInspect(
                fit,
                "cov.lv",
                drop.list.single.group = FALSE)
  out <- mapply(
    function(x, y) {
      list(mean_lv = x,
           cov_lv = y)
    },
    x = lv_means,
    y = lv_cov,
    SIMPLIFY = FALSE
  )
  out
}

#' @noRd
gen_pseudo_raw_lv_data <- function(
  fit
) {
  ns <- lavaan::lavInspect(
          fit,
          "nobs",
          drop.list.single.group = FALSE
        )
  lv_des <- get_lv_means_and_cov(fit)
  f <- function(des, n) {
    cov_lv <- des$cov_lv * n / (n - 1)
    mean_lv <- des$mean_lv
    p <- ncol(cov_lv)
    if (isFALSE(length(mean_lv) == p) ||
        isTRUE(any(is.na(mean_lv)))) {
      mean_lv <- rep(0, p)
      names(mean_lv) <- colnames(cov_lv)
    }
    dat_i <- MASS::mvrnorm(
      n = n,
      mu = mean_lv,
      Sigma = cov_lv,
      empirical = TRUE
    )
    dat_i
  }
  out <- mapply(
    FUN = f,
    des = lv_des,
    n = ns,
    SIMPLIFY = FALSE
  )
  out
}