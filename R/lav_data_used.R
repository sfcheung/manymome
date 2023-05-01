#' @title Extract data used in a lavaan
#' fit object.
#'
#' @param fit The fit object. Currently
#' only supports a
#' [lavaan::lavaan-class] object.
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
                          drop_colon = TRUE) {
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
                                                drop_colon = drop_colon),
                  lavaan.mi = lav_data_used_lavaan_mi(fit = fit,
                                                      drop_colon = drop_colon))
    out
  }

#' @noRd

lav_data_used_lavaan <- function(fit,
                                 drop_colon = TRUE) {
    dat <- lavaan::lavInspect(fit, "data")
    i_excluded <- lavaan::lavInspect(fit, "empty.idx")
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

#' @noRd

lav_data_used_lavaan_mi <- function(fit,
                                    drop_colon = TRUE) {
    dat_list <- fit@DataList
    ovnames <- lavaan::lavNames(fit, "ov")
    fit_org <- lavaan_from_lavaam_mi(fit)
    dat_common <- keep_identical_cells(dat_list)
    # dat_common <- dat_common[, colnames(dat_common) %in% ovnames]
    fit_common <- lavaan::lavaan(model = fit_org,
                                 data = dat_common,
                                 missing = "fiml.x",
                                 do.fit = FALSE)
    dat <- lavaan::lavInspect(fit_common, "data")
    i_excluded <- lavaan::lavInspect(fit_common, "empty.idx")
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

lavaan_from_lavaam_mi <- function(fit_mi) {
    fit_tmp <- methods::new("lavaan",
                  version = as.character(utils::packageVersion("lavaan")))
    fit_tmp@Model <- fit_mi@Model
    fit_tmp@Data <- fit_mi@Data
    fit_tmp@ParTable <- fit_mi@ParTableList[[1]]
    fit_tmp@pta <- fit_mi@pta
    fit_tmp@Options <- fit_mi@Options
    fit_tmp
  }
