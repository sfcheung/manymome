#' @title Extract data used in a lavaan fit object.
#'
#' @param fit The fit object. Currently only supports a
#'            [lavaan::lavaan-class] object.
#' @param drop_colon Logical. Whether product terms (of the form "x:y")
#'                   will be dropped. Default is `TRUE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#' \donttest{
#' }
#' @noRd
#'
#'

lav_data_used <- function(fit,
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
