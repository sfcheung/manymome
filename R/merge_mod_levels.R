#' @title One Line Title
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param ... The output from [mod_levels()].

#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + c4 * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' check_path(x = "x", y = "m3", m = c("m1", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m1", "m2"), fit = fit)
#'
#' @export
#'
#'

merge_mod_levels <- function(...) {
    x <- list(...)
    p <- length(x)
    if (p == 1) {
        if (is.list(x[[1]]) && !is.data.frame(x[[1]])) {
            x <- unlist(x, recursive = FALSE)
            p <- length(x)
          }
      }
    wnames <- paste0("w", seq_len(p))
    names(x) <- wnames
    q <- sapply(x, nrow)
    i <- sapply(q, seq_len, simplify = FALSE)
    qi <- expand.grid(rev(i))
    qi <- qi[, rev(seq_len(ncol(qi)))]
    qinrow <- nrow(qi)
    qi0 <- split(qi, seq_len(qinrow))
    tmpfct <- function(a1, a2, x) {
        out <- x[[a1]][a2, , drop = FALSE]
        # colnames(out) <- a1
        out
      }
    out <- lapply(qi0, function(y) {
                      mapply(tmpfct,
                             a1 = colnames(y),
                             a2 = y[1, ],
                             MoreArgs = list(x = x),
                             USE.NAMES = FALSE,
                             SIMPLIFY = FALSE)
                    })
    out1 <- lapply(out, function(x) {
                data.frame(x, row.names = NULL)
              })
    out2 <- do.call(rbind, out1)
    out2levels0 <- lapply(out, function(x) {
                sapply(x, row.names)
              })
    out2levels <- data.frame(do.call(rbind, out2levels0))
    tmpfct2 <- function(y) {
        if (ncol(y) == 1) return(colnames(y))
        yn0 <- find_prefix(colnames(y))
        if (yn0 != "") {
            return(yn0)
          } else {
            ""
          }
      }
    wnames0 <- lapply(x, tmpfct2)
    tmpfct3 <- function(z1, z2) {
        if (z2 == "") {
            return(z1)
          } else{
            return(z2)
          }
      }
    wnames1 <- mapply(tmpfct3,
                      z1 = names(wnames0),
                      z2 = wnames0)
    colnames(out2levels) <- wnames1
    tmp <- mapply(function(a, b) {paste0(a, ": ", b)},
                  a = colnames(out2levels),
                  b = out2levels)
    wlevels <- apply(tmp, 1, paste, collapse = "; ")
    rownames(out2) <- wlevels
    attr(out2, "wlevels") <- out2levels
    out2
  }
