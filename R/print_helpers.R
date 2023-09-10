#' @noRd

add_whitespace <- function(x,
                           mode = c("left", "right")) {
    mode <- match.arg(mode)
    x_max <- max(sapply(x, nchar))
    out <- sapply(x, function(xx) {
                      to_add <- paste0(rep(" ",
                                           x_max - nchar(xx)),
                                           collapse = "")
                      xx <- switch(mode,
                                   right = paste0(to_add, xx),
                                   left = paste0(xx, to_add))
                      xx
                    }, USE.NAMES = FALSE)
    out
  }