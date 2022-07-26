
#' @title Predicted Values of a 'lm_list'-Class Object
#'
#' @description Compute the predicted values based on
#'  the models stored in a 'lm_list`-class object.
#'
#' @details A `lm_list`-class object is a list
#'  of `lm`-class objects.
#'
#' @return
#' A numeric vector of the predicted values, with length equal to
#' the number of rows of user-supplied data.
#'
#' @param object A 'lm_list'-class object.
#' @param x The variable name at the start of a pathway.
#' @param y The variable name at the end of a pathway.
#' @param m Optional. The mediator(s) from `x` to `y`.
#'          A numeric vector of the names of the mediators.
#'          The path goes from the first element to the last element.
#'          For example, if `m = c("m1", "m2")`, then the path
#'          is `x -> m1 -> m2 -> y`.
#' @param newdata Required. A data frame of the new data. It must be a
#' data frame.
#' @param ... Additional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [lm2list()]
#'
#' @examples
#' data(data_serial_parallel)
#' lm_m11 <- lm(m11 ~ x + c1 + c2, data_serial_parallel)
#' lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, data_serial_parallel)
#' lm_m2 <- lm(m2 ~ x + c1 + c2, data_serial_parallel)
#' lm_y <- lm(y ~ m11 + m12 + m2 + x + c1 + c2, data_serial_parallel)
#' # Join them to form a lm_list-class object
#' lm_serial_parallel <- lm2list(lm_m11, lm_m12, lm_m2, lm_y)
#' lm_serial_parallel
#' summary(lm_serial_parallel)
#' newdat <- data_serial_parallel[3:5, ]
#' predict(lm_serial_parallel,
#'         x = "x",
#'         y = "y",
#'         m = "m2",
#'         newdata = newdat)
#'
#' @export

predict.lm_list <- function(object,
                            x = NULL,
                            y = NULL,
                            m = NULL,
                            newdata, ...) {
    ptable <- lm2ptable(object)$est
    if (!check_path(x = x, y = y, m = m, est = ptable)) {
        stop(paste0("The path from ",
                    dQuote(x),
                    " to ",
                    dQuote(y),
                    " through ",
                    paste0(dQuote(m), collapse = ", "),
                    " is invalid."))
      }
    dvs <- sapply(object, get_response)
    names(object) <- dvs
    ys <- c(m, y)
    newdata_i <- newdata
    for (yi in ys) {
        yi_hat <- stats::predict(object[[yi]], newdata = newdata_i)
        newdata_i[, yi] <- yi_hat
      }
    newdata_i[, y]
  }

