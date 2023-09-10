#' @title Delta_Med in a
#' 'delta_med'-Class Object
#'
#' @description Return the estimate of
#' Delta_Med in a 'delta_med'-class
#' object.
#
#' @details It just extracts and
#' returns the element `delta_med`
#' in the output of `delta_med()`,
#' the estimate of the Delta_Med
#' proposed by Liu, Yuan, and Li (2023),
#' an $R^2$-like measure of indirect
#' effect.
#'
#' @return
#' A scalar: The estimate of Delta_Med.
#'
#' @param object The output of
#' [delta_med()].
#'
#' @param ...  Optional arguments.
#' Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Liu, H., Yuan, K.-H., & Li, H. (2023).
#' A systematic framework for defining
#' R-squared measures in mediation
#' analysis. *Psychological Methods*.
#' Advance online publication.
#' https://doi.org/10.1037/met0000571
#'
#' @seealso [delta_med()]
#'
#' @examples
#'
#' # TODO: TO PREPARE
#' x <- rnorm(10)
#' y <- rnorm(10)
#' out <- lm(y ~ x)
#' coef(out)
#'
#' @export
#'

coef.delta_med <- function(object,
                           ...) {
    unname(object$delta_med)
  }