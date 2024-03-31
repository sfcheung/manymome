#' @title Convert a `cond_indirect_effects`-Class Object to a Data Frame
#'
#' @description Return a
#' `cond_indirect_effects`-class
#' object as a data frame.
#'
#' @details Details
#' It converts a `cond_indirect_effects`-class
#' object as a "pure" data frame.
#'
#' Can add standard errors and *p*-values,
#' if available.
#'
#' @return
#' A data frame with the content of
#' the `cond_indirect_effects`-class
#' object.
#'
#'
                              ...,
                              cut.names = FALSE,
                              col.names = names(x),
                              fix.empty.names = TRUE,
                              check.names = !optional,
                              stringsAsFactors = FALSE


#' @param x A `cond_indirect_effects`-class
#' object, usually the output of
#' [cond_indirect_effects()].
#'
#' @param pvalue Logical. If `TRUE`,
#' asymmetric *p*-values based on
#' bootstrapping will be printed if
#' available. Default is `FALSE.`
#'
#' @param level The level of confidence,
#' default is .95, returning the 95%
#' confidence interval.
#'
#' @param ...  Optional arguments.
#' Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [functionname()]
#'
#' @family relatedfunctions
#'
#' @examples
#'
#' # TODO: TO PREPARE
#'
#' x <- rnorm(10)
#' y <- rnorm(10)
#' out <- lm(y ~ x)
#' confint(out)
#'
#' @export
#' @describeIn topic Description of this function
#' @order 1

as.data.frame.cond_indirect_effects <- function(x,

                              ...) {

}