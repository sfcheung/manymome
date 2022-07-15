#' @title Moderation with One Categorical Moderator: Sample Dataset
#'
#' @description A moderation model with a categorical moderator.
#'
#' @format A data frame with 300 rows and 5 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w}{Moderator. String. Values: "group1", "group2", "group3"}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(one_categorical_moderator)
#' dat <- one_categorical_moderator
#' summary(lm_y <- lm(y ~ x*w + c1 + c2, dat))
"one_categorical_moderator"

#' @title Serial Moderated Mediation with Two Categorical Moderators:
#' Sample Dataset
#'
#' @description A serial mediation model with two categorical moderators.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator. String. Values: "group1", "group2", "group3"}
#'   \item{w2}{Moderator. String. Values: "team1", "team2"}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(serial_mediation_two_categorical_moderators)
#' dat <- serial_mediation_two_categorical_moderators
#' summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ m1 + x + w1 + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m2*w2 + m1 + x + w1 + c1 + c2, dat))
"serial_mediation_two_categorical_moderators"

#' @title Parallel Moderated Mediation with Two Categorical Moderators:
#' Sample Dataset
#'
#' @description A parallel mediation model with two categorical moderators.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator. String. Values: "group1", "group2", "group3"}
#'   \item{w2}{Moderator. String. Values: "team1", "team2"}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(parallel_mediation_two_categorical_moderators)
#' dat <- parallel_mediation_two_categorical_moderators
#' summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ x*w1 + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m1*w2 + m2*w2 + m1 + x + w1 + c1 + c2, dat))
"parallel_mediation_two_categorical_moderators"



#' @title Serial-Parallel Moderated Mediation with Two Categorical Moderators:
#' Sample Dataset
#'
#' @description A serial-parallel mediation model with two categorical
#' moderators.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator. String. Values: "group1", "group2", "group3"}
#'   \item{w2}{Moderator. String. Values: "team1", "team2"}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(serial_parallel_mediation_categorical_moderators)
#' dat <- serial_parallel_mediation_categorical_moderators
#' summary(lm_m11 <- lm(m11 ~ x*w1 + c1 + c2, dat))
#' summary(lm_m12 <- lm(m12 ~ m11 + x + w1 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ x + w1 + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m12 + m2*w2 + m12 + x + c1 + c2, dat))
"serial_parallel_mediation_categorical_moderators"
