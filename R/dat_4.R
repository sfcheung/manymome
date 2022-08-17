#' @title Sample Dataset: Moderation with One Categorical Moderator
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
#' data(data_mod_cat)
#' dat <- data_mod_cat
#' summary(lm_y <- lm(y ~ x*w + c1 + c2, dat))
"data_mod_cat"

#' @title Sample Dataset: Serial Moderated Mediation with Two
#' Categorical Moderators
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
#' data(data_med_mod_serial_cat)
#' dat <- data_med_mod_serial_cat
#' summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ m1 + x + w1 + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m2*w2 + m1 + x + w1 + c1 + c2, dat))
"data_med_mod_serial_cat"

#' @title Sample Dataset: Parallel Moderated Mediation with Two
#' Categorical Moderators
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
#' data(data_med_mod_parallel_cat)
#' dat <- data_med_mod_parallel_cat
#' summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ x*w1 + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m1*w2 + m2*w2 + m1 + x + w1 + c1 + c2, dat))
"data_med_mod_parallel_cat"



#' @title Sample Dataset: Serial-Parallel Moderated Mediation with Two
#' Categorical Moderators
#'
#' @description A serial-parallel mediation model with two categorical
#' moderators.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator. String. Values: "group1", "group2", "group3"}
#'   \item{w2}{Moderator. String. Values: "team1", "team2"}
#'   \item{m11}{Mediator 1 in Path 1. Numeric.}
#'   \item{m12}{Mediator 2 in Path 1. Numeric.}
#'   \item{m2}{Mediator in Path 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' data(data_med_mod_serial_parallel_cat)
#' dat <- data_med_mod_serial_parallel_cat
#' summary(lm_m11 <- lm(m11 ~ x*w1 + c1 + c2, dat))
#' summary(lm_m12 <- lm(m12 ~ m11 + x + w1 + c1 + c2, dat))
#' summary(lm_m2 <- lm(m2 ~ x + w1 + c1 + c2, dat))
#' summary(lm_y <- lm(y ~ m12 + m2*w2 + m12 + x + c1 + c2, dat))
"data_med_mod_serial_parallel_cat"
