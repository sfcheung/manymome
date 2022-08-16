#' @title Sample Dataset: Simple Mediation
#'
#' @description A simple mediation model.
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med)
#' mod <-
#' "
#' m ~ a * x + c1 + c2
#' y ~ b * m + x + c1 + c2
#' ab := a * b
#' "
#' fit <- sem(mod, data_med, fixed.x = FALSE)
#' parameterEstimates(fit)
"data_med"

#' @title Sample Dataset: One Moderator
#'
#' @description A one-moderator model.
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w}{Moderator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_mod)
#' data_mod$xw <- data_mod$x * data_mod$w
#' mod <-
#' "
#' y ~ a * x + w + d * xw + c1 + c2
#' w ~~ v_w * w
#' w ~ m_w * 1
#' a_lo := a + d * (m_w - sqrt(v_w))
#' a_hi := a + d * (m_w + sqrt(v_w))
#' "
#' fit <- sem(mod, data_mod, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 3, 6, 7, 24, 25), ]
"data_mod"


#' @title Sample Dataset: Two Moderators
#'
#' @description A two-moderator model.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_mod2)
#' data_mod2$xw1 <- data_mod2$x * data_mod2$w1
#' data_mod2$xw2 <- data_mod2$x * data_mod2$w2
#' mod <-
#' "
#' y ~ a * x + w1 + w2 + d1 * xw1 + d2 * xw2 + c1 + c2
#' w1 ~~ v_w1 * w1
#' w1 ~ m_w1 * 1
#' w2 ~~ v_w2 * w2
#' w2 ~ m_w2 * 1
#' a_lolo := a + d1 * (m_w1 - sqrt(v_w1)) + d2 * (m_w2 - sqrt(v_w2))
#' a_lohi := a + d1 * (m_w1 - sqrt(v_w1)) + d2 * (m_w2 + sqrt(v_w2))
#' a_hilo := a + d1 * (m_w1 + sqrt(v_w1)) + d2 * (m_w2 - sqrt(v_w2))
#' a_hihi := a + d1 * (m_w1 + sqrt(v_w1)) + d2 * (m_w2 + sqrt(v_w2))
#' "
#' fit <- sem(mod, data_mod2, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 4, 5, 8:11, 34:37), ]
"data_mod2"


#' @title Sample Dataset: Serial Mediation
#'
#' @description A serial mediation model.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_serial)
#' mod <-
#' "
#' m1 ~ a * x + c1 + c2
#' m2 ~ b1 * m1 + x + c1 + c2
#' y ~ b2 * m2 + m1 + x + c1 + c2
#' indirect := a * b1 * b2
#' "
#' fit <- sem(mod, data_serial,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 4, 8, 28), ]
"data_serial"


#' @title Sample Dataset: Parallel Mediation
#'
#' @description A parallel mediation model.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_parallel)
#' mod <-
#' "
#' m1 ~ a1 * x + c1 + c2
#' m2 ~ a2 * x + c1 + c2
#' y ~ b2 * m2 + b1 * m1 + x + c1 + c2
#' indirect1 := a1 * b1
#' indirect2 := a2 * b2
#' indirect := a1 * b1 + a2 * b2
#' "
#' fit <- sem(mod, data_parallel,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 4, 7, 8, 27:29), ]
"data_parallel"



#' @title Sample Dataset: Serial-Parallel Mediation
#'
#' @description A mediation model with both serial and parallel
#' components.
#'
#' @format A data frame with 100 rows and 7 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{m11}{Mediator 1 in Path 1. Numeric.}
#'   \item{m12}{Mediator 2 in Path 1. Numeric.}
#'   \item{m2}{Mediator in Path 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_serial_parallel)
#' mod <-
#' "
#' m11 ~ a11 * x + c1 + c2
#' m12 ~ b11 * m11 + x + c1 + c2
#' m2 ~ a2 * x + c1 + c2
#' y ~ b12 * m12 + b2 * m2 + m11 + x + c1 + c2
#' indirect1 := a11 * b11 * b12
#' indirect2 := a2 * b2
#' indirect := a11 * b11 * b12 + a2 * b2
#' "
#' fit <- sem(mod, data_serial_parallel,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 4, 8, 11, 12, 34:36), ]
"data_serial_parallel"


#' @title Sample Dataset: Simple Mediation with a-Path Moderated
#'
#' @description A simple mediation model with a-path moderated.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w}{Moderator. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_a)
#' data_med_mod_a$xw <-
#'  data_med_mod_a$x *
#'  data_med_mod_a$w
#' mod <-
#' "
#' m ~ a * x + w + d * xw + c1 + c2
#' y ~ b * m + x + w + c1 + c2
#' w ~~ v_w * w
#' w ~ m_w * 1
#' ab := a * b
#' ab_lo := (a + d * (m_w - sqrt(v_w))) * b
#' ab_hi := (a + d * (m_w + sqrt(v_w))) * b
#' "
#' fit <- sem(mod, data_med_mod_a,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 3, 6, 11, 12, 31:33), ]
"data_med_mod_a"



#' @title Sample Dataset: Simple Mediation with b-Path Moderated
#'
#' @description A simple mediation model with b-path moderated.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w}{Moderator. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_b)
#' data_med_mod_b$mw <-
#'  data_med_mod_b$m *
#'  data_med_mod_b$w
#' mod <-
#' "
#' m ~ a * x + w + c1 + c2
#' y ~ b * m + x + d * mw + c1 + c2
#' w ~~ v_w * w
#' w ~ m_w * 1
#' ab := a * b
#' ab_lo := a * (b + d * (m_w - sqrt(v_w)))
#' ab_hi := a * (b + d * (m_w + sqrt(v_w)))
#' "
#' fit <- sem(mod, data_med_mod_b,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 5, 7, 10, 11, 30:32), ]
"data_med_mod_b"


#' @title Sample Dataset: Simple Mediation with Both Paths Moderated (Two Moderators)
#'
#' @description A simple mediation model with a-path and b-path
#' each moderated by a moderator.
#'
#' @format A data frame with 100 rows and 7 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_ab)
#' data_med_mod_ab$xw1 <-
#'  data_med_mod_ab$x *
#'  data_med_mod_ab$w1
#' data_med_mod_ab$mw2 <-
#'  data_med_mod_ab$m *
#'  data_med_mod_ab$w2
#' mod <-
#' "
#' m ~ a * x + w1 + d1 * xw1 + c1 + c2
#' y ~ b * m + x + w1 + w2 + d2 * mw2 + c1 + c2
#' w1 ~~ v_w1 * w1
#' w1 ~ m_w1 * 1
#' w2 ~~ v_w2 * w2
#' w2 ~ m_w2 * 1
#' ab := a * b
#' ab_lolo := (a + d1 * (m_w1 - sqrt(v_w1))) * (b + d2 * (m_w2 - sqrt(v_w2)))
#' ab_lohi := (a + d1 * (m_w1 - sqrt(v_w1))) * (b + d2 * (m_w2 + sqrt(v_w2)))
#' ab_hilo := (a + d1 * (m_w1 + sqrt(v_w1))) * (b + d2 * (m_w2 - sqrt(v_w2)))
#' ab_hihi := (a + d1 * (m_w1 + sqrt(v_w1))) * (b + d2 * (m_w2 + sqrt(v_w2)))
#' "
#' fit <- sem(mod, data_med_mod_ab,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 3, 6, 10, 41:45), ]
"data_med_mod_ab"


#' @title Sample Dataset: Simple Mediation with Both Paths Moderated By a Moderator
#'
#' @description A simple mediation model with a-path and b-path
#' moderated by one moderator.
#'
#' @format A data frame with 100 rows and 6 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w}{Moderator. Numeric.}
#'   \item{m}{Mediator. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_ab1)
#' data_med_mod_ab1$xw <-
#'  data_med_mod_ab1$x *
#'  data_med_mod_ab1$w
#' data_med_mod_ab1$mw <-
#'  data_med_mod_ab1$m *
#'  data_med_mod_ab1$w
#' mod <-
#' "
#' m ~ a * x + w + da * xw + c1 + c2
#' y ~ b * m + x + w + db * mw + c1 + c2
#' w ~~ v_w * w
#' w ~ m_w * 1
#' ab := a * b
#' ab_lo := (a + da * (m_w - sqrt(v_w))) * (b + db * (m_w - sqrt(v_w)))
#' ab_hi := (a + da * (m_w + sqrt(v_w))) * (b + db * (m_w + sqrt(v_w)))
#' "
#' fit <- sem(mod, data_med_mod_ab1,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 3, 6, 9, 38:40), ]
"data_med_mod_ab1"

#' @title Sample Dataset: Serial Mediation with Two Moderators
#'
#' @description A simple mediation model with a-path and b2-path
#' moderated.
#'
#' @format A data frame with 100 rows and 8 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_serial)
#' data_med_mod_serial$xw1 <-
#'  data_med_mod_serial$x *
#'  data_med_mod_serial$w1
#' data_med_mod_serial$m2w2 <-
#'  data_med_mod_serial$m2 *
#'  data_med_mod_serial$w2
#' mod <-
#' "
#' m1 ~ a * x + w1 + da1 * xw1 + c1 + c2
#' m2 ~ b1 * m1 + x + w1 + c1 + c2
#' y ~ b2 * m2 + m1 + x + w1 + w2 + db2 * m2w2 + c1 + c2
#' w1 ~~ v_w1 * w1
#' w1 ~ m_w1 * 1
#' w2 ~~ v_w2 * w2
#' w2 ~ m_w2 * 1
#' ab1b2 := a * b1 * b2
#' ab1b2_lolo := (a + da1 * (m_w1 - sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
#' ab1b2_lohi := (a + da1 * (m_w1 - sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
#' ab1b2_hilo := (a + da1 * (m_w1 + sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
#' ab1b2_hihi := (a + da1 * (m_w1 + sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
#' "
#' fit <- sem(mod, data_med_mod_serial,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 3, 6, 11, 16, 49:53), ]
"data_med_mod_serial"


#' @title Sample Dataset: Parallel Mediation with Two Moderators
#'
#' @description A parallel mediation model with a1-path and b2-path
#' moderated.
#'
#' @format A data frame with 100 rows and 8 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{m1}{Mediator 1. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_parallel)
#' data_med_mod_parallel$xw1 <-
#'  data_med_mod_parallel$x *
#'  data_med_mod_parallel$w1
#' data_med_mod_parallel$m2w2 <-
#'  data_med_mod_parallel$m2 *
#'  data_med_mod_parallel$w2
#' mod <-
#' "
#' m1 ~ a1 * x + w1 + da1 * xw1 + c1 + c2
#' m2 ~ a2 * x + w1 + c1 + c2
#' y ~ b1 * m1 + b2 * m2 + x + w1 + w2 + db2 * m2w2 + c1 + c2
#' w1 ~~ v_w1 * w1
#' w1 ~ m_w1 * 1
#' w2 ~~ v_w2 * w2
#' w2 ~ m_w2 * 1
#' a1b1 := a1 * b1
#' a2b2 := a2 * b2
#' a1b1_w1lo := (a1 + da1 * (m_w1 - sqrt(v_w1))) * b1
#' a1b1_w1hi := (a1 + da1 * (m_w1 + sqrt(v_w1))) * b2
#' a2b2_w2lo := a2 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
#' a2b2_w2hi := a2 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
#' "
#' fit <- sem(mod, data_med_mod_parallel,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[c(1, 3, 6, 10, 11, 15, 48:53), ]
"data_med_mod_parallel"



#' @title Sample Dataset: Serial-Parallel Mediation with Two Moderators
#'
#' @description A serial-parallel mediation model with some paths
#' moderated.
#'
#' @format A data frame with 100 rows and 9 variables:
#' \describe{
#'   \item{x}{Predictor. Numeric.}
#'   \item{w1}{Moderator 1. Numeric.}
#'   \item{w2}{Moderator 2. Numeric.}
#'   \item{m11}{Mediator 1 in Path 1. Numeric.}
#'   \item{m12}{Mediator 2 in Path 2. Numeric.}
#'   \item{m2}{Mediator 2. Numeric.}
#'   \item{y}{Outcome variable. Numeric.}
#'   \item{c1}{Control variable. Numeric.}
#'   \item{c2}{Control variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(data_med_mod_serial_parallel)
#' data_med_mod_serial_parallel$xw1 <-
#'  data_med_mod_serial_parallel$x *
#'  data_med_mod_serial_parallel$w1
#' data_med_mod_serial_parallel$m2w2 <-
#'  data_med_mod_serial_parallel$m2 *
#'  data_med_mod_serial_parallel$w2
#' mod <-
#' "
#' m11 ~ a1 * x + w1 + da11 * xw1 + c1 + c2
#' m12 ~ b11 * m11 + x + w1 + c1 + c2
#' m2 ~ a2 * x + c1 + c2
#' y ~ b12 * m12 + b2 * m2 + m11 + x + w1 + w2 + db2 * m2w2 + c1 + c2
#' w1 ~~ v_w1 * w1
#' w1 ~ m_w1 * 1
#' w2 ~~ v_w2 * w2
#' w2 ~ m_w2 * 1
#' a1b11b22 := a1 * b11 * b12
#' a2b2 := a2 * b2
#' ab := a1b11b22 + a2b2
#' a1b11b12_w1lo := (a1 + da11 * (m_w1 - sqrt(v_w1))) * b11 * b12
#' a1b11b12_w1hi := (a1 + da11 * (m_w1 + sqrt(v_w1))) * b11 * b12
#' a2b2_w2lo := a2 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
#' a2b2_w2hi := a2 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
#' "
#' fit <- sem(mod, data_med_mod_serial_parallel,
#'            meanstructure = TRUE, fixed.x = FALSE)
#' parameterEstimates(fit)[parameterEstimates(fit)$label != "", ]
"data_med_mod_serial_parallel"

