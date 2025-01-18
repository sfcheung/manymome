skip("WIP")

library(testthat)
library(manymome)

#'
#' @description One paragraph description.
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param arg1 Argument description.
#' @param ... Additional arguments.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. Advance online publication. *Health Psychology*.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [functionname()]
#'
#' @family relatedfunctions
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
#' @rdname q_functions
NULL

#' @noRd
form_models_simple <- function(x,
                               y,
                               m,
                               cov) {
  if ((length(x) != 1) ||
      (length(y) != 1) ||
      (length(m) != 1)) {
    stop("The model must have exactly one 'x', one 'y', and one 'm'.")
  }
  if (is.list(cov)) {
    cov_m <- cov[[m]]
    cov_y <- cov[[y]]
  } else {
    cov_m <- cov
    cov_y <- cov
  }
  lm_m_form <- paste(m,
                     "~",
                     paste(c(x, cov_m),
                           collapse = " + "))
  lm_y_form <- paste(y,
                     "~",
                     paste(c(m, x, cov_y),
                           collapse = " + "))
  forms <- c(lm_m_form,
             lm_y_form)
  names(forms) <- c(m, y)
  return(forms)
}
# form_models_simple("iv", "dv", "m1", c("c1", "w"))
# form_models_simple("iv", "dv", "m1", list(dv = c("c1", "w"), m1 = "c2"))
# form_models_simple("iv", "dv", "m1", list(dv = c("c1", "w"), m2 = "c2"))
# form_models_simple("iv", "dv", c("m1", "m2"), list(dv = c("c1", "w"), m2 = "c2"))

#' @noRd
form_models_serial <- function(x,
                               y,
                               m,
                               cov) {
  if ((length(x) != 1) ||
      (length(y) != 1)) {
    stop("The model must have exactly one 'x' and one 'y'.")
  }
  if (is.list(cov)) {
    cov_m <- sapply(m,
                    function(xx) cov[[xx]],
                    simplify = FALSE)
    cov_y <- cov[[y]]
  } else {
    cov_m <- sapply(m,
                    function(x) {cov},
                    simplify = FALSE)
    cov_y <- cov
  }
  # For serial
  for (i in seq_along(m)) {
    if (i == 1) next
    cov_m[[i]] <- c(m[seq(1, i - 1)], cov_m[[i]])
  }
  tmpfct <- function(m,
                     x,
                     cov_m) {
              paste(m,
                     "~",
                     paste(c(x, cov_m),
                           collapse = " + "))
            }
  lm_m_form <- mapply(tmpfct,
                      m = m,
                      cov_m = cov_m,
                      MoreArgs = list(x = x))
  lm_y_form <- paste(y,
                     "~",
                     paste(c(m, x, cov_y),
                           collapse = " + "))
  names(lm_y_form) <- y
  forms <- c(lm_m_form,
             lm_y_form)
  return(forms)
}

# form_models_serial("iv", "dv", c("m1", "m3"), c("c1", "w"))
# form_models_serial("iv", "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))
# form_models_serial(c("iv", "iv2"), "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))


#' @noRd
form_models_parallel <- function(x,
                                 y,
                                 m,
                                 cov) {
  if ((length(x) != 1) ||
      (length(y) != 1)) {
    stop("The model must have exactly one 'x' and one 'y'.")
  }
  if (is.list(cov)) {
    cov_m <- sapply(m,
                    function(xx) cov[[xx]],
                    simplify = FALSE)
    cov_y <- cov[[y]]
  } else {
    cov_m <- sapply(m,
                    function(x) {cov},
                    simplify = FALSE)
    cov_y <- cov
  }
  tmpfct <- function(m,
                     x,
                     cov_m) {
              paste(m,
                    "~",
                    paste(c(x, cov_m),
                          collapse = " + "))
            }
  lm_m_form <- mapply(tmpfct,
                      m = m,
                      cov_m = cov_m,
                      MoreArgs = list(x = x))
  lm_y_form <- paste(y,
                     "~",
                     paste(c(m, x, cov_y),
                           collapse = " + "))
  names(lm_y_form) <- y
  forms <- c(lm_m_form,
             lm_y_form)
  return(forms)
}

# form_models_parallel("iv", "dv", c("m1", "m3"), c("c1", "w"))
# form_models_parallel("iv", "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))
# form_models_parallel(c("iv", "iv2"), "dv", c("m1", "m3"), list(dv = c("c1", "w"), m3 = "c2"))

#' @noRd
# The general q_* function
# Not to be used by users and so not exported.

q_mediation <- function(x,
                        y,
                        m = NULL,
                        cov = NULL,
                        data = NULL,
                        ci_type = c("boot", "mc"),
                        level = .95,
                        R = 100,
                        seed = NULL,
                        boot_type = c("perc", "bc"),
                        model = NULL) {
  if (is.null(model)) {
    stop("Must specify the model by setting the argument 'model'.")
  }
  ci_type <- match.arg(ci_type)
  boot_type <- match.arg(boot_type)
  # Form the model
  lm_forms <- switch(model,
                     simple = form_models_simple(x = x,
                                                 y = y,
                                                 m = m,
                                                 cov = cov),
                     serial = form_models_serial(x = x,
                                                 y = y,
                                                 m = m,
                                                 cov = cov),
                     parallel = form_models_parallel(x = x,
                                                     y = y,
                                                     m = m,
                                                     cov = cov))

  # Do listwise deletion
  to_delete <- lm_listwise(formulas = lm_forms,
                           data = data)
  if (length(to_delete) > 0) {
    data <- data[-to_delete, , drop = FALSE]
  }

  # Regression analysis
  lm_all <- sapply(c(m, y),
                   function(xx) {NA},
                   simplify = FALSE)
  for (i in c(m, y)) {
    lm_all[[i]] <- eval(bquote(lm(.(as.formula(lm_forms[[i]])),
                                  data = data)))
  }
  lm_all <- lm2list(lm_all)

  # Indirect effect
  paths <- all_indirect_paths(lm_all,
                              x = x,
                              y = y,
                              exclude = unique(unlist(cov)))
  ind_ustd <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE)
  # Store the bootstrap estimates
  ind_with_boot_out <- ind_ustd[[1]]
  ind_stdy <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE,
                                    standardized_y = TRUE,
                                    ci_out = ind_with_boot_out)
  ind_stdx <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE,
                                    standardized_x = TRUE,
                                    ci_out = ind_with_boot_out)
  ind_std0 <- many_indirect_effects(paths = paths,
                                    fit = lm_all,
                                    R = R,
                                    ci_type = ci_type,
                                    boot_type = boot_type,
                                    level = level,
                                    seed = seed,
                                    progress = FALSE,
                                    parallel = FALSE,
                                    standardized_y = TRUE,
                                    standardized_x = TRUE,
                                    ci_out = ind_with_boot_out)

  # Total indirect effects

  ind_total_ustd <- total_indirect_effect(ind_ustd, x = x, y = y)
  ind_total_stdx <- total_indirect_effect(ind_stdx, x = x, y = y)
  ind_total_stdy <- total_indirect_effect(ind_stdy, x = x, y = y)
  ind_total_std0 <- total_indirect_effect(ind_std0, x = x, y = y)

  # Combine the output
  out <- list(lm_out = lm_all,
              lm_form = lm_forms,
              ind_out = list(ind_ustd,
                             ind_stdx,
                             ind_stdy,
                             ind_std0),
              ind_total = list(ind_total_ustd,
                               ind_total_stdx,
                               ind_total_stdy,
                               ind_total_std0))
  model_class <- switch(model,
                        simple = "q_simple_mediation",
                        serial = "q_serial_mediation",
                        parallel = "q_parallel_mediation")
  class(out) <- c(model_class,
                  "q_mediation",
                  class(out))
  return(out)
}

# Test: Simple mediation

out <- q_mediation(x = "x",
                   y = "y",
                   m = "m",
                   cov = c("c2", "c1"),
                   data = data_med,
                   ci_type = "boot",
                   R = 500,
                   seed = 1234,
                   boot_type = c("perc"),
                   model = "simple")
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
summary(out$lm_out[["m"]])
summary(out$lm_out[["y"]])
out$ind_total[[1]]

out <- q_mediation(x = "x",
                   y = "y",
                   m = "m",
                   cov = list(m = "c2",
                              y = c("c1", "c2")),
                   data = data_med,
                   ci_type = "boot",
                   R = 500,
                   seed = 1234,
                   boot_type = c("bc"),
                   model = "simple")

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
out$ind_total[[1]]


out <- q_mediation(x = "x",
                   y = "y",
                   m = c("m1", "m2"),
                   cov = c("c2", "c1"),
                   data = data_serial,
                   ci_type = "boot",
                   R = 500,
                   seed = 1234,
                   model = "serial")

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]

out <- q_mediation(x = "x",
                   y = "y",
                   m = c("m1", "m2"),
                   cov = list(m1 = "c1",
                              m2 = c("c2", "c1"),
                              y = "c2"),
                   data = data_serial,
                   ci_type = "boot",
                   R = 500,
                   seed = 1234,
                   model = "serial")

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
summary(out$lm_out[[3]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]

out <- q_mediation(x = "x",
                   y = "y",
                   m = c("m1", "m2"),
                   cov = c("c2", "c1"),
                   data = data_parallel,
                   ci_type = "boot",
                   R = 500,
                   seed = 1234,
                   model = "parallel")

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
summary(out$lm_out[[3]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]
out$ind_total[[4]]
