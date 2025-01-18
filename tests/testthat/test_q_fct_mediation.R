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

#' @describeIn q_functions
#' @export

q_simple_mediation <- function(x,
                               y,
                               m = NULL,
                               cov = NULL,
                               data = NULL,
                               ci_type = c("boot", "mc"),
                               level = .95,
                               R = 100,
                               seed = NULL,
                               boot_type = c("perc", "bc")) {
  ci_type <- match.arg(ci_type)
  boot_type <- match.arg(boot_type)
  # Form the model
  if (is.list(cov)) {
    cov_m <- cov$m
    cov_y <- cov$y
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

  # Do listwise deletion
  to_delete <- lm_listwise(formulas = c(lm_m_form,
                                   lm_y_form),
                           data = data)
  if (length(to_delete) > 0) {
    data <- data[-to_delete, , drop = FALSE]
  }

  # Do the analysis
  lm_m <- eval(bquote(lm(.(as.formula(lm_m_form)),
                         data = data)))
  lm_y <- eval(bquote(lm(.(as.formula(lm_y_form)),
                         data = data)))
  lm_all <- lm2list(lm_m,
                    lm_y)
  # Test indirect effect
  ind_ustd <- indirect_effect(x = x,
                              y = y,
                              m = m,
                              fit = lm_all,
                              R = R,
                              ci_type = ci_type,
                              boot_type = boot_type,
                              level = level,
                              seed = seed,
                              progress = FALSE,
                              parallel = FALSE)
  ind_stdy <- indirect_effect(x = x,
                              y = y,
                              m = m,
                              fit = lm_all,
                              R = R,
                              ci_type = ci_type,
                              boot_type = boot_type,
                              level = level,
                              seed = seed,
                              progress = FALSE,
                              parallel = FALSE,
                              standardized_y = TRUE,
                              ci_out = ind_ustd)
  ind_stdx <- indirect_effect(x = x,
                              y = y,
                              m = m,
                              fit = lm_all,
                              R = R,
                              ci_type = ci_type,
                              boot_type = boot_type,
                              level = level,
                              seed = seed,
                              progress = FALSE,
                              parallel = FALSE,
                              standardized_x = TRUE,
                              ci_out = ind_ustd)
  ind_std0 <- indirect_effect(x = x,
                              y = y,
                              m = m,
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
                              ci_out = ind_ustd)
  # Combine the output
  out <- list(lm_out = lm_all,
              lm_form = c(lm_m_form,
                          lm_y_form),
              ind_out = list(ind_ustd,
                             ind_stdx,
                             ind_stdy,
                             ind_std0))
  class(out) <- c("q_simple_mediation",
                  "q_fun",
                  class(out))
  return(out)
}

#' @describeIn q_functions
#' @export

q_serial_mediation <- function(x,
                               y,
                               m = NULL,
                               cov = NULL,
                               data = NULL,
                               ci_type = c("boot", "mc"),
                               level = .95,
                               R = 100,
                               seed = NULL,
                               boot_type = c("perc", "bc")) {
  ci_type <- match.arg(ci_type)
  boot_type <- match.arg(boot_type)
  m_p <- length(m)
  # Form the model
  if (is.list(cov)) {
    cov_m <- cov[m]
    cov_y <- cov[y]
  } else {
    cov_m <- sapply(m,
                    function(x) {cov},
                    simplify = FALSE)
    cov_y <- cov
  }
  # For serial
  for (i in seq_along(m)) {
    if (i == 1) next
    cov_m[[i]] <- c(cov_m[[i]], m[seq(1, i - 1)])
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

  # Do listwise deletion
  to_delete <- lm_listwise(formulas = c(lm_m_form,
                                   lm_y_form),
                           data = data)
  if (length(to_delete) > 0) {
    data <- data[-to_delete, , drop = FALSE]
  }

  # Do the analysis
  lm_m <- sapply(m,
                 function(xx) {NA},
                 simplify = FALSE)
  for (i in m) {
    lm_m[[i]] <- eval(bquote(lm(.(as.formula(lm_m_form[[i]])),
                              data = data)))
  }
  lm_y <- eval(bquote(lm(.(as.formula(lm_y_form)),
                         data = data)))
  lm_all <- do.call(lm2list,
                    c(lm_m, list(lm_y)))
  # Test indirect effect
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
  # Combine the output
  out <- list(lm_out = lm_all,
              lm_form = c(lm_m_form,
                          lm_y_form),
              ind_out = list(ind_ustd,
                             ind_stdx,
                             ind_stdy,
                             ind_std0))
  class(out) <- c("q_serial_mediation",
                  "q_fun",
                  class(out))
  return(out)
}


#' @describeIn q_functions
#' @export

q_parallel_mediation <- function(x,
                                 y,
                                 m = NULL,
                                 cov = NULL,
                                 data = NULL,
                                 ci_type = c("boot", "mc"),
                                 level = .95,
                                 R = 100,
                                 seed = NULL,
                                 boot_type = c("perc", "bc")) {
  ci_type <- match.arg(ci_type)
  boot_type <- match.arg(boot_type)
  m_p <- length(m)
  # Form the model
  if (is.list(cov)) {
    cov_m <- cov[m]
    cov_y <- cov[y]
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

  # Do listwise deletion
  to_delete <- lm_listwise(formulas = c(lm_m_form,
                                   lm_y_form),
                           data = data)
  if (length(to_delete) > 0) {
    data <- data[-to_delete, , drop = FALSE]
  }

  # Do the analysis
  lm_m <- sapply(m,
                 function(xx) {NA},
                 simplify = FALSE)
  for (i in m) {
    lm_m[[i]] <- eval(bquote(lm(.(as.formula(lm_m_form[[i]])),
                              data = data)))
  }
  lm_y <- eval(bquote(lm(.(as.formula(lm_y_form)),
                         data = data)))
  lm_all <- do.call(lm2list,
                    c(lm_m, list(lm_y)))
  # Test indirect effect
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
  ind_total_ustd <- total_indirect_effect(ind_ustd)
  ind_total_stdx <- total_indirect_effect(ind_stdx)
  ind_total_stdy <- total_indirect_effect(ind_stdy)
  ind_total_std0 <- total_indirect_effect(ind_std0)
  # Combine the output
  out <- list(lm_out = lm_all,
              lm_form = c(lm_m_form,
                          lm_y_form),
              ind_out = list(ind_ustd,
                             ind_stdx,
                             ind_stdy,
                             ind_std0),
              ind_total = list(ind_total_ustd,
                               ind_total_stdx,
                               ind_total_stdy,
                               ind_total_std0))
  class(out) <- c("q_parallel_mediation",
                  "q_fun",
                  class(out))
  return(out)
}

# Test: Simple mediation

out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          ci_type = "boot",
                          R = 500,
                          seed = 1234,
                          boot_type = c("perc"))
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
summary(out$lm_out[[2]])


out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = list(m = "c2",
                                     y = c("c1", "c2")),
                          data = data_med,
                          ci_type = "boot",
                          R = 500,
                          seed = 1234,
                          boot_type = c("bc"))

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])


out <- q_serial_mediation(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          cov = c("c2", "c1"),
                          data = data_serial,
                          ci_type = "boot",
                          R = 500,
                          seed = 1234)

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]

out <- q_serial_mediation(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          cov = list(m1 = "c1",
                                     m2 = c("c2", "c1"),
                                     y = "c2"),
                          data = data_serial,
                          ci_type = "boot",
                          R = 500,
                          seed = 1234)

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
summary(out$lm_out[[3]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]


out <- q_parallel_mediation(x = "x",
                            y = "y",
                            m = c("m1", "m2"),
                            cov = c("c2", "c1"),
                            data = data_parallel,
                            ci_type = "boot",
                            R = 500,
                            seed = 1234)

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
summary(out$lm_out[[3]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]
out$ind_total[[4]]
