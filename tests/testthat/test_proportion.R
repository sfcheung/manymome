skip("WIP")
library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- data_med_complicated
# Make some path positive
dat$y1 <- -dat$y1
dat$x2 <- -dat$x2
dat$y2 <- dat$y2 + .2 * dat$x2
mod <-
"
m11 ~ x1 + x2 + c1 + c2
m12 ~ m11 + x1 + x2 + c1 + c2
m2 ~ x1 + x2 + c1 + c2
y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2
y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

all_ind_x1_y1 <- all_indirect_paths(fit, x = "x1", y = "y1")
boot_out <- do_boot(fit, R = 100, seed = 5145)
out <- many_indirect_effects(all_ind_x1_y1, fit = fit, boot_ci = TRUE, boot_out = boot_out)
dir_out <- indirect_effect(x = "x1", y = "y1", fit = fit, boot_ci = TRUE, boot_out = boot_out)
out_sum <- Reduce(`+`, out) + dir_out
out[[1]]$boot_indirect / out_sum$boot_indirect

# all_ind_x2_y1 <- all_indirect_paths(fit, x = "x2", y = "y1")
# all_ind_x1_y2 <- all_indirect_paths(fit, x = "x1", y = "y2")
# all_ind_x2_y2 <- all_indirect_paths(fit, x = "x2", y = "y2")
# coef(many_indirect_effects(all_ind_x1_y1, fit = fit)); coef(indirect_effect(x = "x1", y = "y1", fit = fit))
# coef(many_indirect_effects(all_ind_x2_y1, fit = fit)); coef(indirect_effect(x = "x2", y = "y1", fit = fit))
# coef(many_indirect_effects(all_ind_x1_y2, fit = fit)); coef(indirect_effect(x = "x1", y = "y2", fit = fit))
# coef(many_indirect_effects(all_ind_x2_y2, fit = fit)); coef(indirect_effect(x = "x2", y = "y2", fit = fit))
# x1 -> y1: Positive/negative
# x2 -> y1: All negative
# x1 -> y2: Positive/negative
# x1 -> y2: All positive

indirect_proportion <- function(x,
                                y,
                                m = NULL,
                                fit = NULL) {
    if (is.null(m)) {
        stop("m cannot be NULL.")
      }
    if (!check_path(x = x,
                    y = y,
                    m = m,
                    fit = fit)) {
        stop("The path is not valid.")
      }
    direct <- try(indirect_effect(x = x,
                                  y = y,
                                  fit = fit),
                  silent = TRUE)
    if (inherits(direct, "try-error")) {
        stop("No direct path from x to y.")
      }
    # Add a check for moderated paths.
    all_paths <- all_indirect_paths(fit = fit,
                                    x = x,
                                    y = y)
    all_inds <- many_indirect_effects(all_paths,
                                      fit = fit)
    all_inds_bs <- coef(all_inds)
    dir_b <- coef(direct)
    all_bs <- c(all_inds_bs, dir_b)
    if (!(all(all_bs > 0) ||
          all(all_bs < 0))) {
        stop("Not all paths are of the same sign (positive/negative).")
      }
    all_bs_abs <- abs(all_bs)
    total_effect <- sum(all_bs_abs)
    ms <- lapply(all_inds, function(x) x$m)
    ind_i <- sapply(ms, function(x) identical(unname(x), unname(m)))
    ind_effect <- abs(all_inds_bs[ind_i])
    ind_prop <- ind_effect / total_effect
    out <- list(proportion = ind_prop,
                x = x,
                y = y,
                m = m,
                all_paths = all_paths,
                all_indirects = all_inds,
                direct = direct,
                indirect_effect = ind_effect,
                total_effect = total_effect)
    class(out) <- "indirect_proportion"
    out
  }

out1 <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit)
out2 <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit)
out3 <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit)

test_that("indirect_proportion", {
    expect_error(indirect_proportion(x = "x1", y = "y1", m = c("m11", "m12"), fit = fit))
    expect_error(indirect_proportion(x = "x1", y = "y2", m = c("m11", "m12"), fit = fit))
    expect_error(indirect_proportion(x = "x2", y = "y1", m = c("m11", "m12"), fit = fit))
    expect_equal(out1$proportion,
                 .009894992,
                 tolerance = 1e-4,
                 ignore_attr = TRUE)
    expect_equal(out2$proportion,
                 .4540117,
                 tolerance = 1e-4,
                 ignore_attr = TRUE)
    expect_equal(out3$proportion,
                 .7176185,
                 tolerance = 1e-4,
                 ignore_attr = TRUE)
  })

#' @title Print an 'indirect_proportion'-Class Object
#'
#' @description Print the content of an
#' 'indirect_proportion'-class object,
#' the output of [indirect_proportion()].
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#'  `x` is returned invisibly. Called for its side effect.
#'
#' @param x An 'indirect_proportion'-class object.
#' @param ...  Optional arguments.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [indirect_proportion()]
#'
#' @examples
#'
#' x <- 123
#' print(x)
#'
#' @export

print.indirect_proportion <- function(x, ...) {
  cat("\n ==== Proportion of Effect ====\n")
  cat(x$proportion)
  invisible(x)
}
