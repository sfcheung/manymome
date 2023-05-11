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
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
est <- parameterEstimates(fit)

all_ind_x1_y1 <- all_indirect_paths(fit, x = "x1", y = "y1")
boot_out <- do_boot(fit, R = 5000, seed = 5145)
mc_out <- do_mc(fit, R = 10000, seed = 5145)
# out <- many_indirect_effects(all_ind_x1_y1, fit = fit, boot_ci = TRUE, boot_out = boot_out)
# dir_out <- indirect_effect(x = "x1", y = "y1", fit = fit, boot_ci = TRUE, boot_out = boot_out)
# out_sum <- Reduce(`+`, out) + dir_out
# out[[1]]$boot_indirect / out_sum$boot_indirect

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

#' @title One Line Title
#'
#' @description One paragraph description
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
#' @describeIn topic Description of this function
#' @order 1

indirect_proportion <- function(x,
                                y,
                                m = NULL,
                                fit = NULL,
                                boot_out = NULL,
                                mc_out = NULL,
                                level = .95) {
    if (is.null(m)) {
        stop("m cannot be NULL.")
      }
    if (!is.null(boot_out) && !is.null(mc_out)) {
        stop("Cannot request both bootstrap CI and Monte Carlo CI.")
      }
    if (!check_path(x = x,
                    y = y,
                    m = m,
                    fit = fit)) {
        stop("The path is not valid.")
      }
    ci_type <- NULL
    boot_ci <- FALSE
    mc_ci <- FALSE
    rep_name <- NULL
    ci_name <- NULL
    if (inherits(boot_out, "boot_out")) {
        ci_type <- "boot"
        boot_ci <- TRUE
        rep_name <- "boot_indirect"
        ci_name <- "boot_ci"
      }
    if (inherits(mc_out, "mc_out")) {
        ci_type <- "mc"
        mc_ci <- TRUE
        rep_name <- "mc_indirect"
        ci_name <- "mc_ci"
      }
    direct <- try(indirect_effect(x = x,
                                  y = y,
                                  fit = fit,
                                  ci_type = ci_type,
                                  boot_out = boot_out,
                                  mc_out = mc_out),
                  silent = TRUE)
    if (inherits(direct, "try-error")) {
        stop("No direct path from x to y.")
      }
    # Add a check for moderated paths.
    all_paths <- all_indirect_paths(fit = fit,
                                    x = x,
                                    y = y)
    all_inds <- many_indirect_effects(all_paths,
                                      fit = fit,
                                      ci_type = ci_type,
                                      boot_out = boot_out,
                                      mc_out = mc_out)
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
    if (!is.null(ci_type)) {
        rep_inds <- sapply(all_inds, function(x) x[[rep_name]])
        rep_total <- cbind(direct[[rep_name]], rep_inds)
        effects_sum <- Reduce(`+`, all_inds) + direct
        rep_prop <- all_inds[[which(ind_i)]][[rep_name]] /
                      effects_sum[[rep_name]]
        browser()
        nboot <- length(rep_prop)
        tmp <- list(t = matrix(rep_prop, nrow = nboot, ncol = 1),
                    t0 = ind_prop,
                    R = nboot)
        boot_ci0 <- boot::boot.ci(tmp, conf = level, type = "perc")
        boot_ci1 <- boot_ci0$percent[4:5]
        names(boot_ci1) <- paste0(formatC(c(100 * (1 - level) / 2,
                                     100 * (1 - (1 - level) / 2)), 2,
                                     format = "f"), "%")
      }
    out <- list(proportion = ind_prop,
                x = x,
                y = y,
                m = m,
                all_paths = all_paths,
                all_indirects = all_inds,
                direct = direct,
                indirect_effect = ind_effect,
                total_effect = total_effect)
    if (!is.null(ci_type)) {
        out[[rep_name]] <- rep_prop
        out[[ci_name]] <- boot_ci1
      }
    class(out) <- "indirect_proportion"
    out
  }

out1 <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit)
out2 <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit)
out3 <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit)

out1_boot <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit, boot_out = boot_out)
out2_boot <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit, boot_out = boot_out)
out3_boot <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit, boot_out = boot_out)

out1_mc <- indirect_proportion(x = "x2", y = "y2", m = c("m11", "m12"), fit = fit, mc_out = mc_out)
out2_mc <- indirect_proportion(x = "x2", y = "y2", m = c("m2"), fit = fit, mc_out = mc_out)
out3_mc <- indirect_proportion(x = "m11", y = "y2", m = c("m12"), fit = fit, mc_out = mc_out)

out1_boot$proportion
out2_boot$proportion
out3_boot$proportion
out1_boot$boot_ci
out2_boot$boot_ci
out3_boot$boot_ci

out1_mc$proportion
out2_mc$proportion
out3_mc$proportion
out1_mc$mc_ci
out2_mc$mc_ci
out3_mc$mc_ci

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
