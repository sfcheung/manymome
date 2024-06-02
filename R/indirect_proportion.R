#' @title Proportion of Effect Mediated
#'
#' @description It computes the proportion
#' of effect mediated along a pathway.
#'
#' @details The proportion of effect
#' mediated along a path from `x` to
#' `y` is the indirect effect along
#' this path divided by the total
#' effect from `x` to `y`
#' (Alwin & Hauser, 1975). This total
#' effect is equal to the sum of all
#' indirect effects from `x` to `y`
#' and the direct effect from `x` to
#' `y`.
#'
#' To ensure that the proportion can
#' indeed be interpreted as a proportion,
#' this function computes the the
#' proportion only if the signs of
#' all the indirect and direct effects
#' from `x` to `y`
#' are same (i.e., all effects
#' positive or all effects negative).
#'
#' @return
#' An `indirect_proportion` class object.
#' It is a list-like object with these
#' major elements:
#'
#' - `proportion`: The proportion of
#'   effect mediated.
#'
#' - `x`: The name of the `x`-variable.
#'
#' - `y`: The name of the `y`-variable.
#'
#' - `m`: A character vector of the
#' mediator(s) along a path. The path
#' runs from the first element to the
#' last element.
#'
#' This class has a `print` method
#' and a `coef` method.
#'
#' @param x The name of the `x` variable.
#' Must be supplied as a quoted string.
#'
#' @param y The name of the `y` variable.
#' Must be supplied as a quoted string.
#'
#' @param m A vector of the variable
#' names of the mediator(s). The path
#' goes from the first mediator
#' successively to the last mediator.
#' Cannot be `NULL` for this function.
#'
#' @param fit The fit object. Can be a
#' [lavaan::lavaan-class] object or a
#' list of [lm()] outputs.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [semTools::runMI()] or
#' its wrapper, such as [semTools::sem.mi()].
#'
#'
#' @references
#' Alwin, D. F., & Hauser, R. M. (1975).
#' The decomposition of effects in path
#' analysis.
#' *American Sociological Review, 40*(1),
#' 37. \doi{10.2307/2094445}
#'
#' @seealso [print.indirect_proportion()]
#' for the `print` method, and
#' [coef.indirect_proportion()] for
#' the `coef` method.
#'
#' @examples
#'
#' library(lavaan)
#' dat <- data_med
#' head(dat)
#' mod <-
#' "
#' m ~ x + c1 + c2
#' y ~ m + x + c1 + c2
#' "
#' fit <- sem(mod, dat, fixed.x = FALSE)
#' out <- indirect_proportion(x = "x",
#'                            y = "y",
#'                            m = "m",
#'                            fit = fit)
#' out
#'
#' @export

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
    # Do not enable CI for now
    boot_out <- NULL
    mc_out <- NULL
    level <- .95
    if (!is.null(boot_out) && !is.null(mc_out)) {
        stop("Cannot request both bootstrap CI and Monte Carlo CI.")
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
    all_inds_bs <- stats::coef(all_inds)
    dir_b <- stats::coef(direct)
    all_bs <- c(all_inds_bs, dir_b)
    if (!(all(all_bs > 0) ||
          all(all_bs < 0))) {
        stop("Not all effects, indirect and direct",
             "are of the same sign (positive/negative).")
      }
    all_bs_abs <- abs(all_bs)
    total_effect <- sum(all_bs_abs)
    ms <- lapply(all_inds, function(x) x$m)
    ind_i <- sapply(ms, function(x) identical(unname(x), unname(m)))
    ind_effect <- abs(all_inds_bs[ind_i])
    ind_prop <- ind_effect / total_effect
    if (!is.null(ci_type)) {
        effects_sum <- Reduce(`+`, all_inds) + direct
        rep_prop <- all_inds[[which(ind_i)]][[rep_name]] /
                      effects_sum[[rep_name]]
        boot_ci1 <- boot_ci_internal(t0 = ind_prop,
                            t = rep_prop,
                            level = level,
                            boot_ci_type = "perc")
      }
    # TODO (BC): Store boot_ci_type
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

#' @title Print an 'indirect_proportion'-Class
#' Object
#'
#' @description Print the content of an
#' 'indirect_proportion'-class object,
#' the output of [indirect_proportion()].
#'
#' @details The `print` method of the
#' `indirect_proportion`-class object,
#' which is produced by
#' [indirect_proportion()]. In addition
#' to the proportion of effect
#' mediated, it also prints additional
#' information such as the path for
#' which the proportion is computed,
#' and all indirect path(s) from the
#' x-variable to the y-variable.
#'
#' To get the proportion as a scalar,
#' use the `coef` method of
#' `indirect_proportion` objects.
#'
#' @return
#' `x` is returned invisibly. Called for
#' its side effect.
#'
#' @param x An 'indirect_proportion'-class
#' object.
#'
#' @param digits Number of digits to
#' display. Default is 3.
#'
#' @param annotation Logical. Whether
#' additional information should be
#' printed. Default is `TRUE`.
#'
#' @param ...  Optional arguments.
#' Not used.
#'
#' @seealso [indirect_proportion()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- data_med
#' head(dat)
#' mod <-
#' "
#' m ~ x + c1 + c2
#' y ~ m + x + c1 + c2
#' "
#' fit <- sem(mod, dat, fixed.x = FALSE)
#' out <- indirect_proportion(x = "x",
#'                            y = "y",
#'                            m = "m",
#'                            fit = fit)
#' out
#' print(out, digits = 5)
#'
#'
#' @export

print.indirect_proportion <- function(x,
                                      digits = 3,
                                      annotation = TRUE,
                                      ...) {
  cat("==== Proportion of Effect Mediated ====\n")
  cat("\n")
  cat("Path:", "\t\t", names(x$proportion), "\n")
  cat("Proportion:", "\t", formatC(x$proportion,
                                   digits = digits,
                                   format = "f"), "\n")
  cat("Indirect Effect:", formatC(x$indirect_effect,
                                        digits = digits,
                                        format = "f"), "\n")
  cat("Total Effect:", "\t", formatC(x$total_effect,
                                     digits = digits,
                                     format = "f"), "\n")
  if (annotation) {
      cat("\n")
      cat("Note:\n")
      cat("Use coef() to extract the proportion.\n")
      cat("\n")
      cat("All indirect path(s):\n")
      cat(names(x$all_paths), sep = "\n")

    }
  invisible(x)
}

#' @title Extract the Proportion of
#' Effect Mediated
#
#' @description Return the proportion
#' of effect mediated in the output of
#' [indirect_proportion()].
#'
#' @details It extracts and returns the
#' element `proportion` in the input
#' object.
#'
#' @return
#'  A scalar: The proportion of effect
#' mediated.
#'
#' @param object The output of
#' [indirect_proportion()]
#'
#' @param ...  Not used.
#'
#' @seealso [indirect_proportion()]
#'
#' @examples
#'
#' library(lavaan)
#' dat <- data_med
#' head(dat)
#' mod <-
#' "
#' m ~ x + c1 + c2
#' y ~ m + x + c1 + c2
#' "
#' fit <- sem(mod, dat, fixed.x = FALSE)
#' out <- indirect_proportion(x = "x",
#'                            y = "y",
#'                            m = "m",
#'                            fit = fit)
#' out
#' coef(out)
#'
#' @export


coef.indirect_proportion <- function(object, ...) {
    object$proportion
  }
