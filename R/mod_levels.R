#' @title Create Levels of Moderators
#'
#' @description Create levels of
#' moderators to be used by
#' [indirect_effect()],
#' [cond_indirect_effects()], and
#' [cond_indirect()].
#'
#' @details It creates values of a
#' moderator that can be used to compute
#' conditional effect or conditional
#' indirect effect. By default, for a
#' numeric moderator, it uses one
#' standard deviation below mean, mean,
#' and one standard deviation above
#' mean. The percentiles of these three
#' levels in a normal distribution
#' (16th, 50th, and 84th) can also be
#' used. For categorical variable, it
#' will simply collect the unique
#' categories in the data.
#'
#' The generated levels are then used by
#' [cond_indirect()] and
#' [cond_indirect_effects()].
#'
#' If a model has more than one
#' moderator, [mod_levels_list()] can be
#' used to generate combinations of
#' levels. The output can then passed to
#' [cond_indirect_effects()] to compute
#' the conditional effects or
#' conditional indirect effects for all
#' the combinations.
#'
#' @return [mod_levels()] returns a
#' `wlevels`-class object which is a
#' data frame with additional attributes
#' about the levels.
#'
#' [mod_levels_list()] returns a list of
#' `wlevels`-class objects, or a
#' `wlevels`-class object which is a
#' data frame of the merged levels if
#' `merge = TRUE`.
#'
#' @param w Character. The names of the moderator. If the moderator is
#' categorical with 3 or more groups, this is the vector of the
#' indicator variables.
#'
#' @param fit The fit object. Can be a
#' [lavaan::lavaan-class] object or a
#' list of [lm()] outputs.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [lavaan.mi::lavaan.mi()] or
#' its wrapper, such as [lavaan.mi::sem.mi()].
#' If it is a single model fitted by
#' [lm()], it will be automatically converted
#' to a list by [lm2list()].
#'
#' @param w_type Character. Whether the
#' moderator is a `"numeric"` variable
#' or a `"categorical"` variable. If
#' `"auto"`, the function will try to
#' determine the type automatically.
#'
#' @param w_method Character, either
#' `"sd"` or `"percentile"`. If `"sd"`,
#' the levels are defined by the
#' distance from the mean in terms of
#' standard deviation. if
#' `"percentile"`, the levels are
#' defined in percentiles.
#'
#' @param sd_from_mean A numeric vector.
#' Specify the distance in standard
#' deviation from the mean for each
#' level. Default is `c(-1, 0, 1)` for
#' [mod_levels()]. For
#' [mod_levels_list()], the default is
#' `c(-1, 0, 1)` when there is only one
#' moderator, and `c(-1, 1)` when there
#' are more than one moderator. Ignored
#' if `w_method` is not equal to `"sd"`.
#'
#' @param percentiles A numeric vector.
#' Specify the percentile (in
#' proportion) for each level. Default
#' is `c(.16, .50, .84)` for
#' [mod_levels()], corresponding
#' approximately to one standard
#' deviation below mean, mean, and one
#' standard deviation above mean in a
#' normal distribution. For
#' [mod_levels_list()], default is
#' `c(.16, .50, .84)` if there is one
#' moderator, and `c(.16, .84)` when
#' there are more than one moderator.
#' Ignored if `w_method` is not equal to
#' `"percentile"`.
#'
#' @param extract_gp_names Logical. If
#' `TRUE`, the default, the function
#' will try to determine the name of
#' each group from the variable names.
#'
#' @param prefix Character. If
#' `extract_gp_names` is `TRUE` and
#' `prefix` is supplied, it will be
#' removed from the variable names to
#' create the group names. Default is
#' `NULL`, and the function will try to
#' determine the prefix automatically.
#'
#' @param values For numeric moderators,
#' a numeric vector. These are the
#' values to be used and will override
#' other options. For categorical
#' moderators, a named list of numeric
#' vector, each vector has length equal
#' to the number of indicator variables.
#' If the vector is named, the names
#' will be used to label the values. For
#' example, if set to `list(gp1 = c(0,
#' 0), gp3 = c(0, 1)`, two levels will
#' be returned, one named `gp1` with the
#' indicator variables equal to 0 and 0,
#' the other named `gp3` with the
#' indicator variables equal to 0 and 1.
#' Default is `NULL`.
#'
#' @param reference_group_label For
#' categorical moderator, if the label
#' for the reference group (group with
#' all indicators equal to zero) cannot
#' be determined, the default label is
#' `"Reference"`. To change it, set
#' `reference_group_label` to the
#' desired label. Ignored if `values` is
#' set.
#'
#' @param descending If `TRUE`
#' (default), the rows are sorted in
#' descending order for numerical
#' moderators: The highest value on the
#' first row and the lowest values on
#' the last row. For user supplied
#' values, the first value is on the
#' last row and the last value is on the
#' first row. If `FALSE`, the rows are
#' sorted in ascending order.
#'
#' @param ... The names of moderators
#' variables. For a categorical
#' variable, it should be a vector of
#' variable names.
#'
#' @param merge If `TRUE`,
#' [mod_levels_list()] will call
#' [merge_mod_levels()] and return the
#' merged levels. Default is `FALSE`.
#'
#'
#' @seealso [cond_indirect_effects()] for computing conditional
#'   indiret effects; [merge_mod_levels()] for merging
#'   levels of moderators.
#'
#' @examples
#'
#' library(lavaan)
#' data(data_med_mod_ab)
#' dat <- data_med_mod_ab
#' # Form the levels from a list of lm() outputs
#' lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
#' lm_y <- lm(y ~ m*w2 + x + w1 + c1 + c2, dat)
#' lm_out <- lm2list(lm_m, lm_y)
#' w1_levels <- mod_levels(lm_out, w = "w1")
#' w1_levels
#' w2_levels <- mod_levels(lm_out, w = "w2")
#' w2_levels
#' # Indirect effect from x to y through m, at the first levels of w1 and w2
#' cond_indirect(x = "x", y = "y", m = "m",
#'               fit = lm_out,
#'               wvalues = c(w1 = w1_levels$w1[1],
#'                           w2 = w2_levels$w2[1]))
#' # Can form the levels based on percentiles
#' w1_levels2 <- mod_levels(lm_out, w = "w1", w_method = "percentile")
#' w1_levels2
#' # Form the levels from a lavaan output
#' # Compute the product terms before fitting the model
#' dat$mw2 <- dat$m * dat$w2
#' mod <-
#' "
#' m ~ x + w1 + x:w1 + c1 + c2
#' y ~ m + x + w1 + w2 + mw2 + c1 + c2
#' "
#' fit <- sem(mod, dat, fixed.x = FALSE)
#' cond_indirect(x = "x", y = "y", m = "m",
#'               fit = fit,
#'               wvalues = c(w1 = w1_levels$w1[1],
#'                           w2 = w2_levels$w2[1]))
#' # Can pass all levels to cond_indirect_effects()
#' # First merge the levels by merge_mod_levels()
#' w1w2_levels <- merge_mod_levels(w1_levels, w2_levels)
#' cond_indirect_effects(x = "x", y = "y", m = "m",
#'                       fit = fit,
#'                       wlevels = w1w2_levels)
#'
#'
#'
#' @export
#'
#'
#' @describeIn mod_levels Generate
#' levels for one moderator.
#'
#' @order 1

mod_levels <- function(w,
                       fit,
                       w_type = c("auto", "numeric", "categorical"),
                       w_method = c("sd", "percentile"),
                       sd_from_mean = c(-1, 0, 1),
                       percentiles = c(.16, .50, .84),
                       extract_gp_names = TRUE,
                       prefix = NULL,
                       values = NULL,
                       reference_group_label = NULL,
                       descending = TRUE) {
    if (!missing(fit)) {
         fit <- auto_lm2list(fit)
      }
    fit_type <- cond_indirect_check_fit(fit)
    if (fit_type == "lavaan") {
        if (lavaan::lavTech(fit, "ngroups") > 1) {
            stop("Multigroup models not yet supported.")
          }
      }
    w_type <- match.arg(w_type)
    if (w_type == "auto") {
        w_type <- find_w_type(w, fit)
      }
    if (fit_type == "lm") {
        if (w_type == "numeric") {
            out <- mod_levels_i_lm_numerical(fit = fit,
                                             w = w,
                                             w_method = w_method,
                                             sd_from_mean = sd_from_mean,
                                             percentiles = percentiles,
                                             values = values,
                                             descending = descending)
          }
        if (w_type == "categorical") {
            out <- mod_levels_i_lm_categorical(fit = fit,
                                               w = w,
                                               extract_gp_names = extract_gp_names,
                                               prefix = prefix,
                                               values = values,
                                               reference_group_label = reference_group_label)
          }
      }
    if (fit_type == "lavaan" || fit_type == "lavaan.mi" ) {
        if (w_type == "numeric") {
            out <- mod_levels_i_lavaan_numerical(fit = fit,
                                                 w = w,
                                                 w_method = w_method,
                                                 sd_from_mean = sd_from_mean,
                                                 percentiles = percentiles,
                                                 values = values,
                                                 descending = descending)
          }
        if (w_type == "categorical") {
            out <- mod_levels_i_lavaan_categorical(fit = fit,
                                                   w = w,
                                                   extract_gp_names = extract_gp_names,
                                                   prefix = prefix,
                                                   values = values,
                                                   reference_group_label = reference_group_label)
          }
      }
    tmp <- data.frame(x = rownames(out))
    colnames(tmp) <- attr(out, "wname")
    attr(out, "wlevels") <- tmp
    attr(out, "w_type") <- w_type
    class(out) <- c("wlevels", class(out))
    out
  }

#' @examples
#'
#' # mod_levels_list() forms a combinations of levels in one call
#' # It returns a list, by default.
#' # Form the levels from a list of lm() outputs
#' # "merge = TRUE" is optional. cond_indirect_effects will merge the levels
#' # automatically.
#' w1w2_levels <- mod_levels_list("w1", "w2", fit = fit, merge = TRUE)
#' w1w2_levels
#' cond_indirect_effects(x = "x", y = "y", m = "m",
#'                       fit = fit, wlevels = w1w2_levels)
#' # Can work without merge = TRUE:
#' w1w2_levels <- mod_levels_list("w1", "w2", fit = fit)
#' w1w2_levels
#' cond_indirect_effects(x = "x", y = "y", m = "m",
#'                       fit = fit, wlevels = w1w2_levels)
#'
#'
#' @export
#'
#' @describeIn mod_levels Generate
#' levels for several moderators.
#'
#' @order 2

mod_levels_list <- function(...,
                            fit,
                            w_type = "auto",
                            w_method = "sd",
                            sd_from_mean = NULL,
                            percentiles = NULL,
                            extract_gp_names = TRUE,
                            prefix = NULL,
                            descending = TRUE,
                            merge = FALSE) {
    if (!missing(fit)) {
         fit <- auto_lm2list(fit)
      }
    x <- list(...)
    p <- length(x)
    if (is.null(sd_from_mean)) {
        if (p > 1) {
            sd_from_mean <- c(-1, 1)
          } else {
            sd_from_mean <- c(-1, 0, 1)
          }
      }
    if (is.null(percentiles)) {
        if (p > 1) {
            percentiles <- c(.16, .84)
          } else {
            percentiles <- c(.16, .50, .84)
          }
      }
    out <- lapply(x, mod_levels,
                  fit = fit,
                  w_type = w_type,
                  w_method = w_method,
                  sd_from_mean = sd_from_mean,
                  percentiles = percentiles,
                  extract_gp_names = extract_gp_names,
                  prefix = prefix,
                  descending = descending)
    if (merge) {
        out2 <- merge_mod_levels(out)
        return(out2)
      } else {
        return(out)
      }
  }


mod_levels_i_lavaan_numerical <- mod_levels_i_lm_numerical <- function(fit,
                                      w,
                                      w_method = c("sd", "percentile"),
                                      sd_from_mean = c(-1, 0, 1),
                                      percentiles = c(.16, .50, .84),
                                      values = NULL,
                                      descending = TRUE) {
    # No need for user-specified method. If users want to specify their own
    # values, they do not need  to call this function
    fit_type <- cond_indirect_check_fit(fit)
    mm <- switch(fit_type,
                 lavaan = as.data.frame(lav_data_used(fit)),
                 lavaan.mi = as.data.frame(lav_data_used(fit)),
                 lm = merge_model_matrix(fit))
    w_method <- match.arg(w_method)
    if (!is.null(values)) {
        w_method <- "user"
      }
    if (w_method == "user") {
        if (!is.numeric(values)) {
            stop("values must be a numeric vector.")
          }
        values <- values[!is.na(values)]
        levels <- values
        if (is.null(names(values))) {
            vnames <- as.character(values)
          } else {
            vnames <- names(values)
          }
        names(levels) <- vnames
        out <- data.frame(w = levels)
        rownames(out) <- vnames
        colnames(out) <- w
      }
    if (w_method %in% c("sd", "percentile")) {
      w_dat <- mm[, w]
    }
    if (w_method == "sd") {
        w_mean <- mean(w_dat, na.rm = TRUE)
        w_sd <- stats::sd(w_dat, na.rm = TRUE)
        levels <- w_mean + sd_from_mean * w_sd
        vnames <- ifelse(sd_from_mean > 0,
                         paste0("+", formatC(sd_from_mean, 1, format = "f")),
                         formatC(sd_from_mean, 1, format = "f"))
        vnames <- paste0("M", vnames, "SD")
        vnames <- gsub("M0.0SD", "Mean", vnames)
        names(levels) <- vnames
        out <- data.frame(w = levels)
        rownames(out) <- vnames
        colnames(out) <- w
      }
    if (w_method == "percentile") {
        w_q <- stats::quantile(w_dat, probs = percentiles, na.rm = TRUE)
        out <- data.frame(w = w_q)
        rownames(out) <- names(w_q)
        colnames(out) <- w
      }
    if (descending) {
        out <- out[rev(seq_len(nrow(out))), , drop = FALSE]
      }
    attr(out, "wname") <- w
    return(out)
  }

mod_levels_i_lavaan_categorical <- mod_levels_i_lm_categorical <- function(fit,
                                        w,
                                        extract_gp_names = TRUE,
                                        prefix = NULL,
                                        values = NULL,
                                        reference_group_label = NULL) {
    fit_type <- cond_indirect_check_fit(fit)
    mm <- switch(fit_type,
                 lavaan = as.data.frame(lav_data_used(fit)),
                 lavaan.mi = as.data.frame(lav_data_used(fit)),
                 lm = merge_model_matrix(fit))
    mf <- switch(fit_type,
                 lavaan = NA,
                 lavaan.mi = NA,
                 lm = merge_model_frame(fit))
    if (!(all(w %in% colnames(mm))) && (fit_type == "lm")) {
        w_source <- w
        w <- find_ind(fit, w)
      } else {
        w_source <- NA
      }
    w_dat <- mm[, w, drop = FALSE]
    w_dat <- w_dat[stats::complete.cases(w_dat), , drop = FALSE]
    w_gp <- unique(w_dat)
    k <- nrow(w_gp)
    j <- rev(seq_len(ncol(w_gp)))
    i <- do.call(order, w_gp[, j, drop = FALSE])
    w_gp <- w_gp[i, , drop = FALSE]
    gpnames <- paste0("Category ", seq_len(k))
    rownames(w_gp) <- gpnames
    if (extract_gp_names) {
        if ((fit_type == "lavaan") || (fit_type == "lavaan.mi")) {
            w_gp <- set_gp_names(w_gp, prefix = prefix)
          }
        if (fit_type == "lm") {
            if (is.na(w_source)) {
                w_source <- find_source_cat(fit, w)
              }
            tmp <- cbind(w_source = mf[, w_source, drop = FALSE], mm[, w, drop = FALSE])
            tmp <- tmp[!duplicated(tmp), , drop = FALSE]
            tmp2 <- merge(x = w_gp, y = tmp, sort = FALSE)
            rownames(w_gp) <- tmp2[, w_source]
          }
      }
    if (is.null(prefix)) {
        prefix <- switch(fit_type,
                    lavaan = find_prefix(w),
                    lavaan.mi = find_prefix(w),
                    lm = w_source)
      }
    if (!is.null(values)) {
        if (!is.list(values) && nrow(w_gp) > 2) {
            stop("values must a list of numeric vectors when there are more than 2 groups.")
          }
        if (is.null(names(values))) {
            stop("values must be a named list, with names equal to the labels of each group.")
          }
        w_gp_chk <- sapply(values, check_cat_values, target = w_gp)
        if (!isTRUE(all(w_gp_chk))) {
            stop("Some values are not in the list of group. Please check the values.")
          }
        out <- as.data.frame(do.call(rbind, values))
        colnames(out) <- colnames(w_gp)
        w_gp_org <- w_gp
        w_gp <- out
      } else {
        if (!is.null(reference_group_label)) {
          rownames(w_gp)[which(rownames(w_gp) == "Reference")] <-
            reference_group_label
        }
      }
    attr(w_gp, "wname") <- prefix
    return(w_gp)
  }

find_prefix <- function(x) {
    kmin <- min(nchar(x))
    kmax <- max(nchar(x))
    out <- ""
    for (i in seq_len(kmin)) {
        xi <- substr(x, 1, i)
        if (length(unique(xi)) == 1) {
            out <- xi[1]
          } else {
            break
          }
      }
    out
  }

set_gp_names <- function(x,
                         prefix = NULL) {
    if (!isTRUE(any(x == 0) || any(x == 1))) return(x)
    if (identical(unique(apply(x, 1, function(x) sum(x == 1))),
                  c(0, 1))) {
        return(x)
      }
    names0 <- colnames(x)
    if (is.null(prefix)) {
        prefix <- find_prefix(names0)
      }
    names1 <- sub(prefix, "", names0, fixed = TRUE)
    if (isTRUE(any(names1 == ""))) return(x)
    k <- nrow(x)
    for (i in seq_len(k)) {
        if (isTRUE(all(x[i, ] == 0))) {
            rownames(x)[i] <- "Reference"
            next
          }
        j <- which(x[i, ] == 1)
        if (length(j) == 1) {
            rownames(x)[i] <- names1[j]
            next
          }
      }
    x
  }

check_cat_values <- function(x0, target) {
    for (i in seq_len(nrow(target))) {
        if (isTRUE(all.equal(x0, unlist(target[i, ]), check.attributes = FALSE))) {
            return(TRUE)
          }
      }
    return(FALSE)
  }

find_source_cat <- function(lm_list, w) {
    mm <- merge_model_matrix(lm_list)
    mf <- merge_model_frame(lm_list)
    terms_list <- lapply(lm_list,
                      function(x) stats::delete.response(stats::terms(x)))
    coefs_list <- lapply(lm_list, stats::coef)
    i <- sapply(coefs_list, function(x) {any(w %in% names(x))})
    terms_i <- terms_list[[which(i)[1]]]
    j <- (attr(terms_i, "dataClasses") == "character") |
         (attr(terms_i, "dataClasses") == "factor")
    jnames <- names(j)[j]
    jcat <- sapply(jnames, function(x) unique(mf[, x]),
                   simplify = FALSE)
    jcatind <- mapply(function(x, y) {paste0(x, y)}, x = jnames, y = jcat,
                   SIMPLIFY = FALSE)
    k <- sapply(jcatind, function(x) all(w %in% x))
    jnames[k]
  }

find_ind <- function(lm_list, w) {
    mm <- merge_model_matrix(lm_list)
    mf <- merge_model_frame(lm_list)
    terms_list <- lapply(lm_list,
                      function(x) stats::delete.response(stats::terms(x)))
    coefs_list <- lapply(lm_list, stats::coef)
    i <- sapply(terms_list, function(x) {any(w %in% all.vars(x))})
    terms_i <- terms_list[[which(i)[1]]]
    coefs_i <- coefs_list[[which(i)[1]]]
    coefs_i_names <- names(coefs_i)
    # TODO: Should use a more robust method, e.g., use contrasts.arg
    w_values <- unique(as.character(mf[, w]))
    k <- which(coefs_i_names %in% paste0(w, w_values))
    knames <- coefs_i_names[k]
  }

find_w_type <- function(w, fit) {
    if (length(w) > 1) {
        return("categorical")
      }
    fit_type <- cond_indirect_check_fit(fit)
    if (fit_type == "lavaan" || fit_type == "lavaan.mi") {
        mm <- as.data.frame(lav_data_used(fit))
        w_dat <- as.vector(mm[, w])
        if (length(unique(w_dat)) > 2) {
            return("numeric")
          } else {
            return("categorical")
          }
      }
    if (fit_type == "lm") {
        mm <- merge_model_matrix(fit)
        mf <- merge_model_frame(fit)
        terms_list <- lapply(fit,
                          function(x) stats::delete.response(stats::terms(x)))
        i <- sapply(terms_list, function(x) {any(w %in% all.vars(x))})
        terms_i <- terms_list[[which(i)[1]]]
        w_dc <- attr(terms_i, "dataClasses")[w]
        if (w_dc == "numeric") {
            return("numeric")
          } else {
            return("categorical")
          }
      }
    stop(paste0("Failed to find the type of the moderator ",
                dQuote(w)))
  }