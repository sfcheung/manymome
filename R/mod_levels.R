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
#' @param fit The fit object. Can be a
#'            [lavaan::lavaan-class] object or a list of [lm()] outputs.
#' @param w Character. The names of the moderator. If the moderator is
#'          categorical with 3 or more groups, this is the vector of the
#'          indicator variables.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x
#' m2 ~ a2 * m1
#' m3 ~ a3 * m2
#' y  ~ a4 * m3 + c4 * x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' check_path(x = "x", y = "m3", m = c("m1", "m2"), fit = fit)
#' check_path(x = "x", y = "y", m = c("m1", "m2"), fit = fit)
#'
#' @export
#'
#'

mod_levels <- function(fit,
                       w) {

  }

mod_levels_i <- function(fit,
                         w,
                         w_type = c("numeric", "categorical"),
                         w_method = c("sd", "percentile"),
                         sd_from_mean = c(-1, 0, 1),
                         percentiles = c(.16, .50, .84),
                         extract_gp_names = TRUE,
                         prefix = NULL) {
    fit_type <- cond_indirect_check_fit(fit)
    w_type <- match.arg(w_type)
    if (fit_type == "lm") {
        if (w_type == "numeric") {
            out <- mod_levels_i_lm_numerical(fit = fit,
                                             w = w,
                                             w_method = w_method,
                                             sd_from_mean = sd_from_mean,
                                             percentiles = percentiles)
          }
        if (w_type == "categorical") {
            out <- mod_levels_i_lm_categorical(fit = fit,
                                               w = w,
                                               extract_gp_names = extract_gp_names,
                                               prefix = prefix)
          }
      }
    if (fit_type == "lavaan") {
        if (w_type == "numeric") {
            out <- mod_levels_i_lavaan_numerical(fit = fit,
                                                 w = w,
                                                 w_method = w_method,
                                                 sd_from_mean = sd_from_mean,
                                                 percentiles = percentiles)
          }
        if (w_type == "categorical") {
            out <- mod_levels_i_lavaan_categorical(fit = fit,
                                                   w = w,
                                                   extract_gp_names = extract_gp_names,
                                                   prefix = prefix)
          }
      }
    out
  }

mod_levels_i_lm <- function(fit,
                            w,
                            w_method = c("sd", "percentile"),
                            sd_from_mean = c(-1, 0, 1),
                            percentiles = c(.16, .50, .84),
                            extract_gp_names = TRUE,
                            prefix = NULL) {

  }

mod_levels_i_lm_numerical <- function(fit,
                                      w,
                                      w_method = c("sd", "percentile"),
                                      sd_from_mean = c(-1, 0, 1),
                                      percentiles = c(.16, .50, .84)) {
    # No need for user-specified method. If users want to specify their own
    # values, they do not need  to call this function
    w_method <- match.arg(w_method)
    mm <- merge_model_matrix(fit)
    w_dat <- mm[, w]
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
        return(levels)
      }
    if (w_method == "percentile") {
        w_q <- quantile(w_dat, probs = percentiles, na.rm = TRUE)
        return(w_q)
      }
  }

mod_levels_i_lm_categorical <- function(fit,
                                        w,
                                        extract_gp_names = TRUE,
                                        prefix = NULL) {
    mm <- merge_model_matrix(fit)
    w_dat <- mm[, w]
    w_gp <- unique(w_dat)
    k <- nrow(w_gp)
    j <- rev(seq_len(ncol(w_gp)))
    i <- do.call(order, w_gp[, j])
    w_gp <- w_gp[i, ]
    gpnames <- paste0("Category ", seq_len(k))
    rownames(w_gp) <- gpnames
    if (extract_gp_names) {
        w_gp <- set_gp_names(w_gp, prefix = prefix)
      }
    return(w_gp)
  }

mod_levels_i_lavaan <- function(fit,
                                w,
                                w_method = c("sd", "percentile"),
                                sd_from_mean = c(-1, 0, 1),
                                percentiles = c(.16, .50, .84),
                                extract_gp_names = TRUE,
                                prefix = NULL) {

  }


mod_levels_i_lavaan_numerical <- function(fit,
                                      w_method = c("sd", "percentile"),
                                      sd_from_mean = c(-1, 0, 1),
                                      percentiles = c(.16, .50, .84),
                                      w) {
    # No need for user-specified method. If users want to specify their own
    # values, they do not need  to call this function
    w_method <- match.arg(w_method)
    mm <- lav_data_used(fit)
    w_dat <- mm[, w]
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
        return(levels)
      }
    if (w_method == "percentile") {
        w_q <- quantile(w_dat, probs = percentiles, na.rm = TRUE)
        return(w_q)
      }
  }

mod_levels_i_lavaan_categorical <- function(fit,
                                            w,
                                            extract_gp_names = TRUE,
                                            prefix = NULL) {
    mm <- as.data.frame(lav_data_used(fit))
    w_dat <- mm[, w]
    w_gp <- unique(w_dat)
    k <- nrow(w_gp)
    j <- rev(seq_len(ncol(w_gp)))
    i <- do.call(order, w_gp[, j])
    w_gp <- w_gp[i, ]
    gpnames <- paste0("Category ", seq_len(k))
    rownames(w_gp) <- gpnames
    if (extract_gp_names) {
        w_gp <- set_gp_names(w_gp, prefix = prefix)
      }
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