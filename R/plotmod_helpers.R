
plot_df_meansd_w_numeric <- function(output,
                           x,
                           w,
                           x_levels,
                           w_levels,
                           x_levels_labels,
                           w_levels_labels,
                           other_numeric_on = "mean",
                           other_categorical_on = "reference"
                          ) {
    tmp_lvl <- expand.grid(w_level = w_levels, x_level = x_levels)
    tmp_lbl <- expand.grid(w_label = w_levels_labels, x_label = x_levels_labels)
    mf1 <- stats::model.frame(output)
    mf2 <- data.frame(lapply(mf1, sum_col,
                             other_numeric_on = other_numeric_on,
                             other_categorical_on = other_categorical_on))
    mf2 <- do.call(rbind, replicate(nrow(tmp_lvl), mf2, simplify = FALSE))
    mf2 <- data.frame(x_level = as.character(tmp_lbl$x_label),
                      w_level = as.character(tmp_lbl$w_label),
                      mf2)
    mf2[, x] <- tmp_lvl$x_level
    mf2[, w] <- tmp_lvl$w_level
    mf2
  }


plot_df_meansd_w_categorical <- function(output,
                           x,
                           w,
                           x_levels,
                           w_levels,
                           x_levels_labels,
                           w_levels_labels,
                           other_numeric_on = "mean",
                           other_categorical_on = "reference"
                          ) {
    tmp_lvl <- expand.grid(w_level = w_levels, x_level = x_levels)
    tmp_lbl <- expand.grid(w_label = w_levels, x_label = x_levels_labels)
    mf1 <- stats::model.frame(output)
    mf2 <- data.frame(lapply(mf1, sum_col,
                             other_numeric_on = other_numeric_on,
                             other_categorical_on = other_categorical_on))
    mf2 <- do.call(rbind, replicate(nrow(tmp_lvl), mf2, simplify = FALSE))
    mf2 <- data.frame(x_level = as.character(tmp_lbl$x_label),
                      w_level = as.character(tmp_lbl$w_label),
                      mf2)
    mf2[, x] <- tmp_lvl$x_level
    mf2[, w] <- tmp_lvl$w_level
    mf2
  }

gen_levels <- function(x,
                       method, ...) {
    out <- switch(method,
              sd = gen_levels_sd(x = x, ...),
              percentile = gen_levels_percentiles(x = x, ...)
            )
    return(out)
  }

gen_levels_sd <- function(x,
                          from_mean_in_sd = 1,
                          levels = c(-1, 0, 1),
                          ...) {
    x_mean <- mean(x, na.rm = TRUE)
    x_sd <- stats::sd(x, na.rm = TRUE)
    out <-  x_mean + levels * from_mean_in_sd * x_sd
    out
  }

gen_levels_percentiles <- function(x,
                          sd_to_percentiles = NA,
                          sd_levels = c(-1, 0, 1),
                          percentiles = c(.16, .50, .84),
                          ...) {
    if (is.numeric(sd_to_percentiles)) {
        percentiles <- sapply(sd_levels * sd_to_percentiles, stats::pnorm)
      }
    out <- stats::quantile(x, percentiles, na.rm = TRUE)
    out <- unname(out)
    out
  }

sum_col <- function(x,
                    other_numeric_on = c("mean", "median"),
                    other_categorical_on = c("reference", "modal")
  ) {
    if (missing(other_numeric_on)) other_numeric_on <- "mean"
    if (missing(other_categorical_on)) other_categorical_on <- "reference"
    if (is.numeric(x)) {
        out <- switch(other_numeric_on,
                mean = mean(x, na.rm = TRUE),
                median = stats::median(x, na.rm = TRUE))
        return(out)
      }
    if (is.character(x)) {
        xf <- as.factor(x)
      } else {
        xf <- x
      }
    if (is.factor(xf)) {
        out <- switch(other_categorical_on,
                reference = as.ordered(xf)[1],
                modal = names(which(table(xf) == max(table(xf)))))
        return(out)
      }
  }

is_standardized <- function(x) {
    if (!is.numeric(x)) return(FALSE)
    isTRUE(all.equal(stats::sd(x, na.rm = TRUE), 1)) &&
      isTRUE(all.equal(mean(x, na.rm = TRUE), 0))
  }

find_bs <- function(mf, x, w, w_levels) {
    b_all <- sapply(w_levels, find_b, mf = mf, x = x, w = w)
    b_all
  }

find_b <- function(w_i, mf, x, w) {
    mf_i <- mf[mf[, w] == w_i, ]
    b_i <- (mf_i[mf_i$x_level == "High", "predicted"] -
            mf_i[mf_i$x_level == "Low", "predicted"]) /
          (mf_i[mf_i$x_level == "High", x] -
            mf_i[mf_i$x_level == "Low", x])
    b_i
  }