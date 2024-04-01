x_for_wlevels <- function(wlevels_i, mf, x, band = .16, nvalues = 2) {
    k <- ncol(wlevels_i)
    ws <- colnames(wlevels_i)
    wtypes <- sapply(ws, check_w_type, mf = mf, nvalues = nvalues)
    x_ids <- mapply(x_for_w,
                   w_i = wlevels_i,
                   w = ws,
                   w_numeric = (wtypes == "numeric"),
                   MoreArgs = list(mf = mf,
                                   x = x,
                                   row_id = TRUE,
                                   band = band)
                   )
    x_id <- apply(x_ids, 1, all)
    mf[x_id, x]
  }

check_w_type <- function(w, mf, nvalues = 2) {
    if (!is.numeric(mf[, w])) {
        return("categorical")
      }
    tmp <- length(unique(mf[, w]))
    if (tmp > nvalues) {
        return("numeric")
      } else {
        return("categorical")
      }
  }

x_for_w <- function(w_i, mf, x, w, w_numeric, ...) {
    if (w_numeric) {
        return(x_for_w_numeric(w_i = w_i,
                               mf = mf,
                               x = x,
                               w = w,
                               ...))
      } else {
        return(x_for_w_categorical(w_i = w_i,
                               mf = mf,
                               x = x,
                               w = w,
                               ...))
      }
  }

x_for_w_numeric <- function(w_i, mf, x, w, band = .16, row_id = FALSE) {
    w0 <- mf[, w]
    w_ecdf <- stats::ecdf(w0)
    w_i_p <- w_ecdf(w_i)
    w_i_r <- c(w_i_p - band, w_i_p + band)
    w_i_r[1] <- max(w_i_r[1], 0)
    w_i_r[2] <- min(w_i_r[2], 1)
    w_i_b <- stats::quantile(w0, w_i_r, na.rm = TRUE, names = FALSE)
    x_id <- (w0 >= w_i_b[1]) & (w0 <= w_i_b[2])
    if (row_id) {
        return(x_id)
      } else {
        x1 <- mf[, x]
        return(x1)
      }
  }

x_for_w_categorical <- function(w_i, mf, x, w, row_id = FALSE, ...) {
    w0 <- mf[, w]
    if (row_id) {
        return(w0 == w_i)
      } else {
        x1 <- mf[w0 == w_i, x]
        return(x1)
      }
  }

#' @noRd
# Rescale data such that the means and SDs are those required

scale_by_implied <- function(data_original, implied) {
    data_new <- data_original
    for (x in colnames(data_original)) {
        raw_v <- data_original[, x]
        mean_v <- mean(raw_v)
        sd_v <- stats::sd(raw_v)
        sd_v_implied <- sqrt(implied$cov[x, x])
        if (!(all(is.na(implied$mean)) || is.null(implied$mean))) {
            mean_v_implied <- implied$mean[x]
          } else {
            mean_v_implied <- mean_v
          }
        z_v <- scale(data_original[, x])[, 1]
        new_v <- z_v * sd_v_implied + mean_v_implied
        data_new[, x] <- new_v
      }
    data_new
  }