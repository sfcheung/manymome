lav_implied_all <- function(fit) {
    out <- list(cov = lavaan::lavInspect(fit, "cov.all"),
                mean = c(lavaan::lavInspect(fit, "mean.ov"),
                          lavaan::lavInspect(fit, "mean.lv")))
  }
