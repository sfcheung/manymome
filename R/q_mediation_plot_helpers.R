#' @noRd
# Input:
# - A special type of output based on lavaan output
# Output:
# - A lavaan parameter table with 'est.std' added
add_betaselect_lm_out_lav <- function(
                    lm_out_lav,
                    ptable
                  ) {
  est_std <- lm_out_lav_betaselect(lm_out_lav)
  out <- merge(
            x = ptable,
            y = est_std,
            by = c("lhs", "op", "rhs"),
            all.x = TRUE,
            all.y = FALSE,
            sort = FALSE)
  std_names <- attr(est_std, "standardized")
  y <- attr(est_std, "y")
  x_std <- setdiff(std_names, y)
  i <- (out$lhs %in% x_std) &
       (out$op == "~~") &
       (out$rhs %in% x_std) &
       (out$rhs == out$lhs)
  out[i, "est.std"] <- 1
  out
}

#' @noRd
# Input:
# - An lm_list object
# Output:
# - A lavaan parameter table with 'est.std' added
add_betaselect_lm_list <- function(
                    fit,
                    ptable
                  ) {
  est_std <- lm_list_betaselect(fit)
  out <- merge(
            x = ptable,
            y = est_std,
            by = c("lhs", "op", "rhs"),
            all.x = TRUE,
            all.y = FALSE,
            sort = FALSE)
  std_names <- attr(est_std, "standardized")
  y <- attr(est_std, "y")
  x_std <- setdiff(std_names, y)
  i <- (out$lhs %in% x_std) &
       (out$op == "~~") &
       (out$rhs %in% x_std) &
       (out$rhs == out$lhs)
  out[i, "est.std"] <- 1
  out
}

#' @noRd

# Input:
# - lm_out_lav
# Output:
# - A lavaan parameter table with 'est.std'
lm_out_lav_betaselect <- function(
                    lm_out_lav
                  ) {
  betas <- lapply(
              lm_out_lav,
              function(x) {x$coefs_lm[, "betaS"]}
            )
  y <- names(betas)
  f <- function(z) {
      betas_i <- betas[[z]]
      lhs <- z
      op <- "~"
      rhs <- names(betas_i)
      out <- data.frame(
                lhs = lhs,
                op = "~",
                rhs = rhs,
                est.std = betas_i
              )
      i <- match("(Intercept)", rhs)
      out[i, "rhs"] <- ""
      out[i, "op"] <- "~1"
      rownames(out) <- NULL
      out
    }
  lor <- lapply(
            names(betas),
            f
          )
  std_names <- lapply(
                  lm_out_lav,
                  function(x) {names(x$term_types)[x$term_types == "numeric"]}
                )
  std_names <- unname(unique(unlist(std_names)))
  out <- do.call(rbind,
                 lor)
  attr(out, "standardized") <- std_names
  attr(out, "y") <- y
  out
}

#' @noRd

# Input:
# - An lm_list object
# Output:
# - A lavaan parameter table with 'est.std'
lm_list_betaselect <- function(
                    fit
                  ) {
  betas <- lapply(
              fit,
              std_numeric
            )
  y <- names(betas)
  f <- function(z) {
      betas_i <- betas[[z]]
      lhs <- z
      op <- "~"
      rhs <- names(betas_i)
      out <- data.frame(
                lhs = lhs,
                op = "~",
                rhs = rhs,
                est.std = betas_i
              )
      i <- match("(Intercept)", rhs)
      out[i, "rhs"] <- ""
      out[i, "op"] <- "~1"
      rownames(out) <- NULL
      out
    }
  lor <- lapply(
            names(betas),
            f
          )
  std_names <- lapply(
                  betas,
                  attr,
                  which = "standardized"
                )
  std_names <- unname(unique(unlist(std_names)))
  out <- do.call(rbind,
                 lor)
  attr(out, "standardized") <- std_names
  attr(out, "y") <- y
  out
}

#' @noRd
# Input:
# - A special form of lavaan output by
#   q-function, or lm_list object
# Output:
# - A parameter table with R-squares and
#   their p-values, if available.
rsq_to_ptable <- function(object) {
  if (inherits(object, "lm_list")) {
    return(rsq_to_ptable_lm_list(object))
  } else if (is.list(object)) {
    return(rsq_to_ptable_lav(object))
  } else {
    return(NA)
  }
}

#' @noRd
# Input:
# - An lm_list object
# Output:
# - A parameter table with R-squares and
#   their p-values, if available.
rsq_to_ptable_lm_list <- function(lm_list) {
  lm_summary <- sapply(
                    lm_list,
                    function(x) {summary(x)},
                    simplify = FALSE,
                    USE.NAMES = TRUE
                  )
  lm_rsq <- sapply(
              lm_summary,
              function(x) {x$r.squared}
            )
  f <- function(y) {
    unname(stats::pf(
              y["value"],
              y["numdf"],
              y["dendf"],
              lower.tail = FALSE))
  }
  lm_rsq_p <- sapply(
              lm_summary,
              function(x) {f(x$fstatistic)}
            )
  out <- data.frame(lhs = names(lm_rsq),
                    op = "r2",
                    rhs = names(lm_rsq),
                    est = lm_rsq,
                    pvalue = lm_rsq_p)
  rownames(out) <- names(lm_rsq)
  out
}

#' @noRd
# Input:
# - A special form of lavaan output by
#   q-function
# Output:
# - A parameter table with R-squares and
#   their p-values, if available.
rsq_to_ptable_lav <- function(out_lav) {
  rsq_test <- sapply(
                out_lav,
                function(x) {x$rsq_test}
              )
  out <- data.frame(lhs = names(rsq_test),
                    op = "r2",
                    rhs = names(rsq_test),
                    pvalue = rsq_test)
  rownames(out) <- names(rsq_test)
  out
}

#' @noRd
# Adapted from semptools
# sizeMan = 10,
# sizeLat = 10,
# edge.label.cex = 1.25,
# sizeMan = 8,
# sizeLat = 8,
# edge.label.cex = .80,
quick_scale <- function(
                  m,
                  val_max = 10,
                  val_min = 8,
                  m_p_max = 1,
                  m_p_min = 4
                ) {
  m_p <- length(m)
  a <- max(val_min,
           val_min + (val_max - val_min) * (m_p_min - m_p) / (m_p_min - m_p_max),
           na.rm = TRUE)
  a
}
