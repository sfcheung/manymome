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

# Input:
# - indirect
# Output:
# - A character vector
# Limitations:
# - Single-group only
total_indirect_to_note <- function(
                      ind_out,
                      digits = 3,
                      ci = TRUE,
                      pvalue = TRUE) {
  x <- ind_out$x
  y <- ind_out$y
  s_path <- paste0(
              "Total indirect effect of ",
              x,
              " on ",
              y)
  tmp <- unname(stats::coef(ind_out))
  s_est <- unname(formatC(
            tmp,
            digits = digits,
            format = "f"
          ))
  if (ci) {
    s_ci <- try(stats::confint(ind_out)[1, , drop = TRUE], silent = TRUE)
    if (inherits(s_ci, "try-error") ||
        all(is.na(s_ci))) {
      ci <- FALSE
      s_ci <- NULL
    } else {
      s_ci <- formatC(
                  s_ci,
                  digits = digits,
                  format = "f"
                )
      s_ci <- paste0(unname(s_ci), collapse = ", ")
      s_ci <- paste0("[", s_ci, "]")
    }
  } else {
    s_ci <- NULL
  }
  if (pvalue &&
      (!is.null(ind_out$boot_p))) {
    s_p <- ind_out$boot_p
    if (s_p < .001) {
      s_p <- "italic(p), '< .001'"
    } else {
      s_p <- formatC(s_p, digits = digits, format = "f")
      s_p <- paste0("italic(p), ' = ", s_p, "'")
    }
  } else {
    s_p <- NULL
  }
  s_final <- paste0(c(
                "paste('",
                s_path,
                ": ",
                s_est,
                ifelse(!is.null(s_ci), paste0(" ", s_ci, " "), ""),
                "'",
                ifelse(!is.null(s_p), paste0(", ', ',", s_p), ""),
                ")"),
                collapse = "")
  names(s_final) <- s_path
  s_final
}


# Input:
# - indirect_list
# Output:
# - A character vector
# Limitations:
# - Single-group only
indirect_list_to_note <- function(
                      ind_out,
                      digits = 3,
                      ci = TRUE,
                      pvalue = TRUE) {
  out0 <- indirect_effects_from_list(
              ind_out,
              add_sig = TRUE,
              pvalue = TRUE,
              se = TRUE)
  out <- character(0)
  for (j in seq_len(nrow(out0))) {
    out_i <- out0[j, , drop = FALSE]
    s_path <- rownames(out_i)
    s_path0 <- strsplit(s_path, "->", fixed = TRUE)[[1]]
    s_path0 <- sapply(s_path0, trimws, USE.NAMES = FALSE)
    s_path1 <- sapply(s_path0, sQuote, USE.NAMES = FALSE)
    s_path1 <- paste0(s_path1, collapse = " %->% ")
    tmp <- stats::coef(ind_out[[j]])
    s_est <- unname(formatC(
              tmp,
              digits = digits,
              format = "f"
            ))
    if (ci) {
      s_ci <- try(stats::confint(ind_out[[j]])[1, , drop = TRUE], silent = TRUE)
      if (inherits(s_ci, "try-error") ||
          all(is.na(s_ci))) {
        ci <- FALSE
        s_ci <- NULL
      } else {
        s_ci <- formatC(
                    s_ci,
                    digits = digits,
                    format = "f"
                  )
        s_ci <- paste0(unname(s_ci), collapse = ", ")
        s_ci <- paste0("[", s_ci, "]")
      }
    } else {
      s_ci <- NULL
    }
    if (pvalue &&
        ("pvalue" %in% colnames(out_i))) {
      s_p <- out_i[, "pvalue", drop = TRUE]
      if (s_p < .001) {
        s_p <- "italic(p), '< .001'"
      } else {
        s_p <- formatC(s_p, digits = digits, format = "f")
        s_p <- paste0("italic(p), ' = ", s_p, "'")
      }
    } else {
      s_p <- NULL
    }
    # s_final <- paste0(c(
    #               s_est,
    #               ifelse(!is.null(s_ci), paste0(" ", s_ci), ""),
    #               ifelse(!is.null(s_p), paste0(", ", s_p), "")
    #               ),
    #               collapse = "")
    s_final <- paste0(c(
                  "paste(",
                  s_path1,
                  ",': ",
                  s_est,
                  ifelse(!is.null(s_ci), paste0(" ", s_ci, " "), ""),
                  "'",
                  ifelse(!is.null(s_p), paste0(", ', ',", s_p), ""),
                  ")"),
                  collapse = "")
    names(s_final) <- s_path
    out <- c(out, s_final)
  }
  out
}

# Input:
# - A named vector generated by indirect_list_to_note()
# Output:
# - A character vector to be printed.
text_indirect_list <- function(
                        object,
                        side = 1,
                        start_at = 1,
                        ...) {
  line_i <- start_at
  for (i in seq_along(object)) {
    object_e <- parse(text = object[i])
    mtext(object_e,
          side = side,
          line = line_i,
          ...)
    line_i <- line_i + 1
  }
}

# Input:
# - A string vector generated by indirect_to_note()
# Output:
# - A character vector to be printed.
text_total_indirect <- function(
                    object,
                    side = 1,
                    ...
                  ) {
  object_e <- parse(text = object)
  mtext(object_e,
        side = side,
        ...)
}

# Input:
# - A qgraph
# Output:
# - Are there nodes close to the center
#   bottom with residuals below them?
node_below <- function(object,
                       y_margin = -.9,
                       x_margin = .5,
                       angle_margin = .5) {
  layout <- object$layout
  nodes <- object$graphAttributes$Nodes
  lr <- nodes$loopRotation
  iy <- layout[, 2, drop =  TRUE] <= y_margin
  ix <- abs(layout[, 1, drop =  TRUE]) <= x_margin
  i <- iy & ix
  if (!any(i)) {
    return(FALSE)
  }
  j <- (abs(lr[i]) / pi) >= angle_margin
  if (any(j)) {
    return(TRUE)
  }
  return(FALSE)
}