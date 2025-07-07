skip("WIP")

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem", {

plot_q <- function(
                x,
                standardized = FALSE,
                size_variables = NULL,
                size_path_labels = NULL,
                nchar_variables = NULL,
                nchar_path_labels = NULL,
                rsquares = TRUE,
                sigs = TRUE,
                margins = c(5, 5, 5, 5),
                v_pos = c("middle", "lower", "upper"),
                v_preference = c("upper", "lower"),
                plot_now = TRUE,
                ...
              ) {

  v_pos <- match.arg(v_pos)
  v_preference <- match.arg(v_preference)

  # TODO:
  # - Handle standardized solution

  # ==== Check package requirements ====

  if (!requireNamespace("semptools", quietly = TRUE)) {
    stop("Please install 'semptools' first.")
  }
  if (!requireNamespace("semPlot", quietly = TRUE)) {
    stop("Please install 'semPlot' first.")
  }

  # ==== Check the fit object ====

  fit <- x$lm_out
  fit_type <- NA
  if (inherits(fit, "lavaan")) {
    fit_type <- "lavaan"
  } else {
    fit_type <- "lm"
  }
  est <- switch(
            fit_type,
            lm = lmhelprs::lm_list_to_partable(fit),
            lavaan = lavaan::parameterEstimates(fit)
          )

  # ==== Standardized Solution ====

  if (standardized) {
    if (fit_type == "lm") {
      est <- add_betaselect_lm_list(
                  fit = fit,
                  ptable = est
                )
    } else if (fit_type == "lavaan") {
      est <- add_betaselect_lav(
                  lm_out_lav = x$lm_out_lav,
                  ptable = est
                )
    }
  }

  # ==== semPlotModel ====
  if (!standardized) {
    pm <- switch(
              fit_type,
              lm = semPlot::semPlotModel(est),
              lavaan = semPlot::semPlotModel(fit)
            )
  } else {
    if (fit_type == "lavaan") {
      est$ustart <- est$est
      tmp <- !is.na(est$z)
      est$free <- 0
      est$free[tmp] <- seq_along(which(tmp))
      est$plabel <- paste0(".p", seq_len(nrow(est)), ".")
    }
    est$est_org <- est$est
    est$est <- est$est.std
    est$ustart <- est$est
    pm <- switch(
              fit_type,
              lm = semPlot::semPlotModel(est),
              lavaan = semPlot::semPlotModel(est)
            )
  }
  fit_x <- x$x
  fit_y <- x$y
  fit_m <- x$m
  fit_cov <- eval(x$call$cov)
  fit_ov <- switch(
              fit_type,
              lm = lavaan::lavNames(est, "ov"),
              lavaan = lavaan::lavNames(fit, "ov")
            )
  fit_ov <- setdiff(fit_ov, fit_cov)
  pm <- semptools::keep_nodes(pm, c(fit_ov))

  # ==== Graphic Settings ====

  if (is.null(size_variables)) {
    sizeMan <- quick_scale(
                  m = fit_ov,
                  val_max = 10,
                  val_min = 7,
                  m_p_max = 1 + 2,
                  m_p_min = 4 + 2
                )
  } else {
    sizeMan <- size_variables
  }

  if (is.null(size_path_labels)) {
    edge.label.cex <- quick_scale(
                    m = fit_ov,
                    val_max = 1.00,
                    val_min = 0.75,
                    m_p_max = 1 + 2,
                    m_p_min = 4 + 2
                  )
  } else {
    edge.label.cex <- size_path_labels
  }

  # ==== Base Plot ====

  p <- semPlot::semPaths(
                object = pm,
                what = "paths",
                whatLabels = "est",
                intercepts = FALSE,
                residuals = rsquares,
                thresholds = FALSE,
                nCharNodes = ifelse(is.null(nchar_variables), 0, nchar_variables),
                nCharEdges = ifelse(is.null(nchar_path_labels), 0, nchar_path_labels),
                sizeMan = sizeMan,
                edge.label.cex = edge.label.cex,
                mar = margins,
                ...,
                DoNotPlot = TRUE
              )

  # ==== R-squares ====

  if (rsquares) {
    rsq_est <- switch(
                fit_type,
                lm = rsq_to_ptable(fit),
                lavaan = rsq_to_ptable(x$lm_out_lav)
              )
    p <- switch(
            fit_type,
            lm = semptools::add_rsq(
                      p,
                      ests = rsq_est
                    ),
            lavaan = semptools::add_rsq(
                      p,
                      object = fit
                    )
          )
  } else {
    rsq_est <- NULL
  }

  # ==== Layout ====

  p <- semptools::auto_layout_mediation(
            p,
            v_pos = v_pos,
            v_preference = v_preference
          )
  p <- semptools::safe_edge_label_position(p)
  p <- semptools::safe_resid_position(p)

  # ==== Sig. Stars ====

  if (sigs) {
    p <- switch(
          fit_type,
          lm = semptools::mark_sig(
            p,
            ests = est,
            ests_r2 = rsq_est
          ),
          lavaan = semptools::mark_sig(
              p,
              object = fit,
              ests_r2 = rsq_est
            )
        )
  }

  # ==== Plot? ====

  if (plot_now) {
    plot(p)
  } else {
    return(p)
  }
}

# Input:
# - A special type of output based on lavaan output
# Output:
# - A lavaan parameter table with 'est.std' added
add_betaselect_lav <- function(
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

# Input:
# - lm_out_lav
# Output:
# - A lavaan parameter table with 'est.std'
lm_out_lav_betaselect <- function(
                    lm_out_lav
                  ) {
  betas <- lapply(
              lm_out_lav,
              \(x) x$coefs_lm[, "betaS"]
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
                  \(x) {names(x$term_types)[x$term_types == "numeric"]}
                )
  std_names <- unname(unique(unlist(std_names)))
  out <- do.call(rbind,
                 lor)
  attr(out, "standardized") <- std_names
  attr(out, "y") <- y
  out
}

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

# Input:
# - An lm_list object
# Output:
# - A parameter table with R-squares and
#   their p-values, if available.
rsq_to_ptable_lm_list <- function(lm_list) {
  lm_summary <- sapply(
                    lm_list,
                    \(x) summary(x),
                    simplify = FALSE,
                    USE.NAMES = TRUE
                  )
  lm_rsq <- sapply(
              lm_summary,
              \(x) x$r.squared
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
              \(x) f(x$fstatistic)
            )
  out <- data.frame(lhs = names(lm_rsq),
                    op = "r2",
                    rhs = names(lm_rsq),
                    est = lm_rsq,
                    pvalue = lm_rsq_p)
  rownames(out) <- names(lm_rsq)
  out
}

# Input:
# - A special form of lavaan output by
#   q-function
# Output:
# - A parameter table with R-squares and
#   their p-values, if available.
rsq_to_ptable_lav <- function(out_lav) {
  rsq_test <- sapply(
                out_lav,
                \(x) x$rsq_test
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


out0 <- q_simple_mediation(
            x = "x",
            y = "y",
            m = "m",
            cov = c("c2", "c1"),
            data = data_med,
            ci_type = "mc",
            R = 200,
            seed = 1234,
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
plot_q(out0)
plot_q(out0,
       standardized = TRUE)

outs <- q_serial_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_serial,
            ci_type = "mc",
            R = 100,
            seed = 1234,
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outs)
plot_q(outs,
       rsquares = FALSE)
plot_q(outs,
       standardized = TRUE)



outp <- q_parallel_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_parallel,
            fit_method = "sem",
            ci_type = "mc",
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outp)
plot_q(outp,
       standardized = TRUE)

outm1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m2 -> y1",
                      "x1 -> m11 -> m12 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            ci_type = "mc",
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm1)


outm2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> y1"),
            cov = c("c1", "c2"),
            fit_method = "sem",
            data = data_med_complicated,
            R = 200,
            ci_type = "mc",
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm2)
plot_q(outm2,
       standardized = TRUE)


outm3 <- q_mediation(
            x = "x2",
            y = "c1",
            model = c("x2 -> m2 -> m11 -> y1",
                      "x2 -> m2 -> m12 -> y1",
                      "m2 -> y1",
                      "y1 -> c1"),
            cov = c("c2"),
            fit_method = "sem",
            data = data_med_complicated,
            R = 200,
            ci_type = "mc",
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm3)
plot_q(outm3,
       standardized = TRUE)

# lm

out0 <- q_simple_mediation(
            x = "x",
            y = "y",
            m = "m",
            cov = c("c2", "c1"),
            data = data_med,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(out0)
plot_q(out0,
       standardized = TRUE)


outs <- q_serial_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_serial,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outs)
plot_q(outs,
       standardized = TRUE)

outp <- q_parallel_mediation(
            x = "x",
            y = "y",
            m = c("m1", "m2"),
            cov = c("c2", "c1"),
            data = data_parallel,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outp)
plot_q(outp,
       standardized = TRUE)

outm1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m2 -> y1",
                      "x1 -> m11 -> m12 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm1)
plot_q(outm1,
       standardized = TRUE)

outm2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm2)
plot_q(outm2,
       standardized = TRUE)

outm3 <- q_mediation(
            x = "x2",
            y = "c1",
            model = c("x2 -> m2 -> m11 -> y1",
                      "x2 -> m2 -> m12 -> y1",
                      "m2 -> y1",
                      "y1 -> c1"),
            cov = c("c2"),
            data = data_med_complicated,
            R = 100,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
plot_q(outm3)
plot_q(outm3,
       standardized = TRUE)

})

