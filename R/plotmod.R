#' @title Plot Conditional Effects
#'
#' @description Plot the conditional
#' effects for different levels of
#' moderators.
#'
#' @details This function is a plot
#' method of the output of
#' [cond_indirect_effects()]. It will
#' use the levels of moderators in the
#' output.
#'
#' It plots the conditional effect from
#' `x` to `y` in a model for different
#' levels of the moderators. For
#' multigroup models, the group will
#' be the 'moderator' and one line is
#' drawn for each group.
#'
#' It does not support conditional
#' indirect effects. If there is one or
#' more mediators in `x`, it will raise
#' an error.
#'
#' ## Multigroup Models
#'
#' Since Version 0.1.14.2, support for
#' multigroup models has been added for models
#' fitted by `lavaan`. If the effect
#' for each group is drawn, the
#' `graph_type` is automatically switched
#' to `"tumble"` and the means and SDs
#' in each group will be used to determine
#' the locations of the points.
#'
#' If the multigroup model has any equality
#' constraints, the implied means and/or
#' SDs may be different from those of
#' the raw data. For example, the mean
#' of the `x`-variable may be constrained
#' to be equal in this model. To plot
#' the tumble graph using the model implied
#' means and SDs, set `use_implied_stats`
#' to `TRUE`.
#'
#' ## Latent Variables
#'
#' A path that involves a latent `x`-variable
#' and/or a latent `y`-variable can be
#' plotted. Because the latent variables
#' have no observed data, the model
#' implied statistics will always be used
#' to get the means and SDs to compute
#' values such as the low and high points
#' of the `x`-variable.
#'
#' @return A `ggplot2` graph. Plotted if
#' not assigned to a name. It can be
#' further modified like a usual
#' `ggplot2` graph.
#'
#' @param x The output of
#' [cond_indirect_effects()]. (Named `x`
#' because it is required in the naming
#' of arguments of the `plot` generic
#' function.)
#'
#' @param x_label The label for the
#' X-axis. Default is the value of the
#' predictor in the output of
#' [cond_indirect_effects()].
#'
#' @param w_label The label for the
#' legend for the lines. Default is
#' `"Moderator(s)"`.
#'
#' @param y_label The label for the
#' Y-axis. Default is the name of the
#' response variable in the model.
#'
#' @param title The title of the graph.
#' If not supplied, it will be generated
#' from the variable names or labels (in
#' `x_label`, `y_label`, and `w_label`).
#' If `""`, no title will be printed.
#' This can be used when the plot is for
#' manuscript submission and figures are
#' required to have no titles.
#'
#' @param x_from_mean_in_sd How many SD
#' from mean is used to define "low" and
#' "high" for the focal variable.
#' Default is 1.
#'
#' @param x_method How to define "high"
#' and "low" for the focal variable
#' levels. Default is in terms of the
#' standard deviation of the focal
#' variable, `"sd"`. If equal to
#' `"percentile"`, then the percentiles
#' of the focal variable in the dataset
#' is used. If the focal variable is
#' a latent variable, only
#' `"sd"` can be used.
#'
#' @param x_percentiles If `x_method` is
#' `"percentile"`, then this argument
#' specifies the two percentiles to be
#' used, divided by 100. It must be a
#'   vector of two numbers. The default
#' is `c(.16, .84)`, the 16th and 84th
#' percentiles, which corresponds
#' approximately to one SD below and
#' above mean for a normal distribution,
#' respectively.
#'
#' @param x_sd_to_percentiles If
#' `x_method` is `"percentile"` and this
#' argument is set to a number, this
#' number will be used to determine the
#' percentiles to be used. The lower
#' percentile is the percentile in a
#' normal distribution that is
#' `x_sd_to_percentiles` SD below the
#' mean. The upper percentile is the
#' percentile in a normal distribution
#' that is `x_sd_to_percentiles` SD
#' above the mean. Therefore, if
#' `x_sd_to_percentiles` is set to 1,
#' then the lower and upper percentiles
#' are 16th and 84th, respectively.
#' Default is `NA`.
#'
#' @param note_standardized If `TRUE`,
#' will check whether a variable has SD
#' nearly equal to one. If yes, will
#' report this in the plot. Default is
#' `TRUE`.
#'
#' @param no_title If `TRUE`, title will
#' be suppressed. Default is `FALSE`.
#'
#' @param line_width The width of the
#' lines as used in
#' [ggplot2::geom_segment()]. Default is
#' 1.
#'
#' @param point_size The size of the
#' points as used in
#' [ggplot2::geom_point()]. Default is
#' 5.
#'
#' @param graph_type If `"default"`, the
#' typical line-graph with equal
#' end-points will be plotted. If
#' `"tumble"`, then the tumble graph
#' proposed by Bodner (2016) will be
#' plotted. Default is `"default"`
#' for single-group models, and
#' `"tumble"` for multigroup models.
#'
#' @param use_implied_stats For a
#' multigroup model, if `TRUE`,
#' the default,
#' model implied statistics will be
#' used in computing the means and SDs,
#' which take into equality constraints,
#' if any.
#' If `FALSE`, then the raw
#' data is
#' used to compute the means and SDs.
#' For latent variables, model implied
#' statistics are always used.
#'
#' @param ... Additional arguments.
#' Ignored.
#'
#'
#' @seealso [cond_indirect_effects()]
#'
#' @references
#'
#' Bodner, T. E. (2016). Tumble graphs: Avoiding misleading end point
#' extrapolation when graphing interactions from a moderated multiple
#' regression analysis. *Journal of Educational and Behavioral
#' Statistics, 41*(6), 593-604. \doi{10.3102/1076998616657080}
#'
#' @examples
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' n <- nrow(dat)
#' set.seed(860314)
#' dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
#' dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
#'
#' # Categorical moderator
#'
#' mod <-
#' "
#' m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
#' y ~ m2 + m3 + x
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
#' out_mm_1 <- mod_levels(c("gpgp2", "gpgp3"),
#'                        sd_from_mean = c(-1, 1),
#'                        fit = fit)
#' out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "m3", fit = fit)
#' plot(out_1)
#' plot(out_1, graph_type = "tumble")
#'
#' # Numeric moderator
#'
#' dat <- modmed_x1m3w4y1
#' mod2 <-
#' "
#' m3 ~ m1 + x + w1 + x:w1
#' y ~ m3 + x
#' "
#' fit2 <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE)
#' out_mm_2 <- mod_levels("w1",
#'                        w_method = "percentile",
#'                        percentiles = c(.16, .84),
#'                        fit = fit2)
#' out_mm_2
#' out_2 <- cond_indirect_effects(wlevels = out_mm_2, x = "x", y = "m3", fit = fit2)
#' plot(out_2)
#' plot(out_2, graph_type = "tumble")
#'
#' # Multigroup models
#'
#' dat <- data_med_mg
#' mod <-
#' "
#' m ~ x + c1 + c2
#' y ~ m + x + c1 + c2
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE,
#'            group = "group")
#'
#' # For a multigroup model, group will be used as
#' # a moderator
#' out <- cond_indirect_effects(x = "m",
#'                              y = "y",
#'                              fit = fit)
#' out
#' plot(out)
#'
#'
#'
#' @export

plot.cond_indirect_effects <- function(
                            x,
                            x_label,
                            w_label = "Moderator(s)",
                            y_label,
                            title,
                            x_from_mean_in_sd = 1,
                            x_method = c("sd", "percentile"),
                            x_percentiles = c(.16, .84),
                            x_sd_to_percentiles= NA,
                            note_standardized = TRUE,
                            no_title = FALSE,
                            line_width = 1,
                            point_size = 5,
                            graph_type = c("default", "tumble"),
                            use_implied_stats = TRUE,
                            ...
                    ) {
    has_groups <- cond_indirect_effects_has_groups(x)
    has_wlevels <- cond_indirect_effects_has_wlevels(x)
    if (has_wlevels && has_groups) {
        stop("Objects with both wlevels and groups not yet supported")
      }
    output <- x
    if (has_groups) {
        tmp <- group_labels_and_numbers_cond(output)
        group_labels <- tmp$label
        group_numbers <- tmp$number
        w_label <- ifelse(w_label == "Moderator(s)",
                          "Group",
                          w_label)
      } else {
        group_labels <- NULL
        group_numbers <- NULL
      }
    fit <- attr(output, "fit")
    fit_type <- cond_indirect_check_fit(fit)
    tmp <- cond_indirect_effects_has_x_y(x)
    x_latent <- tmp$x_latent
    y_latent <- tmp$y_latent
    latent_vars <- switch(fit_type,
                          lavaan = lavaan::lavNames(fit, "lv"),
                          lavaan.mi = lavaan::lavNames(fit, "lv"),
                          lm = character(0))
    has_latent <- (length(latent_vars) > 0)
    x_method <- match.arg(x_method)
    if ((x_method == "percentile") && x_latent) {
        stop("x_method cannot be 'percentile' if x is a latent variable.")
      }
    graph_type <- match.arg(graph_type)
    graph_type_original <- graph_type
    if (has_groups && graph_type == "default") {
        # warning("Only tumble graph is supported for multiple group models. ",
        #         "Changed graph_type to 'tumble'.")
        graph_type <- "tumble"
      }
    full_output <- attr(output, "full_output")
    full_output_1 <- full_output[[1]]
    x <- full_output_1$x
    y <- full_output_1$y
    m <- full_output_1$m
    if (!is.null(m)) {
        stop("The plot method does not support indirect effects.")
      }
    if (has_wlevels) {
        wlevels <- attr(output, "wlevels")
        if (fit_type == "lm") {
            wlevels <- ind_to_cat(wlevels)
          }
        w_names <- colnames(wlevels)
      }
    # mf0 is a list of datasets for multiple group models
    # TODO:
    # - Add support for paths involving latent variables
    mf0 <- switch(fit_type,
                  lavaan = lavaan::lavInspect(fit, "data"),
                  lavaan.mi = lav_data_used(fit, drop_colon = FALSE),
                  lm = merge_model_frame(fit))
    if (has_groups && use_implied_stats) {
        fit_implied_stats <- lavaan::lavInspect(fit, "implied")
        fit_implied_stats <- fit_implied_stats[names(mf0)]
        mf0 <- mapply(scale_by_implied,
                      data_original = mf0,
                      implied = fit_implied_stats,
                      SIMPLIFY = FALSE)
      }
    # Add fill-in data if latent variables are present.
    # The covariance structure with observed variables is not maintained but
    # this is not an issue because only univariate means and SDs are used.
    if (has_latent && (fit_type == "lavaan")) {
        cov_lv <- lavaan::lavInspect(fit, "cov.lv")
        mean_lv <- lavaan::lavInspect(fit, "mean.lv")
        if (has_groups) {
            cov_lv <- cov_lv[names(mf0)]
            mean_lv <- mean_lv[names(mf0)]
            mf0 <- mapply(add_fillin_lv,
                          data_original = mf0,
                          cov_lv = cov_lv,
                          mean_lv = mean_lv,
                          SIMPLIFY = FALSE)
          } else {
            mf0 <- add_fillin_lv(mf0,
                                 cov_lv = cov_lv,
                                 mean_lv = mean_lv)
          }
      }
    fit_list <- switch(fit_type,
                       lavaan = lm_from_lavaan_list(fit),
                       lavaan.mi = lm_from_lavaan_list(fit),
                       lm = fit)
    if ((fit_type == "lm") && !inherits(fit_list, "lm_list")) {
        fit_list <- lm2list(fit_list)
      }
    # dat0 <- switch(fit_type,
    #               lavaan = lavaan::lavInspect(fit, "data"),
    #               lavaan.mi = lav_data_used(fit, drop_colon = FALSE),
    #               lm = merge_model_frame(fit))
    dat0 <- mf0
    x_numeric <- TRUE
    if (!x_numeric) {
        stop("x variable must be a numeric variable.")
      }
    if (graph_type == "default") {
        x_levels <- gen_levels(mf0[, x],
                              method = x_method,
                              from_mean_in_sd = x_from_mean_in_sd,
                              levels = c(-1, 1),
                              sd_levels = c(-1, 1),
                              sd_to_percentiles = x_sd_to_percentiles,
                              percentiles = x_percentiles)
        x_levels_list <- replicate(nrow(wlevels), x_levels, simplify = FALSE)
      }
    if (graph_type == "tumble") {
        if (has_wlevels && !has_groups) {
            x_subsets <- lapply(split(wlevels, seq_len(nrow(wlevels))),
                                x_for_wlevels,
                                mf = mf0, x = x)
          }
        if (!has_wlevels && has_groups) {
            x_subsets <- lapply(mf0, function(xx) {
                              xx[, x, drop = TRUE]
                            })
          }
        if (has_wlevels && has_groups) {
            # TODO
            # - Support objects with both wlevels and groups.
          }
        x_levels_list <- lapply(x_subsets,
                                gen_levels,
                                method = x_method,
                                from_mean_in_sd = x_from_mean_in_sd,
                                levels = c(-1, 1),
                                sd_levels = c(-1, 1),
                                sd_to_percentiles = x_sd_to_percentiles,
                                percentiles = x_percentiles)
      }
    x_levels_m <- do.call(rbind, x_levels_list)
    plot_df_xstart <- data.frame(x = x_levels_m[, 1])
    plot_df_xend <- data.frame(x = x_levels_m[, 2])
    if (has_groups) {
        # mf2 has rows equal to ngroups
        mf2 <- lapply(dat0, function(xx) {
                    data.frame(lapply(as.data.frame(xx), sum_col),
                          check.names = FALSE)
                  })
        mf2 <- lapply(mf2, function(xx) {
                    xx[, -(which(colnames(xx) %in% c(x)))]
                  })
        mf2 <- do.call(rbind, mf2)
      } else {
        # mf2 has one single row
        # TO-THINK:
        # - Maybe we can use different values of other variables
        mf2 <- data.frame(lapply(as.data.frame(dat0), sum_col),
                          check.names = FALSE)
        mf2 <- mf2[, -(which(colnames(mf2) %in% c(x, w_names)))]
      }
    if (has_groups) {
        plot_df_xstart <- cbind(plot_df_xstart, mf2)
        plot_df_xend <- cbind(plot_df_xend, mf2)
      } else {
        plot_df_xstart <- cbind(plot_df_xstart, wlevels, mf2)
        plot_df_xend <- cbind(plot_df_xend, wlevels, mf2)
      }
    colnames(plot_df_xstart)[1] <- x
    colnames(plot_df_xend)[1] <- x
    if (has_groups) {
        plot_df_xstart_i <- split(plot_df_xstart,
                                  seq_len(nrow(plot_df_xstart)),
                                  drop = FALSE)
        plot_df_xend_i <- split(plot_df_xend,
                                seq_len(nrow(plot_df_xend)),
                                drop = FALSE)
        plot_df_xstart[, y] <- mapply(stats::predict,
                                  object = fit_list,
                                  newdata = plot_df_xstart_i,
                                  MoreArgs = list(x = x, y = y, m = m))
        plot_df_xend[, y] <- mapply(stats::predict,
                                  object = fit_list,
                                  newdata = plot_df_xend_i,
                                  MoreArgs = list(x = x, y = y, m = m))
      } else {
        plot_df_xstart[, y] <- stats::predict(fit_list,
                                              x = x, y = y, m = m,
                                              newdata = plot_df_xstart)
        plot_df_xend[, y] <- stats::predict(fit_list,
                                            x = x, y = y, m = m,
                                            newdata = plot_df_xend)
      }

    if (missing(x_label)) x_label <- x
    if (missing(y_label)) y_label <- y

    x_standardized <- full_output_1$standardized_x
    y_standardized <- full_output_1$standardized_y

    if (x_standardized || y_standardized) {
        implied_stats <- switch(fit_type,
                          lavaan = lavaan::lavInspect(fit, "implied"),
                          lavaan.mi = lav_implied_all(fit),
                          lm = lm2ptable(fit)$implied_stats)
        if (has_latent) {
            # Cannot use "cov.all" because we also need the mean vectors.
            # numeric(0) if an element is not present.
            cov_lv <- lavaan::lavInspect(fit, "cov.lv")
            mean_lv <- lavaan::lavInspect(fit, "mean.lv")
            if (has_groups) {
                implied_stats <- mapply(add_lv_implied,
                                   implied_stats = implied_stats,
                                   cov_lv = cov_lv,
                                   mean_lv = mean_lv,
                                   SIMPLIFY = FALSE)
              } else {
                implied_stats <- add_lv_implied(
                                   implied_stats = implied_stats,
                                   cov_lv = cov_lv,
                                   mean_lv = mean_lv)
              }
          }
      }
    if (x_standardized) {
        if (has_groups) {
            # x_sd and x_mean are vectors if ngroups > 1
            group_labels <- names(fit_list)
            implied_stats <- implied_stats[group_labels]
            x_sd <- sapply(implied_stats, function(xx) {
                        sqrt(xx$cov[x, x])
                      })
            x_mean <- sapply(implied_stats, function(xx) {
                        out <- xx$mean[x]
                        out <- ifelse(is.null(out), 0, out)
                        out
                      })
          } else {
            x_sd <- sqrt(implied_stats$cov[x, x])
            x_mean <- implied_stats$mean[x]
            if (is.null(x_mean)) x_mean <- 0
          }
        plot_df_xstart[, x] <- (plot_df_xstart[, x] - x_mean) / x_sd
        plot_df_xend[, x] <- (plot_df_xend[, x] - x_mean) / x_sd
      }
    if (y_standardized) {
        if (has_groups) {
            # y_sd and y_mean are vectors if ngroups > 1
            group_labels <- names(fit_list)
            implied_stats <- implied_stats[group_labels]
            y_sd <- sapply(implied_stats, function(xx) {
                        xx$cov[y, y]
                      })
            y_mean <- sapply(implied_stats, function(xx) {
                        out <- xx$mean[y]
                        out <- ifelse(is.null(out), 0, out)
                        out
                      })
          } else {
            y_sd <- sqrt(implied_stats$cov[y, y])
            y_mean <- implied_stats$mean[y]
            if (is.null(y_mean)) y_mean <- 0
          }
        plot_df_xstart[, y] <- (plot_df_xstart[, y] - y_mean) / y_sd
        plot_df_xend[, y] <- (plot_df_xend[, y] - y_mean) / y_sd
      }

    cap_txt <- NULL
    if (!is.null(m)) {
        cap_txt <- "The effects are *total* effects through all possible paths"
      }

    if (note_standardized) {
        if (any(c(x_standardized,
                  y_standardized))) {
            tmp <- ifelse(c(x_standardized,
                              y_standardized),
                            c(x_label,
                              y_label),
                            c(NA, NA))
            cap_std <-
              paste0(stats::na.omit(tmp), collapse = ", ")
            cap_std <- paste0(cap_std, " standardized")
          } else {
            cap_std <- NULL
          }
      }
    if (missing(title)) {
        if (!is.null(m)) {
            title <- "Conditional Total Effects"
          } else {
            title <- "Conditional Effects"
          }
      }
    if (has_groups) {
        plot_df_xstart$wlevels <- group_labels
        plot_df_xend$wlevels <- group_labels
      } else {
        plot_df_xstart$wlevels <- rownames(wlevels)
        plot_df_xend$wlevels <- rownames(wlevels)
      }
    plot_df <- rbind(plot_df_xstart, plot_df_xend)
    p <- ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes(x = .data[[x]],
                                           y = .data[[y]],
                                           colour = .data[["wlevels"]]),
                              data = plot_df,
                              size = point_size) +
          ggplot2::geom_segment(ggplot2::aes(
                x =  plot_df_xstart[, x],
                xend =  plot_df_xend[, x],
                y =  plot_df_xstart[, y],
                yend = plot_df_xend[, y],
                colour = plot_df_xstart$wlevels
              ), linewidth = line_width)

    if (note_standardized & !is.null(cap_std)) {
        if (!is.null(cap_txt)) {
            cap_txt <- paste0(cap_txt, "\n", cap_std)
          } else {
            cap_txt <- cap_std
          }
      }
    if (has_groups && graph_type_original == "default") {
        cap_txt <- paste0(cap_txt,
                          "\n",
                          "Graph type is set to tumble for multigroup models.")
      }
    if (use_implied_stats && fit_type != "lm") {
        cap_txt <- paste0(cap_txt,
                          "\n",
                          "Model implied means and SDs are used in drawing the points.")
      }
    out <- p +
      ggplot2::labs(title = title,
                    caption = cap_txt) +
      ggplot2::theme(legend.position = "top",
                     plot.caption = ggplot2::element_text(hjust = .5,
                                                          size = 9),
                     plot.title = ggplot2::element_text(hjust = .5),
                     plot.subtitle = ggplot2::element_text(hjust = .5,
                                                           size = 9)) +
      ggplot2::xlab(x_label) +
      ggplot2::ylab(y_label) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = w_label))
    if (no_title) {
        out <- out + ggplot2::labs(title = NULL)
      }
    out
  }

utils::globalVariables(".data")

