#' @title Plot Conditional ToTal Effect
#'
#' @description Plot the conditional total effects for different
#'   levels of moderators.
#'
#' @details This function is a plot method of the output
#'  of [cond_indirect_effects()]. It will use the levels of moderators
#'  in the output.
#'
#' It plots the *total* effect from `x` to `y` in a model for
#' different levels of the moderators.
#'
#' If `x` has only one path to `y` and there is no
#' mediator along this path, then it plots the conditional (simple)
#' effect of `x` on `y`.
#'
#' If `x` has only one path to `y` and there is one or more
#' mediators along this path, then it plots the conditional
#' *indirect* effect along this path.
#'
#' If `x` has more than one path to `y`, then it plots the
#' conditional *total* effect from `x` to `y`.
#'
#' @return
#'  A [ggplot2] graph. Plotted if not assigned to a name. It can
#'  be further modified like a usual [ggplot2] graph.
#'
#' @param x The output of [cond_indirect_effects()]. (Named `x`
#'          because it is required in the naming of arguments
#'          of the `plot`` generic function.)
#' @param x_label The label for the X-axis. Default is the value of the
#'                predictor in the output of [cond_indirect_effects()].
#' @param w_label The label for the legend for the lines.
#'                Default is `"Moderator(s)"`.
#' @param y_label The label for the Y-axis. Default is the
#'                name of the response variable in the model.
#' @param title The title of the graph. If not supplied, it will be
#'               generated from the variable
#'               names or labels (in `x_label`, `y_label`,
#'               and `w_label`). If `""`, no title will be printed.
#'               This can be used when the plot is for manuscript
#'               submission and figures are required to have no
#'               titles.
#' @param x_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the focal variable.
#'                          Default is 1.
#' @param x_method How to define "high" and "low" for the focal
#'                  variable levels.
#'                  Default is in terms of the
#'                  standard deviation of the focal variable, `"sd"`.
#'                  If equal to
#'                  `"percentile"`, then the percentiles of the
#'                  focal variable in the
#'                  dataset is used.
#' @param x_percentiles If `x_method` is `"percentile"`, then this
#'                      argument
#'                      specifies the two percentiles to be used,
#'                      divided by 100.
#'                        It must be a
#'                      vector of two numbers. The default is
#'                      `c(.16, .84)`,
#'                      the 16th and 84th percentiles,
#'                      which corresponds approximately
#'                      to one SD below and above mean for a
#'                      normal distribution, respectively.
#' @param x_sd_to_percentiles If `x_method` is `"percentile"` and this
#'                            argument is
#'                            set to a number, this number will be used
#'                            to determine the percentiles to be used. The
#'                            lower percentile is the percentile in a
#'                            normal
#'                            distribution
#'                            that is `x_sd_to_percentiles` SD below the mean.
#'                            The upper percentile is the percentile in a normal
#'                            distribution that is `x_sd_to_percentiles` SD
#'                            above the mean. Therefore, if
#'                            `x_sd_to_percentiles` is set to 1, then the lower
#'                            and upper percentiles are 16th and 84th,
#'                            respectively. Default is `NA`.
#' @param note_standardized If `TRUE`, will check whether a variable has SD
#'                          nearly equal to one. If yes, will report this in the
#'                          plot.
#'                          Default is `TRUE`.
#' @param no_title If `TRUE`, title will be suppressed. Default is `FALSE`.
#' @param line_width The width of the lines as used in
#'                   [ggplot2::geom_segment()].
#'                   Default is 1.
#' @param point_size The size of the points as used in [ggplot2::geom_point()].
#'                    Default is 5.
#' @param graph_type If `"default"`, the typical line-graph with equal end-points
#'                   will be plotted. If `"tubmle"`, then the tumble graph
#'                   proposed by Bodner (2016) will be plotted. Default is
#'                   `"default"`.
#' @param ... Additional arguments. Ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#' # lavaan
#' mod <-
#' "
#' m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
#' y ~ m2 + m3 + x + w4 + x:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
#' out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"),
#'                             sd_from_mean = c(-1, 1),
#'                             fit = fit, merge = TRUE)
#' out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)
#' plot(out_1)
#' plot(out_1, graph_type = "tumble")
#'
#' # Regression
#' lm_m3 <- lm(m3 ~ m1 + x*gp, dat)
#' lm_y <- lm(y ~ m2 + m3 + x*w4, dat)
#' fit_lm <- lm2list(lm_m3, lm_y)
#' out_mm_1_lm <- mod_levels_list("w4", c("gpgp2", "gpgp3"),
#'                             sd_from_mean = c(-1, 1),
#'                             fit = fit_lm, merge = TRUE)
#' out_1_lm <- cond_indirect_effects(wlevels = out_mm_1_lm, x = "x", y = "y", m = "m3", fit = fit_lm)
#' plot(out_1_lm)
#' plot(out_1_lm, graph_type = "tumble")
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
                            ...
                    ) {
    output <- x
    fit <- attr(output, "fit")
    fit_type <- cond_indirect_check_fit(fit)
    x_method <- match.arg(x_method)
    graph_type <- match.arg(graph_type)
    full_output <- attr(output, "full_output")
    full_output_1 <- full_output[[1]]
    x <- full_output_1$x
    y <- full_output_1$y
    m <- full_output_1$m
    wlevels <- attr(output, "wlevels")
    if (fit_type == "lm") {
        wlevels <- ind_to_cat(wlevels)
      }

    w_names <- colnames(wlevels)
    mf0 <- switch(fit_type,
                  lavaan = lavaan::lavInspect(fit, "data"),
                  lm = merge_model_frame(fit))
    fit_list <- switch(fit_type,
                       lavaan = lm_from_lavaan_list(fit),
                       lm = fit)
    if ((fit_type == "lm") && !inherits(fit_list, "lm_list")) {
        fit_list <- lm2list(fit_list)
      }
    dat0 <- switch(fit_type,
                  lavaan = lavaan::lavInspect(fit, "data"),
                  lm = merge_model_frame(fit))
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
        x_subsets <- lapply(split(wlevels, seq_len(nrow(wlevels))),
                            x_for_wlevels,
                            mf = mf0, x = x)
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
    mf2 <- data.frame(lapply(as.data.frame(dat0), sum_col),
                      check.names = FALSE)
    mf2 <- mf2[, -(which(colnames(mf2) %in% c(x, w_names)))]
    plot_df_xstart <- cbind(plot_df_xstart, wlevels, mf2)
    plot_df_xend <- cbind(plot_df_xend, wlevels, mf2)
    colnames(plot_df_xstart)[1] <- x
    colnames(plot_df_xend)[1] <- x
    plot_df_xstart[, y] <- stats::predict(fit_list,
                                          x = x, y = y, m = m,
                                          newdata = plot_df_xstart)
    plot_df_xend[, y] <- stats::predict(fit_list,
                                        x = x, y = y, m = m,
                                        newdata = plot_df_xend)

    if (missing(x_label)) x_label <- x
    if (missing(y_label)) y_label <- y

    x_standardized <- full_output_1$standardized_x
    y_standardized <- full_output_1$standardized_y

    if (x_standardized || y_standardized) {
        implied_stats <- switch(fit_type,
                          lavaan = lavaan::lavInspect(fit, "implied"),
                          lm = lm2ptable(fit)$implied_stats)
      }
    if (x_standardized) {
        x_sd <- sqrt(implied_stats$cov[x, x])
        x_mean <- implied_stats$mean[x]
        if (is.null(x_mean)) x_mean <- 0
        plot_df_xstart[, x] <- (plot_df_xstart[, x] - x_mean) / x_sd
        plot_df_xend[, x] <- (plot_df_xend[, x] - x_mean) / x_sd
      }
    if (y_standardized) {
        y_sd <- sqrt(implied_stats$cov[y, y])
        y_mean <- implied_stats$mean[y]
        if (is.null(y_mean)) y_mean <- 0
        plot_df_xstart[, y] <- (plot_df_xstart[, y] - y_mean) / y_sd
        plot_df_xend[, y] <- (plot_df_xend[, y] - y_mean) / y_sd
      }

    cap_txt <- NULL

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
        title <- "Moderation Effect"
      }

    plot_df_xstart$wlevels <- rownames(wlevels)
    plot_df_xend$wlevels <- rownames(wlevels)
    plot_df <- rbind(plot_df_xstart, plot_df_xend)

    p <- ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes_string(x = x,
                                                  y = y,
                                                  colour = "wlevels"),
                              data = plot_df,
                              size = point_size) +
          ggplot2::geom_segment(ggplot2::aes(
                x =  plot_df_xstart[, x],
                xend =  plot_df_xend[, x],
                y =  plot_df_xstart[, y],
                yend = plot_df_xend[, y],
                colour = plot_df_xstart$wlevels
              ), size = line_width)

    if (note_standardized & !is.null(cap_std)) {
        if (!is.null(cap_txt)) {
            cap_txt <- paste0(cap_txt, "\n", cap_std)
          } else {
            cap_txt <- cap_std
          }
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
