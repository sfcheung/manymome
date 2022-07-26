#' @title Moderation Effect Plot
#'
#' @description Plot the moderation effect in a regression model
#'
#' @details This function generate a basic [ggplot2] graph
#'          typically found in psychology manuscripts. It tries to
#'          check whether one or more variables are standardized, and
#'          report this in the plot if required.
#'
#' This function only has features for typical plots of moderation effects.
#' It is not intended to be a flexible tool for a fine control on the plots.
#'
#' @return
#'  A [ggplot2] graph. Plotted if not assigned to a name. It can
#'  be further modified like a usual [ggplot2] graph.
#'
#' @param output The output
#'                  of [stats::lm()], [std_selected()], or
#'                  [std_selected_boot()].
#'
#' @param x The name of the focal variable (x-axis) in `output``. It
#'          can be the name of the variable, with or without quotes.
#'          Currently only numeric variables are supported.
#' @param w The name of the moderator in `output`. It
#'          can be the name of the variable, with or without quotes.
#' @param x_label The label for the X-axis. Default is the value of `x`.
#' @param w_label The label for the legend for the lines.
#'                Default is the value of`w`.
#' @param y_label The label for the Y-axis. Default is the
#'                name of the response variable in the model.
#' @param title The title of the graph. If not supplied, it will be
#'               generated from the variable
#'               names or labels (in `x_label`, `y_label`,
#'               and `w_label`). If `""`, no title will be printed.
#'               This can be used when the plot is for manuscript
#'               submission and figures are required to have no
#'               titles.
#' @param digits Number of decimal places to print. Default is 3.
#' @param x_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the focal variable.
#'                          Default is 1.
#' @param w_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the moderator. Default is 1.
#'                          Ignored if `w` is categorical.
#' @param w_method How to define "high" and "low" for the moderator levels.
#'                  Default is in terms of the
#'                  standard deviation of the moderator, `"sd"`.
#'                  If equal to
#'                  `"percentile"`, then the percentiles of the moderator in
#'                  the
#'                  dataset are used.
#'                  Ignored if `w` is categorical.
#' @param w_percentiles If `w_method` is `"percentile"`, then this
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
#'                      Ignored if `w` is categorical.
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
#' @param w_sd_to_percentiles If `w_method` is `"percentile"` and
#'                            this argument is
#'                            set to a number, this number will be
#'                            used
#'                            to determine the percentiles to be used.
#'                            The
#'                            lower percentile is the percentile in a
#'                            normal
#'                            distribution
#'                            that is `w_sd_to_percentiles` SD below
#'                            the mean.
#'                            The upper percentile is the percentile in
#'                            a normal
#'                            distribution that is `w_sd_to_percentiles`
#'                            SD
#'                            above the mean. Therefore, if
#'                            `w_sd_to_percentiles` is set to 1, then the
#'                            lower
#'                            and upper percentiles are 16th and 84th,
#'                            respectively. Default is `NA`.
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
#'
#' # Do a moderated regression by lm
#' lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, sleep_emo_con)
#' plotmod(lm_out,
#'         x = emotional_stability,
#'         w = conscientiousness,
#'         x_label = "Emotional Stability",
#'         w_label = "Conscientiousness",
#'         y_label = "Sleep Duration")
#'
#' # Standardize all variables except for categorical variables
#' lm_std <- std_selected(lm_out,
#'                        to_scale = ~ .,
#'                        to_center = ~ .)
#' plotmod(lm_std,
#'         x = emotional_stability,
#'         w = conscientiousness,
#'         x_label = "Emotional Stability",
#'         w_label = "Conscientiousness",
#'         y_label = "Sleep Duration")
#'
#' # Tumble Graph
#' plotmod(lm_std,
#'         x = emotional_stability,
#'         w = conscientiousness,
#'         x_label = "Emotional Stability",
#'         w_label = "Conscientiousness",
#'         y_label = "Sleep Duration",
#'         graph_type = "tumble")
#'
#' @export

plot.cond_indirect_effects <- function(x,
                            x_label,
                            w_label,
                            y_label,
                            fit,
                            title,
                            digits = 3,
                            x_from_mean_in_sd = 1,
                            # w_from_mean_in_sd = 1,
                            # w_method = c("sd", "percentile"),
                            # w_percentiles = c(.16, .84),
                            x_method = c("sd", "percentile"),
                            x_percentiles = c(.16, .84),
                            # w_sd_to_percentiles = NA,
                            x_sd_to_percentiles= NA,
                            note_standardized = TRUE,
                            no_title = FALSE,
                            line_width = 1,
                            point_size = 5,
                            graph_type = c("default", "tumble")
                    ) {
    # Note
    # - No need to generate w levels. They can be retrieved from the output of
    #   cond_indirect_effects().
    # - x and y are from the output
    #
    # w_method <- match.arg(w_method)
    x_method <- match.arg(x_method)
    graph_type <- match.arg(graph_type)
    output <- x
    full_output <- attr(output, "full_output")
    full_output_1 <- full_output[[1]]
    x <- full_output_1$x
    y <- full_output_1$y
    m <- full_output_1$m
    wlevels <- attr(output, "wlevels")
    w_names <- colnames(wlevels)
    fit <- attr(output, "fit")
    fit_type <- cond_indirect_check_fit(fit)
    mf0 <- switch(fit_type,
                  lavaan = lavaan::lavInspect(fit, "data"),
                  lm = lm2ptable(fit)$data)
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
    browser()
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

    # b_all <- find_bs(mf = mf2, x = x, w = w, w_levels = w_levels)
    # b_format <- paste0("%.", digits, "f")

    if (note_standardized & !is.null(cap_std)) {
        if (!is.null(cap_txt)) {
            cap_txt <- paste0(cap_txt, "\n", cap_std)
          } else {
            cap_txt <- cap_std
          }
      }
    out <- p +
      ggplot2::labs(title = title) +
      ggplot2::theme(legend.position = "top",
                     plot.caption = ggplot2::element_text(hjust = .5,
                                                          size = 9),
                     plot.title = ggplot2::element_text(hjust = .5),
                     plot.subtitle = ggplot2::element_text(hjust = .5,
                                                           size = 9)) +
      ggplot2::xlab(x_label) +
      ggplot2::ylab(y_label) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Moderator(s)"))
    if (no_title) {
        out <- out + ggplot2::labs(title = NULL)
      }
    out
  }
