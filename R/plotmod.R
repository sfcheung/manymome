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

plotmod <- function(output, x_label,
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
    # x0 <- deparse(substitute(x))
    # if (inherits(tryCatch(x00 <- as.character(x), error = function(e) e),
    #              "simpleError")) {
    #     x <- x0
    #   } else {
    #     x <- x00
    #   }
    # w0 <- deparse(substitute(w))
    # if (inherits(tryCatch(w00 <- as.character(w), error = function(e) e),
    #              "simpleError")) {
    #     w <- w0
    #   } else {
    #     w <- w00
    #   }
    # y <- colnames(stats::model.frame(output))[
    #                 attr(stats::terms(output), "response")
    #               ]
    plot_df <- as.data.frame(output)
    full_output <- attr(output, "full_output")
    full_output_1 <- full_output[[1]]
    x <- full_output_1$x
    y <- full_output_1$y
    wlevels <- attr(output, "wlevels")
    fit_type <- cond_indirect_check_fit(fit)
    mf0 <- switch(fit_type,
                  lavaan = lavaan::lavInspect(fit, "data"),
                  lm = lm2ptable(fit)$data)
    x_numeric <- TRUE
    # w_numeric <- is.numeric(mf0[, w])
    if (!x_numeric) {
        stop("x variable must be a numeric variable.")
      }
    # if (w_numeric) {
    #     w_levels <- gen_levels(mf0[, w],
    #                           method = w_method,
    #                           from_mean_in_sd = w_from_mean_in_sd,
    #                           levels = c(-1, 1),
    #                           sd_levels = c(-1, 1),
    #                           sd_to_percentiles = w_sd_to_percentiles,
    #                           percentiles = w_percentiles)
    #     tmp <- length(w_levels)
    #     if (tmp == 2) {
    #       w_levels_labels <- c("Low", "High")
    #     }
    #     if (tmp == 3) {
    #       w_levels_labels <- c("Low", "Medium", "High")
    #     }
    #     if (tmp > 3) {
    #       if (w_method == "percentile") {
    #           w_levels_labels <- paste0(formatC(w_percentiles * 100,
    #                                             digits = 0,
    #                                             format = "f"),
    #                                     "%")
    #         } else {
    #           w_levels_labels <- formatC(w_levels,
    #                                      digits = 2,
    #                                      format = "f")
    #         }
    #     }
    #   } else {
    #     w_lo <- NA
    #     w_hi <- NA
    #     w_levels <- levels(as.factor(mf0[, w]))
    #   }
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
        # TO-FIX
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
    plot_df$plotmod_xstart <- x_levels_m[, 1]
    plot_df$plotmod_xend <- x_levels_m[, 2]
    if (w_numeric) {
        if (graph_type == "default") {
            mf2 <- plot_df_meansd_w_numeric(output = output,
                                            x = x,
                                            w = w,
                                            x_levels,
                                            w_levels,
                                            x_levels_labels = c("Low", "High"),
                                            w_levels_labels = w_levels_labels,
                                            other_numeric_on = "mean",
                                            other_categorical_on = "reference")
          }
        if (graph_type == "tumble") {
            mf_list <- mapply(plot_df_meansd_w_numeric,
                              x_levels = x_levels_list,
                              w_levels = w_levels,
                              w_levels_labels = w_levels_labels,
                              MoreArgs = list(output = output,
                                              x = x,
                                              w = w,
                                              x_levels_labels = c("Low", "High"),
                                              other_numeric_on = "mean",
                                              other_categorical_on = "reference"),
                              SIMPLIFY = FALSE)
            mf2 <- do.call(rbind, mf_list)
          }
      } else {
        if (graph_type == "default") {
            mf2 <- plot_df_meansd_w_categorical(output = output,
                                            x = x,
                                            w = w,
                                            x_levels,
                                            w_levels,
                                            x_levels_labels = c("Low", "High"),
                                            other_numeric_on = "mean",
                                            other_categorical_on = "reference")
          }
        if (graph_type == "tumble") {
            mf_list <- mapply(plot_df_meansd_w_categorical,
                              x_levels = x_levels_list,
                              w_levels = w_levels,
                              MoreArgs = list(output = output,
                                              x = x,
                                              w = w,
                                              x_levels_labels = c("Low", "High"),
                                              other_numeric_on = "mean",
                                              other_categorical_on = "reference"),
                              SIMPLIFY = FALSE)
            mf2 <- do.call(rbind, mf_list)
          }
      }

    mf2$predicted <- stats::predict(output, mf2)

    if (missing(x_label)) x_label <- x
    if (missing(w_label)) w_label <- w
    if (missing(y_label)) y_label <- y

    x_standardized <- is_standardized(mf0[, x])
    w_standardized <- is_standardized(mf0[, w])
    y_standardized <- is_standardized(mf0[, y])

    if (note_standardized) {
        if (any(c(x_standardized,
                  w_standardized,
                  y_standardized))) {
            tmp <- ifelse(c(x_standardized,
                              w_standardized,
                              y_standardized),
                            c(x_label,
                              w_label,
                              y_label),
                            c(NA, NA, NA))
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
    p <- ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes_string(x = x,
                                                  y = "predicted",
                                                  colour = "w_level"),
                              data = mf2,
                              size = point_size) +
          ggplot2::geom_segment(ggplot2::aes(
                x = mf2[mf2$x_level == "Low", x],
                xend = mf2[mf2$x_level == "High", x],
                y = mf2[mf2$x_level == "Low", "predicted"],
                yend = mf2[mf2$x_level == "High", "predicted"],
                colour = mf2[mf2$x_level == "Low", "w_level"]
              ), size = line_width)

    b_all <- find_bs(mf = mf2, x = x, w = w, w_levels = w_levels)
    b_format <- paste0("%.", digits, "f")

    if (w_numeric) {
        tmp <- paste0(w_label, " ", w_levels_labels, ": ",
                      x_label, " effect = ",
                      sprintf(b_format, b_all))
        subtxt <- paste(tmp, collapse = "\n")
      } else {
        subtxt <- paste0(w_levels,
                         ": ",
                         x_label,
                         " effect = ",
                         sprintf(b_format, b_all),
                         collapse = "\n")
      }
    if (w_numeric) {
        if (w_method == "percentile") {
            tmp <- paste0(w_levels_labels, ": ",
                          round(w_percentiles * 100, 0),
                          "th percentile")
            cap_txt <- paste0(w_label, " levels: ",
                              paste0(tmp, collapse = "; "))
          }
        if (w_method == "sd") {
            cap_txt <- paste0(w_label, " levels: ",
                              "Low: ", w_from_mean_in_sd,
                              "SD below mean; Hi: ",
                              w_from_mean_in_sd, " SD above mean")
          }
      } else {
        cap_txt <- NULL
      }
    if (note_standardized & !is.null(cap_std)) {
        if (!is.null(cap_txt)) {
            cap_txt <- paste0(cap_txt, "\n", cap_std)
          } else {
            cap_txt <- cap_std
          }
      }
    out <- p +
      ggplot2::labs(title = title,
                    subtitle = subtxt,
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
