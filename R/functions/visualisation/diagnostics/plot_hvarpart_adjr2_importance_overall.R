#' @title Plot overall adjusted R-squared and human importance
#' @description
#' Create the standalone all-model scatterplot with a linear fit and reported
#' Spearman and Pearson correlations.
#' @param data_values Model-level correlation values.
#' @param data_statistics One-row correlation statistics.
#' @param importance_column Human importance column to plot.
#' @param x_limits Shared adjusted R-squared axis limits.
#' @param y_limits Human importance axis limits.
#' @return A ggplot object.
plot_hvarpart_adjr2_importance_overall <- function(
  data_values,
  data_statistics,
  importance_column = c(
    "human_importance_signed",
    "human_importance_bounded"
  ),
  x_limits = NULL,
  y_limits = NULL
) {
  importance_column <-
    match.arg(importance_column)

  required_values <-
    c(
      "adjusted_r_squared",
      "climatezone",
      importance_column
    )

  required_statistics <-
    c(
      "n_models",
      "spearman_rho",
      "pearson_r",
      "correlation_available"
    )

  assertthat::assert_that(
    is.data.frame(data_values),
    all(required_values %in% names(data_values)),
    msg = "`data_values` does not satisfy the overall plot contract."
  )
  assertthat::assert_that(
    is.data.frame(data_statistics),
    nrow(data_statistics) == 1L,
    all(required_statistics %in% names(data_statistics)),
    msg = "`data_statistics` must contain one overall statistics row."
  )

  data_plot <-
    data_values |>
    prepare_climatezone_factor()

  if (
    is.null(x_limits)
  ) {
    x_limits <-
      range(
        data_plot[["adjusted_r_squared"]],
        finite = TRUE
      )
  }

  if (
    is.null(y_limits)
  ) {
    y_limits <-
      range(
        data_plot[[importance_column]],
        finite = TRUE
      )
  }

  x_padding <-
    max(
      diff(x_limits) * 0.04,
      0.01
    )

  y_padding <-
    max(
      diff(y_limits) * 0.04,
      0.02
    )

  annotation <-
    stringr::str_c(
      "n = ",
      data_statistics[["n_models"]],
      "\nSpearman \u03c1 = ",
      ifelse(
        data_statistics[["correlation_available"]],
        formatC(
          data_statistics[["spearman_rho"]],
          digits = 2,
          format = "f"
        ),
        "NA"
      ),
      "\nPearson r = ",
      ifelse(
        data_statistics[["correlation_available"]],
        formatC(
          data_statistics[["pearson_r"]],
          digits = 2,
          format = "f"
        ),
        "NA"
      )
    )

  figure <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["adjusted_r_squared"]],
        y = .data[[importance_column]]
      )
    ) +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      colour = common_gray,
      fill = colorspace::lighten(common_gray, 0.55),
      linewidth = line_size * 5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data[["climatezone"]]),
      alpha = 0.75,
      size = point_size * 1.7
    ) +
    ggplot2::annotate(
      "text",
      x = x_limits[[1]] + x_padding,
      y = y_limits[[2]] - y_padding,
      label = annotation,
      hjust = 0,
      vjust = 1,
      size = text_size / ggplot2::.pt
    ) +
    ggplot2::scale_colour_manual(
      values = palette_ecozones,
      drop = FALSE,
      name = "Climate zone"
    ) +
    ggplot2::coord_cartesian(
      xlim = x_limits,
      ylim = y_limits,
      expand = FALSE
    ) +
    ggplot2::labs(
      x = "Adjusted R\u00b2",
      y = if (importance_column == "human_importance_bounded") {
        "Human relative importance\n(zero-truncated allocation)"
      } else {
        "Human relative importance\n(signed allocation)"
      }
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )

  return(figure)
}
