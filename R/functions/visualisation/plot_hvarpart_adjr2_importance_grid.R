#' @title Plot continent-climate fit-importance correlations
#' @description
#' Create a five-continent by eleven-climate-zone grid with explicit empty
#' cells and model-count and Spearman annotations in populated panels.
#' @param data_values Model-level correlation values.
#' @param data_statistics Grouped correlation statistics.
#' @param importance_column Human importance column to plot.
#' @param x_limits Shared adjusted R-squared axis limits.
#' @param y_limits Shared human importance axis limits.
#' @return A ggplot object.
plot_hvarpart_adjr2_importance_grid <- function(
  data_values,
  data_statistics,
  importance_column = c(
    "human_importance_signed",
    "human_importance_bounded"
  ),
  x_limits,
  y_limits
) {
  importance_column <-
    match.arg(importance_column)

  required_values <-
    c(
      "adjusted_r_squared",
      "region",
      "climatezone",
      importance_column
    )

  required_statistics <-
    c(
      "region",
      "climatezone",
      "n_models",
      "spearman_rho",
      "correlation_available"
    )

  assertthat::assert_that(
    is.data.frame(data_values),
    all(required_values %in% names(data_values)),
    msg = "`data_values` does not satisfy the grid plot contract."
  )
  assertthat::assert_that(
    is.data.frame(data_statistics),
    all(required_statistics %in% names(data_statistics)),
    msg = "`data_statistics` does not satisfy the grid plot contract."
  )

  region_levels <-
    unname(vec_regions)

  climate_levels <-
    get_climatezone_label(
      data_climate_zones[["climatezone_label"]]
    )

  data_plot <-
    data_values |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    add_climatezone_as_factor()
  data_stats <-
    data_statistics |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    add_climatezone_as_factor() |>
    dplyr::mutate(
      annotation = stringr::str_c(
        "n=",
        .data[["n_models"]],
        "\n\u03c1=",
        dplyr::if_else(
          .data[["correlation_available"]],
          formatC(
            .data[["spearman_rho"]],
            digits = 2,
            format = "f"
          ),
          "NA"
        )
      )
    )
  data_scaffold <-
    tidyr::crossing(
      region = factor(region_levels, levels = region_levels),
      climatezone_label = factor(climate_levels, levels = climate_levels)
    ) |>
    dplyr::left_join(
      data_stats |>
        dplyr::select(
          .data[["region"]],
          .data[["climatezone_label"]],
          .data[["annotation"]]
        ),
      by = c("region", "climatezone_label")
    ) |>
    dplyr::mutate(
      annotation = tidyr::replace_na(.data[["annotation"]], "No models")
    )
  data_smooth <-
    data_plot |>
    dplyr::semi_join(
      data_stats |>
        dplyr::filter(.data[["correlation_available"]]) |>
        dplyr::select(
          .data[["region"]],
          .data[["climatezone"]]
        ),
      by = c("region", "climatezone")
    )
  x_padding <-
    max(
      diff(x_limits) * 0.03,
      0.005
    )

  y_padding <-
    max(
      diff(y_limits) * 0.03,
      0.01
    )

  figure <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["adjusted_r_squared"]],
        y = .data[[importance_column]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      cols = ggplot2::vars(.data[["climatezone_label"]]),
      drop = FALSE,
      switch = "both",
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(region_labeller)
      )
    ) +
    ggplot2::geom_blank(
      data = data_scaffold,
      mapping = ggplot2::aes(
        x = x_limits[[1]],
        y = y_limits[[1]]
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_smooth(
      data = data_smooth,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      colour = common_gray,
      linewidth = line_size * 3
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data[["climatezone"]]),
      alpha = 0.72,
      size = point_size
    ) +
    ggplot2::geom_text(
      data = data_scaffold,
      mapping = ggplot2::aes(
        x = x_limits[[1]] + x_padding,
        y = y_limits[[2]] - y_padding,
        label = .data[["annotation"]]
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = text_size / ggplot2::.pt * 0.62,
      colour = common_gray,
      lineheight = 0.9
    ) +
    ggplot2::scale_colour_manual(
      values = palette_ecozones,
      drop = FALSE,
      guide = "none"
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
      panel.spacing = grid::unit(1, "mm"),
      strip.background = ggplot2::element_rect(
        fill = "white",
        colour = common_gray,
        linewidth = line_size
      ),
      strip.text.x = ggplot2::element_text(
        angle = 90,
        colour = common_gray
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        colour = common_gray
      ),
      axis.text = ggplot2::element_text(size = text_size * 0.65)
    )

  return(figure)
}
