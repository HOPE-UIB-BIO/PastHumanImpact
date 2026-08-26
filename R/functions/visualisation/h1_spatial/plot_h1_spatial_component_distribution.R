#' @title Plot one time-controlled H1 spatial component
#' @description
#' Combine continent maps, continent densities, and climate-zone summaries for
#' one signed time-controlled HVarPart component on one continuous scale.
#' @param data_values Dataset-level spatial values for one measure and
#' component.
#' @param data_geo_koppen Spatial climate-zone object used by
#' `build_region_map()`.
#' @param component_colour Positive endpoint colour.
#' @param value_limits Two finite limits shared by the maps and summaries.
#' @param value_breaks Axis and legend breaks.
#' @param y_axis_title Two-line vertical axis title.
#' @param legend_title Component name shown above the continuous legend.
#' @return A named list containing the plot and its source data.
#' @examples
#' \dontrun{
#' plot_h1_spatial_component_distribution(
#'   values,
#'   climate_zones,
#'   "#C99B38",
#'   c(-0.4, 0.8),
#'   c(-0.4, 0, 0.4, 0.8),
#'   "Relative importance\n(Untruncated hierarchical contribution)",
#'   "Human"
#' )
#' }
plot_h1_spatial_component_distribution <- function(
  data_values,
  data_geo_koppen,
  component_colour,
  value_limits,
  value_breaks,
  y_axis_title,
  legend_title
) {
  required_columns <-
    c(
      "dataset_id", "analysis", "region", "climatezone",
      "long", "lat", "measure", "component", "value"
    )

  assertthat::assert_that(
    is.data.frame(data_values),
    all(required_columns %in% names(data_values)),
    nrow(data_values) > 0L,
    all(is.finite(data_values[["value"]])),
    length(unique(data_values[["measure"]])) == 1L,
    length(unique(data_values[["component"]])) == 1L,
    is.data.frame(data_geo_koppen),
    length(value_limits) == 2L,
    all(is.finite(value_limits)),
    value_limits[[1]] < value_limits[[2]],
    msg = "Spatial component plot inputs do not satisfy the contract."
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")

  value_palette <- c(common_gray, "#F2F2F2", component_colour)

  value_colour <-
    scales::col_numeric(
      palette = value_palette,
      domain = value_limits
    )

  data_records <-
    data_values |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    prepare_climatezone_factor()

  if (
    any(data_records[["value"]] < value_limits[[1]]) ||
      any(data_records[["value"]] > value_limits[[2]])
  ) {
    cli::cli_abort("At least one spatial value lies outside `value_limits`.")
  }

  data_climatezone_summary <-
    data_records |>
    dplyr::group_by(
      .data[["analysis"]],
      .data[["region"]],
      .data[["climatezone"]],
      .data[["climatezone_label"]]
    ) |>
    dplyr::summarise(
      value = stats::median(.data[["value"]]),
      dataset_count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      summary_colour = value_colour(.data[["value"]])
    )

  data_region_summary <-
    data_records |>
    dplyr::group_by(.data[["analysis"]], .data[["region"]]) |>
    dplyr::summarise(
      value = stats::median(.data[["value"]]),
      dataset_count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(line_colour = value_colour(.data[["value"]]))

  data_quantiles <-
    data_records |>
    dplyr::group_by(
      .data[["region"]],
      .data[["climatezone"]],
      .data[["climatezone_label"]]
    ) |>
    dplyr::summarise(
      q_95_lwr = stats::quantile(.data[["value"]], 0.025),
      q_95_upr = stats::quantile(.data[["value"]], 0.975),
      q_75_lwr = stats::quantile(.data[["value"]], 0.125),
      q_75_upr = stats::quantile(.data[["value"]], 0.875),
      q_50_lwr = stats::quantile(.data[["value"]], 0.25),
      q_50_upr = stats::quantile(.data[["value"]], 0.75),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("q_"),
      names_to = c("interval", ".value"),
      names_pattern = "q_(\\d+)_(lwr|upr)"
    )

  data_density <-
    data_records |>
    dplyr::group_split(.data[["region"]]) |>
    purrr::map(
      ~ {
        density_values <-
          stats::density(
            .x[["value"]],
            from = value_limits[[1]],
            to = value_limits[[2]],
            n = 256,
            cut = 0
          )

        tibble::tibble(
          region = factor(
            unique(.x[["region"]]),
            levels = region_levels
          ),
          value = density_values[["x"]],
          density = density_values[["y"]]
        )
      }
    ) |>
    dplyr::bind_rows()

  density_max <- max(data_density[["density"]], na.rm = TRUE)

  data_density <-
    data_density |>
    dplyr::mutate(
      density_scaled = 3 * .data[["density"]] / density_max,
      panel_label = "Continent"
    )

  density_x <- seq(0, 3, length.out = 90)

  data_density_tiles <-
    tidyr::crossing(data_density, density_x = density_x) |>
    dplyr::filter(.data[["density_x"]] <= .data[["density_scaled"]])

  panel_levels <-
    c("Continent", levels(data_records[["climatezone_label"]]))

  data_climatezone_summary <-
    data_climatezone_summary |>
    dplyr::mutate(
      panel_label = factor(
        as.character(.data[["climatezone_label"]]),
        levels = panel_levels
      ),
      panel_x = 0.5
    )

  data_quantiles <-
    data_quantiles |>
    dplyr::mutate(
      panel_label = factor(
        as.character(.data[["climatezone_label"]]),
        levels = panel_levels
      ),
      panel_x = 0.5
    )

  data_density <-
    data_density |>
    dplyr::mutate(
      panel_label = factor(.data[["panel_label"]], levels = panel_levels)
    )

  data_density_tiles <-
    data_density_tiles |>
    dplyr::mutate(
      panel_label = factor(.data[["panel_label"]], levels = panel_levels)
    )

  displayed_regions <-
    region_levels[region_levels %in% as.character(data_records[["region"]])]

  data_panel_scaffold <-
    tidyr::crossing(
      region = factor(displayed_regions, levels = region_levels),
      panel_label = factor(panel_levels, levels = panel_levels),
      panel_edge = c(0, 1)
    ) |>
    dplyr::mutate(
      panel_x = dplyr::if_else(
        .data[["panel_label"]] == "Continent",
        3 * .data[["panel_edge"]],
        .data[["panel_edge"]]
      )
    )

  data_region_lines <-
    tidyr::crossing(
      data_region_summary,
      panel_label = factor(panel_levels, levels = panel_levels)
    )

  background_values <-
    seq(value_limits[[1]], value_limits[[2]], length.out = 201)

  background_step <- background_values[[2]] - background_values[[1]]

  data_background <-
    tidyr::crossing(
      region = factor(displayed_regions, levels = region_levels),
      panel_label = panel_levels[panel_levels != "Continent"],
      value = background_values
    ) |>
    dplyr::mutate(
      panel_label = factor(.data[["panel_label"]], levels = panel_levels),
      ymin = .data[["value"]] - background_step / 2,
      ymax = .data[["value"]] + background_step / 2
    )

  interval_layers <-
    c("95", "75", "50") |>
    purrr::map(
      ~ ggplot2::geom_segment(
        data = data_quantiles |>
          dplyr::filter(.data[["interval"]] == .x),
        mapping = ggplot2::aes(
          x = .data[["panel_x"]],
          xend = .data[["panel_x"]],
          y = .data[["lwr"]],
          yend = .data[["upr"]]
        ),
        colour = common_gray,
        alpha = 0.75,
        linewidth = (0.1 + (1 - as.numeric(.x) / 100)) * 5
      )
    )

  statistical_plot <-
    ggplot2::ggplot() +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      cols = ggplot2::vars(.data[["panel_label"]]),
      scales = "free_x",
      space = "free_x",
      switch = "both",
      drop = TRUE
    ) +
    ggplot2::geom_blank(
      data = data_panel_scaffold,
      mapping = ggplot2::aes(x = .data[["panel_x"]], y = 0)
    ) +
    ggplot2::geom_rect(
      data = data_background,
      mapping = ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        fill = .data[["value"]]
      ),
      colour = NA,
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = colorspace::lighten(common_gray, amount = 0.35),
      linewidth = line_size * 2,
      linetype = 2
    ) +
    ggplot2::geom_tile(
      data = data_density_tiles,
      mapping = ggplot2::aes(
        x = .data[["density_x"]],
        y = .data[["value"]],
        fill = .data[["value"]]
      ),
      width = 3 / length(density_x),
      height = diff(value_limits) / 256
    ) +
    interval_layers +
    ggplot2::geom_hline(
      data = data_region_lines,
      mapping = ggplot2::aes(
        yintercept = .data[["value"]],
        colour = .data[["line_colour"]]
      ),
      linewidth = line_size * 8
    ) +
    ggplot2::geom_point(
      data = data_climatezone_summary,
      mapping = ggplot2::aes(
        x = .data[["panel_x"]],
        y = .data[["value"]]
      ),
      colour = common_gray,
      size = point_size * 4
    ) +
    ggplot2::geom_point(
      data = data_climatezone_summary,
      mapping = ggplot2::aes(
        x = .data[["panel_x"]],
        y = .data[["value"]],
        colour = .data[["summary_colour"]]
      ),
      size = point_size * 3
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = value_breaks,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_fill_gradient2(
      low = value_palette[[1]],
      mid = value_palette[[2]],
      high = value_palette[[3]],
      midpoint = 0,
      limits = value_limits,
      oob = scales::squish,
      name = legend_title,
      guide = ggplot2::guide_colourbar(
        title.position = "left",
        direction = "horizontal",
        barwidth = grid::unit(28, "mm"),
        barheight = grid::unit(2, "mm")
      )
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::labs(x = NULL, y = y_axis_title) +
    ggplot2::coord_cartesian(ylim = value_limits, expand = FALSE) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      panel.spacing.x = grid::unit(0, "mm"),
      panel.spacing.y = grid::unit(2, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        size = text_size * 0.58,
        colour = common_gray
      ),
      strip.text.x.bottom = ggplot2::element_text(
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        size = text_size * 0.65,
        colour = common_gray,
        lineheight = 0.8,
        margin = ggplot2::margin(0, 0, 0, 0)
      ),
      strip.switch.pad.grid = grid::unit(0, "mm"),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line.y.right = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      ),
      axis.text.y = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      axis.title.y = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      line = ggplot2::element_line(
        linewidth = line_size,
        colour = common_gray
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = text_size * 0.6),
      legend.text = ggplot2::element_text(size = text_size * 0.55),
      legend.key.width = grid::unit(1, "mm"),
      legend.key.height = grid::unit(1.5, "mm"),
      legend.spacing.x = grid::unit(0, "mm"),
      legend.box.spacing = grid::unit(0, "mm"),
      legend.margin = ggplot2::margin(0.25, 0.5, 0.25, 0.5, unit = "mm"),
      legend.box.background = ggplot2::element_rect(
        fill = "white",
        colour = common_gray,
        linewidth = line_size
      )
    )

  map_list <-
    displayed_regions |>
    purrr::map(
      ~ build_region_map(
        rasterdata = data_geo_koppen,
        select_region = .x,
        sel_alpha = 0
      ) +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_point(
          data = data_records |>
            dplyr::filter(.data[["region"]] == .x),
          mapping = ggplot2::aes(
            x = .data[["long"]],
            y = .data[["lat"]],
            fill = .data[["value"]]
          ),
          shape = 21,
          colour = common_gray,
          stroke = line_size * 2,
          size = point_size,
          show.legend = FALSE
        ) +
        ggplot2::scale_fill_gradient2(
          low = value_palette[[1]],
          mid = value_palette[[2]],
          high = value_palette[[3]],
          midpoint = 0,
          limits = value_limits,
          oob = scales::squish
        ) +
        ggplot2::theme(
          legend.position = "none",
          panel.border = ggplot2::element_blank(),
          plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
        )
    )

  statistical_grob <-
    statistical_plot |>
    ggplot2::ggplotGrob() |>
    build_horizontal_hvarpart_density_strip() |>
    build_hvarpart_density_divider()

  panel_rows <-
    statistical_grob[["layout"]] |>
    dplyr::filter(grepl("^panel-", .data[["name"]])) |>
    dplyr::distinct(.data[["t"]]) |>
    dplyr::arrange(.data[["t"]]) |>
    dplyr::pull(.data[["t"]])

  map_panel_rows <- panel_rows[match(displayed_regions, region_levels)]

  if (
    length(map_panel_rows) != length(map_list) ||
      anyNA(map_panel_rows)
  ) {
    cli::cli_abort("Could not align every component map to a continent row.")
  }

  combined_grob <-
    gtable::gtable_add_cols(
      x = statistical_grob,
      widths = grid::unit(3.5, "null"),
      pos = 0
    )

  combined_plot <-
    purrr::reduce2(
      .x = map_list,
      .y = map_panel_rows,
      .init = combined_grob,
      .f = build_hvarpart_balance_map_grob
    ) |>
    cowplot::ggdraw()

  res <-
    list(
      plot = combined_plot,
      statistical_plot = statistical_plot,
      record_values = data_records,
      climatezone_values = data_climatezone_summary,
      region_values = data_region_summary,
      density_values = data_density
    )

  return(res)
}


