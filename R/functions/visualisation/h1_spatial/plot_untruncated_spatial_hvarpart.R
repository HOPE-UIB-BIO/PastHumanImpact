#' @title Plot signed spatial human-impact allocation
#' @description
#' Combines continent maps, continent-wide densities, climate-zone
#' distributions, and pooled continent reference lines on one aligned signed
#' allocation scale. Values at or below zero share the neutral endpoint colour;
#' values at or above one share the full human-impact endpoint colour.
#' @param data_importance Canonical predictor-level HVarPart importance data.
#' @param data_meta Dataset metadata containing coordinates and spatial groups.
#' @param data_geo_koppen Spatial climate-zone object used by `build_region_map()`.
#' @return A named list containing the plot and its record, climate-zone,
#' continent, and density source data.
#' @examples
#' \dontrun{
#' plot_untruncated_spatial_hvarpart(
#'   data_importance = importance,
#'   data_meta = metadata,
#'   data_geo_koppen = climate_zones
#' )
#' }
plot_untruncated_spatial_hvarpart <- function(
  data_importance,
  data_meta,
  data_geo_koppen
) {
  required_importance <- c(
    "analysis", "model_id", "dataset_id", "region", "climatezone",
    "predictor", "individual", "total_adjusted_r_squared",
    "is_importance_eligible"
  )
  required_meta <- c(
    "dataset_id", "long", "lat", "region", "climatezone"
  )
  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_importance %in% names(data_importance)),
    is.data.frame(data_meta),
    all(required_meta %in% names(data_meta)),
    msg = "Signed spatial plot inputs do not satisfy the required contract."
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")
  signed_palette <-
    c(
      "#F2F2F2",
      colorspace::lighten(palette_predictors[["human"]], amount = 0.5),
      palette_predictors[["human"]]
    )
  data_spatial <-
    data_importance |>
    dplyr::filter(
      .data[["analysis"]] == "spatial_spd",
      .data[["is_importance_eligible"]]
    )
  data_records <-
    data_spatial |>
    dplyr::group_by(.data[["model_id"]]) |>
    dplyr::mutate(
      zero_truncated_individual = pmax(.data[["individual"]], 0),
      zero_truncated_total = sum(.data[["zero_truncated_individual"]]),
      zero_truncated_allocation =
        .data[["zero_truncated_individual"]] /
          .data[["zero_truncated_total"]],
      signed_allocation = .data[["individual"]] /
        .data[["total_adjusted_r_squared"]]
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data[["predictor"]] == "human") |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    prepare_climatezone_factor()

  data_climatezone_summary <-
    summarise_untruncated_spatial_importance(
      data_importance = data_spatial,
      group_vars = c("analysis", "region", "climatezone"),
      region_levels = region_levels
    ) |>
    prepare_climatezone_factor() |>
    dplyr::mutate(
      summary_colour = resolve_untruncated_importance_colour(
        .data[["pooled_allocation"]],
        palette = signed_palette
      )
    )
  data_region_summary <-
    summarise_untruncated_spatial_importance(
      data_importance = data_spatial,
      group_vars = c("analysis", "region"),
      region_levels = region_levels
    ) |>
    dplyr::mutate(
      line_colour = resolve_untruncated_importance_colour(
        .data[["pooled_allocation"]],
        palette = signed_palette
      )
    )

  data_quantiles <-
    data_records |>
    dplyr::group_by(
      .data[["region"]],
      .data[["climatezone"]],
      .data[["climatezone_label"]]
    ) |>
    dplyr::summarise(
      q_95_lwr = stats::quantile(
        .data[["signed_allocation"]],
        0.025,
        na.rm = TRUE
      ),
      q_95_upr = stats::quantile(
        .data[["signed_allocation"]],
        0.975,
        na.rm = TRUE
      ),
      q_75_lwr = stats::quantile(
        .data[["signed_allocation"]],
        0.125,
        na.rm = TRUE
      ),
      q_75_upr = stats::quantile(
        .data[["signed_allocation"]],
        0.875,
        na.rm = TRUE
      ),
      q_50_lwr = stats::quantile(
        .data[["signed_allocation"]],
        0.25,
        na.rm = TRUE
      ),
      q_50_upr = stats::quantile(
        .data[["signed_allocation"]],
        0.75,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("q_"),
      names_to = c("interval", ".value"),
      names_pattern = "q_(\\d+)_(lwr|upr)"
    )

  raw_range <-
    range(
      c(
        data_records[["signed_allocation"]],
        data_climatezone_summary[["pooled_allocation"]],
        data_region_summary[["pooled_allocation"]]
      ),
      finite = TRUE
    )
  range_padding <- max(diff(raw_range) * 0.04, 0.04)
  y_limits <- raw_range + c(-range_padding, range_padding)
  display_limits <- c(-0.5, 1.5)
  y_breaks <- seq(display_limits[[1]], display_limits[[2]], by = 0.5)

  density_groups <-
    data_records |>
    dplyr::group_split(.data[["region"]])
  data_density <-
    density_groups |>
    purrr::map(
      .f = ~ {
        density_values <-
          stats::density(
            .x[["signed_allocation"]],
            from = y_limits[[1]],
            to = y_limits[[2]],
            n = 256,
            cut = 0
          )

        res <-
          tibble::tibble(
            region = factor(
              unique(.x[["region"]]),
              levels = region_levels
            ),
            signed_allocation = density_values[["x"]],
            density = density_values[["y"]]
          )

        return(res)
      }
    ) |>
    dplyr::bind_rows()
  density_max <- max(data_density[["density"]], na.rm = TRUE)
  data_density <-
    data_density |>
    dplyr::mutate(
      density_scaled = 3 * .data[["density"]] / density_max,
      panel_label = "Density"
    )
  density_x <- seq(0, 3, length.out = 90)
  data_density_tiles <-
    tidyr::crossing(data_density, density_x = density_x) |>
    dplyr::filter(.data[["density_x"]] <= .data[["density_scaled"]])

  panel_levels <-
    c("Density", levels(data_records[["climatezone_label"]]))
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
        .data[["panel_label"]] == "Density",
        3 * .data[["panel_edge"]],
        .data[["panel_edge"]]
      )
    )
  data_region_lines <-
    tidyr::crossing(
      data_region_summary,
      panel_label = factor(panel_levels, levels = panel_levels)
    )
  background_values <- seq(
    y_limits[[1]],
    y_limits[[2]],
    length.out = 241
  )
  background_step <- background_values[[2]] - background_values[[1]]
  data_background <-
    tidyr::crossing(
      region = factor(displayed_regions, levels = region_levels),
      panel_label = panel_levels[panel_levels != "Density"],
      signed_allocation = background_values
    ) |>
    dplyr::mutate(
      panel_label = factor(.data[["panel_label"]], levels = panel_levels),
      ymin = .data[["signed_allocation"]] - background_step / 2,
      ymax = .data[["signed_allocation"]] + background_step / 2
    )

  interval_layers <-
    purrr::map(
      c("95", "75", "50"),
      ~ ggplot2::geom_segment(
        data = data_quantiles |>
          dplyr::filter(.data[["interval"]] == .x),
        mapping = ggplot2::aes(
          x = .data[["panel_x"]],
          xend = .data[["panel_x"]],
          y = .data[["lwr"]],
          yend = .data[["upr"]],
          colour = .data[["climatezone"]]
        ),
        alpha = 0.85,
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
        fill = .data[["signed_allocation"]]
      ),
      colour = NA,
      alpha = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      colour = colorspace::lighten(common_gray, amount = 0.55),
      linewidth = line_size
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
        y = .data[["signed_allocation"]],
        fill = .data[["signed_allocation"]]
      ),
      width = 3 / length(density_x),
      height = diff(y_limits) / 256
    ) +
    build_untruncated_importance_fill_scale(
      palette = signed_palette,
      guide = ggplot2::guide_colourbar(
        title.position = "top",
        direction = "horizontal",
        barwidth = ggplot2::unit(45, "mm"),
        barheight = ggplot2::unit(3, "mm"),
        order = 1
      )
    ) +
    interval_layers +
    ggplot2::scale_colour_manual(
      values = palette_ecozones,
      drop = FALSE,
      guide = "none"
    ) +
    ggnewscale::new_scale_colour() +
    ggplot2::geom_hline(
      data = data_region_lines,
      mapping = ggplot2::aes(
        yintercept = .data[["pooled_allocation"]],
        colour = .data[["line_colour"]]
      ),
      linewidth = line_size * 8
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(
      data = data_climatezone_summary,
      mapping = ggplot2::aes(
        x = .data[["panel_x"]],
        y = .data[["pooled_allocation"]],
        fill = .data[["climatezone"]]
      ),
      shape = 21,
      colour = common_gray,
      stroke = line_size * 4,
      size = point_size * 3
    ) +
    ggplot2::scale_fill_manual(
      "Climate zone",
      values = palette_ecozones,
      drop = FALSE,
      guide = ggplot2::guide_legend(
        title.position = "top",
        nrow = 2,
        byrow = TRUE,
        order = 2
      )
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = y_breaks,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Signed human allocation of adjusted explained variation"
    ) +
    ggplot2::coord_cartesian(ylim = display_limits, expand = FALSE) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      plot.margin = grid::unit(c(5, 2, 4, 2), "mm"),
      panel.spacing.x = grid::unit(0, "mm"),
      panel.spacing.y = grid::unit(5, "mm"),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        size = text_size,
        colour = common_gray
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
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
      legend.box = "vertical",
      legend.box.just = "left",
      legend.spacing.y = grid::unit(1, "mm")
    )

  data_points <-
    data_meta |>
    dplyr::inner_join(
      data_records |>
        dplyr::select(
          dplyr::all_of(c("dataset_id", "signed_allocation"))
        ),
      by = "dataset_id"
    ) |>
    dplyr::distinct() |>
    prepare_climatezone_factor()
  map_list <-
    purrr::map(
      displayed_regions,
      ~ build_region_map(
        rasterdata = data_geo_koppen,
        select_region = .x,
        sel_alpha = 0
      ) +
        ggplot2::geom_point(
          data = data_points |>
            dplyr::filter(.data[["region"]] == .x),
          mapping = ggplot2::aes(
            x = .data[["long"]],
            y = .data[["lat"]],
            fill = .data[["climatezone"]]
          ),
          shape = 21,
          colour = common_gray,
          stroke = line_size * 2,
          size = point_size,
          show.legend = FALSE
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
        )
    )

  statistical_grob <- ggplot2::ggplotGrob(statistical_plot)
  panel_rows <-
    statistical_grob[["layout"]] |>
    dplyr::filter(grepl("^panel-", .data[["name"]])) |>
    dplyr::distinct(.data[["t"]]) |>
    dplyr::arrange(.data[["t"]]) |>
    dplyr::pull(.data[["t"]])
  map_panel_rows <- panel_rows[match(displayed_regions, region_levels)]
  assertthat::assert_that(
    length(map_panel_rows) == length(map_list),
    !anyNA(map_panel_rows),
    msg = "Could not align every signed map with its continent row."
  )
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
      .f = build_plot_grob_row
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
