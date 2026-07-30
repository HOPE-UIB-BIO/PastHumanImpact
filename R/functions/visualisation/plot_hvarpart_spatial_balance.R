#' @title Plot spatial HVarPart importance balance
#' @description
#' Creates the canonical spatial Figure 2 using the difference between
#' zero-truncated human and climate allocations. A value of one indicates
#' exclusively human importance, zero indicates equal importance, and minus
#' one indicates exclusively climate importance.
#' @param data_importance Canonical predictor-level HVarPart importance data.
#' @param data_meta Dataset metadata containing coordinates and spatial groups.
#' @param data_geo_koppen Spatial climate-zone object used by
#' `get_map_region()`.
#' @return
#' A named list containing the combined plot, statistical panel, and the
#' record-, climate-zone-, region-, and density-level source data.
#' @details
#' The density and climate-zone summaries are facets in one ggplot object.
#' Consequently, each continental pooled line has exactly the same vertical
#' coordinate in the density and every climate-zone panel. The equal-importance
#' line is dashed, and climate-zone summary points use a dark outline.
#' @examples
#' \dontrun{
#' result <- plot_hvarpart_spatial_balance(
#'   data_importance = importance,
#'   data_meta = metadata,
#'   data_geo_koppen = koppen
#' )
#' result$plot
#' }
plot_hvarpart_spatial_balance <- function(
  data_importance,
  data_meta,
  data_geo_koppen
) {
  required_importance <- c(
    "analysis", "model_id", "dataset_id", "region", "climatezone",
    "predictor", "individual", "is_importance_eligible"
  )
  required_metadata <- c(
    "dataset_id", "long", "lat", "region", "climatezone"
  )

  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_importance %in% names(data_importance)),
    msg = "`data_importance` does not satisfy the spatial balance contract."
  )
  assertthat::assert_that(
    is.data.frame(data_meta),
    all(required_metadata %in% names(data_meta)),
    msg = "`data_meta` does not satisfy the spatial balance contract."
  )

  region_levels <- c(
    "North America", "Latin America", "Europe", "Asia", "Oceania"
  )
  balance_palette <- c(
    palette_predictors[["climate"]],
    "#F2F2F2",
    palette_predictors[["human"]]
  )
  balance_colour <- scales::col_numeric(
    palette = balance_palette,
    domain = c(-1, 1)
  )

  data_profile <-
    data_importance |>
    dplyr::filter(
      .data[["analysis"]] == "spatial_spd",
      .data[["is_importance_eligible"]]
    ) |>
    dplyr::group_by(.data[["model_id"]]) |>
    dplyr::mutate(
      truncated_individual = pmax(.data[["individual"]], 0),
      truncated_total = sum(.data[["truncated_individual"]])
    ) |>
    dplyr::ungroup()

  invalid_profile <-
    data_profile |>
    dplyr::filter(
      !is.finite(.data[["truncated_total"]]) |
        .data[["truncated_total"]] <= 0
    )

  if (
    nrow(invalid_profile) > 0L
  ) {
    cli::cli_abort(
      "At least one eligible model has no positive truncated contribution."
    )
  }

  data_records <-
    data_profile |>
    dplyr::mutate(
      zero_truncated_allocation =
        .data[["truncated_individual"]] /
          .data[["truncated_total"]]
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "model_id",
          "dataset_id",
          "region",
          "climatezone",
          "predictor",
          "zero_truncated_allocation"
        )
      )
    ) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "zero_truncated_allocation"
    )

  if (
    !all(c("human", "climate") %in% names(data_records)) ||
      any(!is.finite(data_records[["human"]])) ||
      any(!is.finite(data_records[["climate"]]))
  ) {
    cli::cli_abort(
      "Every eligible model must have finite human and climate allocations."
    )
  }

  data_records <-
    data_records |>
    dplyr::mutate(
      importance_balance =
        .data[["human"]] - .data[["climate"]],
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    add_climatezone_as_factor()

  if (
    any(abs(data_records[["importance_balance"]]) > 1 + 1e-10)
  ) {
    cli::cli_abort(
      "Zero-truncated human-climate balances must be between -1 and 1."
    )
  }

  summarise_balance <- function(group_vars) {
    data_summary <-
      summarise_hvarpart_importance(
        data_importance = data_importance |>
          dplyr::filter(.data[["analysis"]] == "spatial_spd"),
        group_vars = group_vars,
        profile = "zero_truncated"
      ) |>
      dplyr::select(
        dplyr::all_of(
          c(
            group_vars,
            "predictor",
            "pooled_allocation",
            "n_models"
          )
        )
      ) |>
      tidyr::pivot_wider(
        names_from = "predictor",
        values_from = "pooled_allocation"
      )

    if (
      !all(c("human", "climate") %in% names(data_summary))
    ) {
      cli::cli_abort(
        "Pooled summaries must contain human and climate allocations."
      )
    }

    data_summary <-
      data_summary |>
      dplyr::mutate(
        importance_balance =
          .data[["human"]] - .data[["climate"]],
        region = factor(.data[["region"]], levels = region_levels)
      ) |>
      dplyr::arrange(.data[["region"]])

    return(data_summary)
  }

  data_climatezone_summary <-
    summarise_balance(
      c("analysis", "region", "climatezone")
    ) |>
    add_climatezone_as_factor() |>
    dplyr::mutate(
      climate_colour = unname(
        palette_ecozones[as.character(.data[["climatezone"]])]
      )
    )
  data_region_summary <-
    summarise_balance(c("analysis", "region")) |>
    dplyr::mutate(
      line_colour = balance_colour(.data[["importance_balance"]])
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
        .data[["importance_balance"]],
        0.025,
        na.rm = TRUE
      ),
      q_95_upr = stats::quantile(
        .data[["importance_balance"]],
        0.975,
        na.rm = TRUE
      ),
      q_75_lwr = stats::quantile(
        .data[["importance_balance"]],
        0.125,
        na.rm = TRUE
      ),
      q_75_upr = stats::quantile(
        .data[["importance_balance"]],
        0.875,
        na.rm = TRUE
      ),
      q_50_lwr = stats::quantile(
        .data[["importance_balance"]],
        0.25,
        na.rm = TRUE
      ),
      q_50_upr = stats::quantile(
        .data[["importance_balance"]],
        0.75,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("q_"),
      names_to = c("interval", ".value"),
      names_pattern = "q_(\\d+)_(lwr|upr)"
    ) |>
    dplyr::mutate(
      climate_colour = unname(
        palette_ecozones[as.character(.data[["climatezone"]])]
      )
    )

  density_groups <-
    data_records |>
    dplyr::group_split(.data[["region"]])
  data_density <-
    density_groups |>
    purrr::map_dfr(
      .f = ~ {
        region_name <- unique(.x[["region"]])
        density_values <-
          stats::density(
            .x[["importance_balance"]],
            from = -1,
            to = 1,
            n = 256,
            cut = 0
          )

        return(
          tibble::tibble(
            region = factor(region_name, levels = region_levels),
            importance_balance = pmin(
              pmax(density_values[["x"]], -1),
              1
            ),
            density = density_values[["y"]]
          )
        )
      }
    )
  density_max <- max(data_density[["density"]], na.rm = TRUE)
  data_density <-
    data_density |>
    dplyr::mutate(
      density_scaled = 3 * .data[["density"]] / density_max,
      panel_label = "Density"
    )
  density_x <- seq(0, 3, length.out = 90)
  data_density_tiles <-
    tidyr::crossing(
      data_density,
      density_x = density_x
    ) |>
    dplyr::filter(
      .data[["density_x"]] <= .data[["density_scaled"]]
    )

  panel_levels <- c(
    "Density",
    levels(data_records[["climatezone_label"]])
  )
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
      panel_label = factor(
        .data[["panel_label"]],
        levels = panel_levels
      )
    )
  data_density_tiles <-
    data_density_tiles |>
    dplyr::mutate(
      panel_label = factor(
        .data[["panel_label"]],
        levels = panel_levels
      )
    )

  displayed_regions <-
    region_levels[
      region_levels %in% as.character(data_records[["region"]])
    ]
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
  background_values <- seq(-1, 1, length.out = 201)
  background_step <- background_values[[2]] - background_values[[1]]
  data_climatezone_background <-
    tidyr::crossing(
      region = factor(displayed_regions, levels = region_levels),
      panel_label = panel_levels[panel_levels != "Density"],
      background_balance = background_values
    ) |>
    dplyr::mutate(
      panel_label = factor(.data[["panel_label"]], levels = panel_levels),
      ymin = .data[["background_balance"]] - background_step / 2,
      ymax = .data[["background_balance"]] + background_step / 2
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
          colour = .data[["climate_colour"]]
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
      mapping = ggplot2::aes(
        x = .data[["panel_x"]],
        y = 0
      )
    ) +
    ggplot2::geom_rect(
      data = data_climatezone_background,
      mapping = ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        fill = .data[["background_balance"]]
      ),
      colour = NA,
      alpha = 0.25,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = c(-1, 1),
      colour = colorspace::lighten(common_gray, amount = 0.55),
      linewidth = line_size,
      alpha = 0.7
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
        y = .data[["importance_balance"]],
        fill = .data[["importance_balance"]]
      ),
      width = 3 / length(density_x),
      height = 2 / 256
    ) +
    interval_layers +
    ggplot2::geom_hline(
      data = data_region_lines,
      mapping = ggplot2::aes(
        yintercept = .data[["importance_balance"]],
        colour = .data[["line_colour"]]
      ),
      linewidth = line_size * 8
    ) +
    ggplot2::geom_point(
      data = data_climatezone_summary,
      mapping = ggplot2::aes(
        x = .data[["panel_x"]],
        y = .data[["importance_balance"]]
      ),
      colour = common_gray,
      size = point_size * 4
    ) +
    ggplot2::geom_point(
      data = data_climatezone_summary,
      mapping = ggplot2::aes(
        x = .data[["panel_x"]],
        y = .data[["importance_balance"]],
        colour = .data[["climate_colour"]]
      ),
      size = point_size * 3
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_y_continuous(
      position = "right",
      breaks = c(-1, 0, 1),
      labels = c(
        "Climate impact",
        "Equal",
        "Human impact"
      ),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_fill_gradient2(
      low = balance_palette[[1]],
      mid = balance_palette[[2]],
      high = balance_palette[[3]],
      midpoint = 0,
      limits = c(-1, 1),
      oob = scales::squish,
      guide = "none"
    ) +
    ggplot2::scale_colour_identity(
      breaks = unname(palette_ecozones),
      labels = names(palette_ecozones),
      guide = ggplot2::guide_legend(
        title = "Climate zone",
        nrow = 2,
        byrow = TRUE,
        override.aes = list(size = point_size * 3)
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Relative importance balance\n(human impact \u2212 climate)"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(-1.12, 1.12),
      expand = FALSE
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      plot.margin = grid::unit(c(5, 0, 0, 0), "mm"),
      panel.spacing.x = grid::unit(0, "mm"),
      panel.spacing.y = grid::unit(5, "mm"),
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
      legend.direction = "horizontal"
    )

  data_points <-
    data_meta |>
    dplyr::filter(
      .data[["dataset_id"]] %in% data_records[["dataset_id"]]
    ) |>
    add_climatezone_as_factor()
  map_list <-
    purrr::map(
      displayed_regions,
      ~ get_map_region(
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
            colour = .data[["climatezone"]]
          ),
          size = point_size,
          shape = 16,
          show.legend = FALSE
        ) +
        ggplot2::scale_colour_manual(
          values = palette_ecozones,
          drop = FALSE
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
  map_panel_rows <-
    panel_rows[match(displayed_regions, region_levels)]

  if (
    length(map_panel_rows) != length(map_list) ||
      anyNA(map_panel_rows)
  ) {
    cli::cli_abort(
      c(
        "The number of map rows must equal the number of statistical rows.",
        "i" = "Could not match every displayed continent to a facet row."
      )
    )
  }

  combined_grob <-
    gtable::gtable_add_cols(
      x = statistical_grob,
      widths = grid::unit(3.5, "null"),
      pos = 0
    )
  add_map_grob <- function(current_grob, map_plot, panel_row) {
    result <-
      gtable::gtable_add_grob(
        x = current_grob,
        grobs = ggplot2::ggplotGrob(map_plot),
        t = panel_row,
        b = panel_row,
        l = 1,
        r = 1,
        clip = "on"
      )

    return(result)
  }
  combined_plot <-
    purrr::reduce2(
      .x = map_list,
      .y = map_panel_rows,
      .init = combined_grob,
      .f = add_map_grob
    ) |>
    cowplot::ggdraw()

  return(
    list(
      plot = combined_plot,
      statistical_plot = statistical_plot,
      record_values = data_records,
      climatezone_values = data_climatezone_summary,
      region_values = data_region_summary,
      density_values = data_density
    )
  )
}
