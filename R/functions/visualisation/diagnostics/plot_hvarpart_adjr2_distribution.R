#' @title Plot spatial adjusted R-squared distributions
#' @description
#' Build a Figure 2-style continent-map and climate-zone grid showing the
#' record-level distribution of signed adjusted R-squared values from spatial
#' SPD models.
#' @param data_decomposition Model-level HVarPart decomposition data.
#' @param data_meta Dataset metadata containing coordinates.
#' @param data_geo_koppen Spatial climate-zone data used by `build_region_map()`.
#' @return A list containing the combined plot and record-, climate-zone-, and
#' continent-level source data.
plot_hvarpart_adjr2_distribution <- function(
  data_decomposition,
  data_meta,
  data_geo_koppen
) {
  required_decomposition <-
    c(
      "analysis",
      "model_id",
      "dataset_id",
      "region",
      "climatezone",
      "total_adjusted_r_squared",
      "has_finite_decomposition"
    )

  required_metadata <-
    c(
      "dataset_id",
      "long",
      "lat",
      "region",
      "climatezone"
    )

  assertthat::assert_that(
    is.data.frame(data_decomposition),
    all(required_decomposition %in% names(data_decomposition)),
    msg = "`data_decomposition` does not satisfy the distribution contract."
  )
  assertthat::assert_that(
    is.data.frame(data_meta),
    all(required_metadata %in% names(data_meta)),
    msg = "`data_meta` does not satisfy the distribution contract."
  )

  region_levels <-
    unname(vec_regions)

  climate_levels <-
    resolve_climatezone_label(
      data_climate_zones[["climatezone_label"]]
    )

  data_records <-
    data_decomposition |>
    dplyr::filter(
      .data[["analysis"]] == "spatial_spd",
      .data[["has_finite_decomposition"]],
      is.finite(.data[["total_adjusted_r_squared"]])
    ) |>
    dplyr::transmute(
      .data[["model_id"]],
      .data[["dataset_id"]],
      region = factor(.data[["region"]], levels = region_levels),
      .data[["climatezone"]],
      adjusted_r_squared = .data[["total_adjusted_r_squared"]]
    ) |>
    prepare_climatezone_factor()

  assertthat::assert_that(
    nrow(data_records) > 0L,
    msg = "No finite spatial SPD decompositions are available."
  )

  data_climatezone <-
    summarise_hvarpart_adjr2_distribution(
      data_values = data_records,
      group_vars = c("region", "climatezone", "climatezone_label")
    )
  data_continent <-
    summarise_hvarpart_adjr2_distribution(
      data_values = data_records,
      group_vars = "region"
    )
  data_scaffold <-
    tidyr::crossing(
      region = factor(region_levels, levels = region_levels),
      climatezone_label = factor(
        climate_levels,
        levels = climate_levels
      )
    ) |>
    dplyr::left_join(
      data_climatezone |>
        dplyr::select(
          .data[["region"]],
          .data[["climatezone_label"]],
          .data[["n_models"]]
        ),
      by = c("region", "climatezone_label")
    ) |>
    dplyr::mutate(
      annotation = dplyr::if_else(
        is.na(.data[["n_models"]]),
        "No models",
        stringr::str_c(
          "n=",
          .data[["n_models"]]
        )
      )
    )
  data_violin <-
    data_records |>
    dplyr::group_by(
      .data[["region"]],
      .data[["climatezone"]],
      .data[["climatezone_label"]]
    ) |>
    dplyr::filter(
      dplyr::n() >= 3L,
      dplyr::n_distinct(.data[["adjusted_r_squared"]]) > 1L
    ) |>
    dplyr::ungroup()
  y_limits <-
    range(
      c(
        0,
        data_records[["adjusted_r_squared"]]
      ),
      finite = TRUE
    )

  y_padding <-
    max(
      diff(y_limits) * 0.04,
      0.02
    )

  statistical_plot <-
    ggplot2::ggplot(
      data_records,
      ggplot2::aes(
        x = 1,
        y = .data[["adjusted_r_squared"]]
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
      mapping = ggplot2::aes(x = 1, y = y_limits[[1]]),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = line_size,
      colour = common_gray,
      linetype = 2
    ) +
    ggplot2::geom_violin(
      data = data_violin,
      ggplot2::aes(fill = .data[["climatezone"]]),
      width = 0.9,
      trim = FALSE,
      alpha = 0.3,
      colour = NA
    ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data[["climatezone"]]),
      width = 0.34,
      outlier.shape = NA,
      alpha = 0.55,
      linewidth = line_size * 1.5,
      colour = common_gray
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(colour = .data[["climatezone"]]),
      width = 0.14,
      height = 0,
      alpha = 0.55,
      size = point_size * 0.75
    ) +
    ggplot2::geom_text(
      data = data_scaffold,
      mapping = ggplot2::aes(
        x = 0.52,
        y = y_limits[[2]] + y_padding * 0.25,
        label = .data[["annotation"]]
      ),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 1,
      size = text_size / ggplot2::.pt * 0.6,
      colour = common_gray
    ) +
    ggplot2::scale_fill_manual(
      values = palette_ecozones,
      drop = FALSE,
      guide = "none"
    ) +
    ggplot2::scale_colour_manual(
      values = palette_ecozones,
      drop = FALSE,
      guide = "none"
    ) +
    ggplot2::scale_x_continuous(limits = c(0.48, 1.52)) +
    ggplot2::coord_cartesian(
      ylim = y_limits + c(-y_padding, y_padding),
      expand = FALSE
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Adjusted R\u00b2 across spatial SPD models"
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      panel.spacing = grid::unit(1, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        angle = 90,
        colour = common_gray
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        colour = common_gray
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = grid::unit(c(5, 0, 0, 0), "mm")
    )

  data_points <-
    data_meta |>
    dplyr::filter(
      .data[["dataset_id"]] %in% data_records[["dataset_id"]]
    ) |>
    prepare_climatezone_factor()
  map_list <-
    purrr::map(
      region_levels,
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
            colour = .data[["climatezone"]]
          ),
          size = point_size,
          show.legend = FALSE
        ) +
        ggplot2::scale_colour_manual(
          values = palette_ecozones,
          drop = FALSE
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.margin = grid::unit(rep(0, 4), "mm")
        )
    )

  statistical_grob <-
    ggplot2::ggplotGrob(statistical_plot)
  panel_rows <-
    statistical_grob[["layout"]] |>
    dplyr::filter(grepl("^panel-", .data[["name"]])) |>
    dplyr::distinct(.data[["t"]]) |>
    dplyr::arrange(.data[["t"]]) |>
    dplyr::pull(.data[["t"]])

  assertthat::assert_that(
    length(panel_rows) == length(map_list),
    msg = "Every canonical continent must match one distribution plot row."
  )

  combined_grob <-
    gtable::gtable_add_cols(
      x = statistical_grob,
      widths = grid::unit(3.5, "null"),
      pos = 0
    )
  for (
    map_index in seq_along(map_list)
  ) {
    combined_grob <-
      gtable::gtable_add_grob(
        x = combined_grob,
        grobs = ggplot2::ggplotGrob(map_list[[map_index]]),
        t = panel_rows[[map_index]],
        b = panel_rows[[map_index]],
        l = 1,
        r = 1,
        clip = "on"
      )
  }

  return(
    list(
      plot = combined_grob,
      statistical_plot = statistical_plot,
      record_values = data_records,
      climatezone_values = data_climatezone,
      continent_values = data_continent
    )
  )
}
