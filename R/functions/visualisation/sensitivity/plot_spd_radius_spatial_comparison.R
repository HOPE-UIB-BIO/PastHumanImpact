#' @title Plot paired spatial SPD-radius sensitivity
#' @description
#' Show dataset-level 250 km and 500 km human-climate balances and matched
#' geographic summaries using canonical human-gold and climate-teal semantics.
#' @param data_all_available Long dataset-radius source table.
#' @param data_summary Matched geographic summary table.
#' @param metric_profile HVAR presentation profile. The default
#'   `"zero_truncated"` shows the renormalised balance of positive human and
#'   climate contributions. `"untruncated_signed"` is a supplementary
#'   adjusted-R-squared diagnostic.
#' @return A two-panel patchwork figure.
#' @examples
#' \dontrun{
#' plot_spd_radius_spatial_comparison(all_available, summary)
#' }
plot_spd_radius_spatial_comparison <- function(
  data_all_available,
  data_summary,
  metric_profile = c("zero_truncated", "untruncated_signed")
) {
  metric_profile <-
    match.arg(metric_profile)

  metric_column <-
    if (metric_profile == "zero_truncated") {
      "zero_balance"
    } else {
      "signed_difference"
    }

  balance_label <-
    if (metric_profile == "zero_truncated") {
      "Zero-truncated human-climate importance balance"
    } else {
      paste(
        "Untruncated signed human-climate hierarchical contribution",
        "(adjusted-R-squared scale)"
      )
    }

  required_all <-
    c(
      "dataset_id",
      "radius_km",
      "region",
      metric_column,
      "status"
    )
  required_summary <-
    c(
      "summary_level",
      "metric",
      "region",
      "climatezone",
      "median_delta",
      "delta_q25",
      "delta_q75"
    )
  assertthat::assert_that(
    is.data.frame(data_all_available),
    all(required_all %in% names(data_all_available)),
    is.data.frame(data_summary),
    all(required_summary %in% names(data_summary)),
    msg = "Spatial SPD radius figure inputs do not satisfy the contract."
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")
  radius_region_labeller <- region_labeller
  radius_region_labeller[["Latin America"]] <-
    "Central &\nSouth America"
  data_plot <-
    data_all_available |>
    dplyr::filter(
      .data[["status"]] %in%
        c("estimated", "estimated_residual_temporal_dependence"),
      is.finite(.data[[metric_column]])
    ) |>
    dplyr::group_by(.data[["dataset_id"]]) |>
    dplyr::filter(dplyr::n_distinct(.data[["radius_km"]]) == 2L) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      radius = factor(
        stringr::str_c(.data[["radius_km"]], " km"),
        levels = c("250 km", "500 km")
      ),
      region = factor(.data[["region"]], levels = region_levels)
    )
  plot_pairs <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["radius"]],
        y = .data[[metric_column]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#6B6B6B") +
    ggplot2::geom_line(
      ggplot2::aes(group = .data[["dataset_id"]]),
      colour = "#8A8A8A",
      alpha = 0.10,
      linewidth = 0.25
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = .data[["radius"]],
        fill = .data[[metric_column]]
      ),
      colour = "#333333",
      alpha = 0.70,
      size = 1.7,
      stroke = 0.25
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["region"]]),
      nrow = 1,
      labeller = ggplot2::as_labeller(radius_region_labeller)
    ) +
    ggplot2::scale_x_discrete(
      labels = c("250 km" = "250", "500 km" = "500")
    ) +
    ggplot2::scale_shape_manual(
      values = c("250 km" = 21, "500 km" = 22),
      guide = "none"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 1),
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    ggplot2::scale_fill_gradient2(
      low = palette_predictors[["climate"]],
      mid = "#F2F2F2",
      high = palette_predictors[["human"]],
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1"),
      oob = scales::squish
    ) +
    ggplot2::labs(
      x = "SPD radius (km)",
      y = balance_label,
      fill = balance_label
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        order = 1,
        title.position = "top",
        barwidth = grid::unit(70, "pt"),
        barheight = grid::unit(5, "pt")
      )
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 0, unit = "pt")
      ),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center"
    )

  data_summary_plot <-
    data_summary |>
    dplyr::filter(
      .data[["summary_level"]] %in% c("region", "climatezone"),
      .data[["metric"]] == .env[["metric_column"]],
      is.finite(.data[["median_delta"]]),
      is.finite(.data[["delta_q25"]]),
      is.finite(.data[["delta_q75"]])
    ) |>
    prepare_climatezone_factor() |>
    dplyr::mutate(
      region_display = dplyr::recode(
        as.character(.data[["region"]]),
        !!!radius_region_labeller
      ),
      geographic_group = dplyr::case_when(
        .data[["summary_level"]] == "region" ~
          .data[["region_display"]],
        .default = as.character(.data[["climatezone"]])
      ),
      geographic_group = factor(
        .data[["geographic_group"]],
        levels = rev(c(
          unname(radius_region_labeller),
          data_climate_zones[["climatezone_label"]]
        ))
      )
    )
  plot_summaries <-
    ggplot2::ggplot(
      data_summary_plot,
      ggplot2::aes(
        x = .data[["median_delta"]],
        y = .data[["geographic_group"]]
      )
    ) +
    ggplot2::geom_vline(xintercept = 0, colour = "#6B6B6B") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data[["delta_q25"]],
        xmax = .data[["delta_q75"]]
      ),
      orientation = "y",
      width = 0,
      colour = "#6B6B6B"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        fill = .data[["median_delta"]]
      ),
      shape = 21,
      colour = "#333333",
      size = 2.5
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["summary_level"]]),
      scales = "free_y",
      labeller = ggplot2::as_labeller(
        c(region = "Region", climatezone = "Climate zone")
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1),
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    ggplot2::scale_fill_gradient2(
      low = palette_predictors[["climate"]],
      mid = "#F2F2F2",
      high = palette_predictors[["human"]],
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("-1", "0", "1"),
      oob = scales::squish
    ) +
    ggplot2::labs(
      x = paste(
        "Median change in human-climate importance balance",
        stringr::str_c("(", balance_label, "; 500 km - 250 km)"),
        sep = "\n"
      ),
      y = NULL,
      fill = "Median balance change"
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        order = 1,
        title.position = "top",
        barwidth = grid::unit(70, "pt"),
        barheight = grid::unit(5, "pt")
      )
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.title = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 7),
      legend.key.width = grid::unit(11, "pt"),
      legend.spacing.x = grid::unit(2, "pt")
    )

  res <-
    patchwork::wrap_plots(
      patchwork::free(
        plot_pairs,
        type = "space",
        side = "l"
      ),
      plot_summaries,
      ncol = 1,
      heights = c(1.3, 1)
    ) +
    patchwork::plot_annotation(
      tag_levels = "A",
      title = paste(
        "Sensitivity of spatial human-climate importance balance",
        "to SPD search radius"
      )
    )

  return(res)
}
