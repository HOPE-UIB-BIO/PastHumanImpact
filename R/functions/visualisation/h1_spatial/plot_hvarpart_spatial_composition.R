#' @title Plot spatial HVarPart allocations
#' @description
#' Retains the original human-impact half of Figure 2: continent maps,
#' continent-wide densities, climate-zone distribution summaries, and pooled
#' continent reference lines. The main plot uses zero-truncated allocations;
#' the supplementary plot shows the complete signed range.
#' @param data_importance Canonical predictor-level HVarPart importance data.
#' @param data_meta Dataset metadata containing coordinates and spatial groups.
#' @param data_geo_koppen Spatial climate-zone object used by `build_region_map()`.
#' @return A named list containing main and supplementary plots and plot data.
#' @examples
#' \dontrun{
#' plots <- plot_hvarpart_spatial_composition(
#'   data_importance = importance,
#'   data_meta = metadata,
#'   data_geo_koppen = koppen
#' )
#' plots$main_plot
#' plots$signed_full_range_plot
#' }
plot_hvarpart_spatial_composition <- function(
  data_importance,
  data_meta,
  data_geo_koppen
) {
  required_importance <- c(
    "analysis", "model_id", "dataset_id", "region", "climatezone",
    "predictor", "individual", "total_adjusted_r_squared",
    "is_importance_eligible"
  )
  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_importance %in% names(data_importance)),
    all(c("dataset_id", "long", "lat", "region", "climatezone") %in%
      names(data_meta)),
    msg = "Spatial figure inputs do not satisfy the required contract."
  )

  region_levels <- c(
    "North America", "Latin America", "Europe", "Asia", "Oceania"
  )

  data_records <-
    data_importance |>
    dplyr::filter(
      .data[["analysis"]] == "spatial_spd",
      .data[["is_importance_eligible"]]
    ) |>
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

  make_summary <- function(group_vars, profile_name) {
    result <-
      summarise_hvarpart_importance(
        data_importance = data_importance |>
          dplyr::filter(.data[["analysis"]] == "spatial_spd"),
        group_vars = group_vars,
        profile = profile_name
      ) |>
      dplyr::filter(.data[["predictor"]] == "human") |>
      dplyr::mutate(
        profile = profile_name,
        region = factor(.data[["region"]], levels = region_levels)
      )

    return(result)
  }

  data_climatezone_summary <-
    dplyr::bind_rows(
      make_summary(
        c("analysis", "region", "climatezone"),
        "zero_truncated"
      ),
      make_summary(c("analysis", "region", "climatezone"), "signed")
    ) |>
    prepare_climatezone_factor()

  data_region_summary <-
    dplyr::bind_rows(
      make_summary(c("analysis", "region"), "zero_truncated"),
      make_summary(c("analysis", "region"), "signed")
    )

  data_points <-
    data_meta |>
    dplyr::filter(.data[["dataset_id"]] %in% data_records$dataset_id) |>
    prepare_climatezone_factor()

  add_points <- function(map, region_name) {
    result <-
      map +
      ggplot2::geom_point(
        data = data_points |>
          dplyr::filter(.data[["region"]] == region_name),
        ggplot2::aes(
          x = .data[["long"]],
          y = .data[["lat"]],
          colour = .data[["climatezone"]],
          fill = .data[["climatezone"]]
        ),
        size = point_size,
        shape = 16,
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_manual(
        values = palette_ecozones,
        drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = palette_ecozones,
        drop = FALSE
      ) +
      ggplot2::theme(legend.position = "none")

    return(result)
  }

  map_regions <-
    region_levels[region_levels %in% as.character(data_records$region)]
  map_list <-
    purrr::map(
      map_regions,
      ~ add_points(
        build_region_map(
          rasterdata = data_geo_koppen,
          select_region = .x,
          sel_alpha = 0
        ),
        .x
      )
    )
  maps <- cowplot::plot_grid(plotlist = map_list, ncol = 1)

  palette_ecozones_labels <-
    rlang::set_names(
      palette_ecozones,
      resolve_climatezone_label(names(palette_ecozones))
    )

  get_quantiles <- function(data_source) {
    result <-
      data_source |>
      dplyr::group_by(
        .data[["region"]],
        .data[["climatezone"]],
        .data[["climatezone_label"]],
        .data[["predictor"]]
      ) |>
      dplyr::summarise(
        q_95_lwr = stats::quantile(
          .data[["displayed_allocation"]],
          0.025,
          na.rm = TRUE
        ),
        q_95_upr = stats::quantile(
          .data[["displayed_allocation"]],
          0.975,
          na.rm = TRUE
        ),
        q_75_lwr = stats::quantile(
          .data[["displayed_allocation"]],
          0.125,
          na.rm = TRUE
        ),
        q_75_upr = stats::quantile(
          .data[["displayed_allocation"]],
          0.875,
          na.rm = TRUE
        ),
        q_50_lwr = stats::quantile(
          .data[["displayed_allocation"]],
          0.25,
          na.rm = TRUE
        ),
        q_50_upr = stats::quantile(
          .data[["displayed_allocation"]],
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

    return(result)
  }

  build_plot <- function(profile_name) {
    is_zero_truncated <- identical(profile_name, "zero_truncated")
    allocation_col <- if (
      is_zero_truncated
    ) {
      "zero_truncated_allocation"
    } else {
      "signed_allocation"
    }
    y_label <- if (
      is_zero_truncated
    ) {
      "Share of positive hierarchical contribution"
    } else {
      paste(
        "Untruncated signed hierarchical contribution",
        "(adjusted-R-squared scale)"
      )
    }
    reference_values <- if (
      is_zero_truncated
    ) {
      seq(0, 1, 0.25)
    } else {
      c(0, 1)
    }
    nonzero_reference_values <-
      reference_values[reference_values != 0]
    y_breaks <- if (
      is_zero_truncated
    ) {
      seq(0, 1, 0.25)
    } else {
      scales::breaks_pretty(n = 5)
    }

    profile_records <-
      data_records |>
      dplyr::mutate(
        displayed_allocation = .data[[allocation_col]]
      )
    profile_climatezone <-
      data_climatezone_summary |>
      dplyr::filter(.data[["profile"]] == profile_name)
    profile_region <-
      data_region_summary |>
      dplyr::filter(.data[["profile"]] == profile_name)
    data_quantiles <- get_quantiles(profile_records)

    y_limits <- if (
      is_zero_truncated
    ) {
      c(0, 1)
    } else {
      range(
        c(
          profile_records$displayed_allocation,
          profile_climatezone$pooled_allocation,
          profile_region$pooled_allocation
        ),
        finite = TRUE
      )
    }

    common_theme <-
      ggplot2::theme_bw(base_size = text_size) +
      ggplot2::theme(
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
        panel.spacing.y = grid::unit(5, "mm"),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(
          size = text_size,
          colour = common_gray
        ),
        line = ggplot2::element_line(
          linewidth = line_size,
          colour = common_gray
        )
      )

    density_plot <-
      ggplot2::ggplot() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["region"]]),
        cols = ggplot2::vars(.data[["predictor"]]),
        switch = "both",
        drop = FALSE
      ) +
      ggplot2::geom_hline(
        yintercept = nonzero_reference_values,
        colour = colorspace::lighten(common_gray, amount = 0.5),
        alpha = 0.5,
        linewidth = line_size
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = colorspace::lighten(common_gray, amount = 0.5),
        alpha = 0.5,
        linewidth = line_size,
        linetype = 2
      ) +
      ggplot2::geom_density(
        data = profile_records,
        mapping = ggplot2::aes(
          y = .data[["displayed_allocation"]]
        ),
        trim = FALSE,
        fill = palette_predictors[["human"]],
        colour = NA
      ) +
      ggplot2::geom_segment(
        data = profile_region,
        mapping = ggplot2::aes(
          x = Inf,
          xend = -Inf,
          y = .data[["pooled_allocation"]],
          yend = .data[["pooled_allocation"]]
        ),
        linewidth = line_size * 10,
        colour = colorspace::darken(
          palette_predictors[["human"]],
          amount = 0.3
        )
      ) +
      ggplot2::scale_x_continuous(trans = "reverse") +
      ggplot2::scale_y_continuous(
        position = "right",
        breaks = y_breaks
      ) +
      ggplot2::coord_cartesian(ylim = y_limits) +
      common_theme +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank()
      )

    interval_layers <-
      purrr::map(
        c("95", "75", "50"),
        ~ ggplot2::geom_segment(
          data = data_quantiles |>
            dplyr::filter(.data[["interval"]] == .x),
          mapping = ggplot2::aes(
            x = .data[["predictor"]],
            xend = .data[["predictor"]],
            y = .data[["lwr"]],
            yend = .data[["upr"]],
            colour = .data[["climatezone_label"]]
          ),
          alpha = 0.8,
          linewidth = (0.1 + (1 - as.numeric(.x) / 100)) * 5
        )
      )

    summary_plot <-
      ggplot2::ggplot() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["region"]]),
        cols = ggplot2::vars(.data[["climatezone_label"]]),
        switch = "both",
        drop = FALSE
      ) +
      ggplot2::geom_hline(
        yintercept = nonzero_reference_values,
        colour = colorspace::lighten(common_gray, amount = 0.5),
        alpha = 0.5,
        linewidth = line_size
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = colorspace::lighten(common_gray, amount = 0.5),
        alpha = 0.5,
        linewidth = line_size,
        linetype = 2
      ) +
      interval_layers +
      ggplot2::geom_segment(
        data = profile_region,
        mapping = ggplot2::aes(
          x = -Inf,
          xend = Inf,
          y = .data[["pooled_allocation"]],
          yend = .data[["pooled_allocation"]]
        ),
        linewidth = line_size * 10,
        colour = colorspace::darken(
          palette_predictors[["human"]],
          amount = 0.3
        )
      ) +
      ggplot2::geom_point(
        data = profile_climatezone,
        mapping = ggplot2::aes(
          x = .data[["predictor"]],
          y = .data[["pooled_allocation"]],
          fill = .data[["climatezone_label"]]
        ),
        shape = 21,
        colour = common_gray,
        size = point_size * 3
      ) +
      ggplot2::scale_colour_manual(
        values = palette_ecozones_labels,
        drop = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = palette_ecozones_labels,
        drop = FALSE
      ) +
      ggplot2::scale_y_continuous(
        position = "right",
        breaks = y_breaks
      ) +
      ggplot2::coord_cartesian(ylim = y_limits) +
      ggplot2::labs(x = NULL, y = y_label) +
      common_theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_blank(),
        strip.text.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = text_size,
          colour = common_gray
        ),
        axis.title.y = ggplot2::element_text(
          size = text_size,
          colour = common_gray
        )
      )

    statistical_panels <-
      cowplot::plot_grid(
        density_plot,
        summary_plot,
        nrow = 1,
        align = "v",
        axis = "tb",
        rel_widths = c(0.32, 0.95)
      )
    maps_with_spacer <-
      cowplot::plot_grid(
        maps,
        NULL,
        ncol = 1,
        rel_heights = c(1, 0.075)
      )

    res <-
      cowplot::plot_grid(
        maps_with_spacer,
        statistical_panels,
        nrow = 1,
        rel_widths = c(0.5, 2)
      )

    return(res)
  }

  res <-
    list(
      main_plot = build_plot("zero_truncated"),
      signed_full_range_plot = build_plot("signed"),
      record_values = data_records,
      summary_values = data_climatezone_summary,
      region_values = data_region_summary
    )

  return(res)
}
