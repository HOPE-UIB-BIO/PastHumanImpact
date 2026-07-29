#' @title Recreate the spatial HVarPart figure with signed allocations
#' @description
#' Recreates the established region-map and predictor-panel composition using
#' unmodified signed HVarPart allocations. Central limits only affect the
#' display; the returned full-range plot and tables retain every value.
#' @param data_importance Canonical predictor-level HVarPart importance data.
#' @param data_meta Dataset metadata containing coordinates and spatial groups.
#' @param data_geo_koppen Spatial climate-zone object used by `get_map_region()`.
#' @param display_rounding Increment used to round global central limits.
#' @return A named list containing central/full-range plots and plot data.
plot_hvarpart_spatial_recreation <- function(
  data_importance,
  data_meta,
  data_geo_koppen,
  display_rounding = 0.05
) {
  required_importance <- c(
    "analysis", "dataset_id", "region", "climatezone", "predictor",
    "individual", "total_adjusted_r_squared", "is_importance_eligible"
  )
  assertthat::assert_that(
    is.data.frame(data_importance),
    all(required_importance %in% names(data_importance)),
    all(c("dataset_id", "long", "lat", "region", "climatezone") %in%
      names(data_meta)),
    msg = "Spatial recreation inputs do not satisfy the required contract."
  )

  data_records <-
    data_importance |>
    dplyr::filter(
      .data[["analysis"]] == "spatial_spd",
      .data[["is_importance_eligible"]]
    ) |>
    dplyr::mutate(
      signed_allocation = .data[["individual"]] /
        .data[["total_adjusted_r_squared"]]
    )

  display <- get_hvarpart_display_limits(
    data_source = data_records,
    value_col = "signed_allocation",
    rounding = display_rounding
  )

  data_summary <-
    summarise_hvarpart_importance(
      data_importance = data_importance |>
        dplyr::filter(.data[["analysis"]] == "spatial_spd"),
      group_vars = c("analysis", "region", "climatezone"),
      profile = "signed"
    )

  tail_note <- sprintf(
    "Outside central limits: %s below, %s above (all retained in tables)",
    display$tail_counts$n_below_display,
    display$tail_counts$n_above_display
  )

  build_panel <- function(use_central_limits) {
    panel <-
      ggplot2::ggplot(
        data_records,
        ggplot2::aes(
          x = .data[["predictor"]],
          y = .data[["signed_allocation"]],
          colour = .data[["predictor"]]
        )
      ) +
      ggplot2::geom_hline(yintercept = c(0, 1), colour = "grey70") +
      ggplot2::geom_jitter(
        width = 0.13,
        alpha = 0.28,
        size = point_size,
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = data_summary,
        mapping = ggplot2::aes(
          x = .data[["predictor"]],
          y = .data[["pooled_allocation"]],
          fill = .data[["predictor"]]
        ),
        shape = 21,
        colour = common_gray,
        size = point_size * 3,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data[["region"]]),
        cols = ggplot2::vars(.data[["climatezone"]]),
        scales = "free_x",
        space = "free_x"
      ) +
      ggplot2::scale_colour_manual(values = palette_predictors) +
      ggplot2::scale_fill_manual(values = palette_predictors) +
      ggplot2::labs(
        x = NULL,
        y = "Signed allocation of adjusted explained variation",
        subtitle = if (use_central_limits) tail_note else "Full signed range"
      ) +
      ggplot2::theme_bw(base_size = text_size) +
      ggplot2::theme(
        strip.text.y = ggplot2::element_text(angle = 0),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.spacing = grid::unit(1.5, "mm")
      )

    if (use_central_limits) {
      data_tails <-
        data_records |>
        dplyr::filter(
          .data[["signed_allocation"]] < display$limits[[1L]] |
            .data[["signed_allocation"]] > display$limits[[2L]]
        ) |>
        dplyr::mutate(
          displayed_allocation = pmin(
            pmax(.data[["signed_allocation"]], display$limits[[1L]]),
            display$limits[[2L]]
          ),
          tail = dplyr::if_else(
            .data[["signed_allocation"]] < display$limits[[1L]],
            "below",
            "above"
          )
        )

      panel <-
        panel +
        ggplot2::geom_point(
          data = data_tails,
          mapping = ggplot2::aes(
            x = .data[["predictor"]],
            y = .data[["displayed_allocation"]],
            shape = .data[["tail"]]
          ),
          colour = common_gray,
          size = point_size * 1.5,
          inherit.aes = FALSE,
          show.legend = FALSE
        ) +
        ggplot2::scale_shape_manual(values = c(below = 6, above = 2)) +
        ggplot2::coord_cartesian(ylim = display$limits)
    }

    return(panel)
  }

  data_points <-
    data_meta |>
    dplyr::filter(.data[["dataset_id"]] %in% data_records$dataset_id)

  add_points <- function(map, region_name) {
    map +
      ggplot2::geom_point(
        data = data_points |>
          dplyr::filter(.data[["region"]] == region_name),
        ggplot2::aes(
          x = .data[["long"]],
          y = .data[["lat"]],
          colour = .data[["climatezone"]]
        ),
        size = point_size,
        show.legend = FALSE
      ) +
      ggplot2::scale_colour_manual(values = palette_ecozones) +
      ggplot2::theme(legend.position = "none")
  }

  map_regions <- intersect(
    c("North America", "Latin America", "Europe", "Asia", "Oceania"),
    unique(as.character(data_records$region))
  )
  map_list <-
    purrr::map(
      map_regions,
      ~ add_points(
        get_map_region(
          rasterdata = data_geo_koppen,
          select_region = .x,
          sel_alpha = 0
        ),
        .x
      )
    )
  maps <- cowplot::plot_grid(plotlist = map_list, ncol = 1)

  compose <- function(panel) {
    cowplot::plot_grid(
      maps,
      panel,
      nrow = 1,
      rel_widths = c(0.5, 2)
    )
  }

  return(
    list(
      central_plot = compose(build_panel(TRUE)),
      full_range_plot = compose(build_panel(FALSE)),
      record_values = data_records,
      summary_values = data_summary,
      tail_counts = display$tail_counts,
      display_limits = display$limits
    )
  )
}
