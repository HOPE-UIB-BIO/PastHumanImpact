#' @title Build main and signed H2 HVarPart figures
#' @description
#' Combines dbRDA trajectories with zero-truncated importance-balance
#' lollipops for the main figure and signed unstacked insets for the
#' supplementary figure.
#' @param output_h2 Fitted H2 output containing response, predictor, and HVarPart
#'   data.
#' @param data_meta Dataset metadata with region and climate-zone fields.
#' @param summary_zero_truncated Zero-truncated H2 pooled allocations.
#' @param summary_signed Signed H2 pooled allocations.
#' @return A named list containing the main and signed supplementary figures.
#' @examples
#' \dontrun{
#' figures <- build_hvarpart_h2_figure(
#'   output_h2 = fitted_h2,
#'   data_meta = metadata,
#'   summary_zero_truncated = h2_zero,
#'   summary_signed = h2_signed
#' )
#' figures$main_plot
#' figures$signed_full_range_plot
#' }
build_hvarpart_h2_figure <- function(
  output_h2,
  data_meta,
  summary_zero_truncated,
  summary_signed
) {
  assertthat::assert_that(
    is.data.frame(output_h2),
    all(c("region", "climatezone", "data_response_dist", "data_merge") %in%
      names(output_h2)),
    is.data.frame(data_meta),
    all(c("region", "climatezone", "ecozone_koppen_5") %in%
      names(data_meta)),
    requireNamespace("ggnewscale", quietly = TRUE),
    msg = "H2 figure inputs or required packages are unavailable."
  )

  data_trajectory <-
    output_h2 |>
    dplyr::mutate(
      mod_dbrda = purrr::map2(
        .data[["data_response_dist"]],
        .data[["data_merge"]],
        ~ run_dbrda(.x, .y)
      ),
      scores_dbrda = purrr::map(
        .data[["mod_dbrda"]],
        ~ get_scores_dbrda(.x)
      )
    ) |>
    dplyr::select(
      .data[["region"]],
      .data[["climatezone"]],
      .data[["scores_dbrda"]]
    ) |>
    dplyr::left_join(
      data_meta |>
        dplyr::select(
          .data[["region"]],
          .data[["climatezone"]],
          .data[["ecozone_koppen_5"]]
        ) |>
        dplyr::distinct(),
      by = c("region", "climatezone")
    ) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = vec_regions),
      ecozone_koppen_5 = factor(
        .data[["ecozone_koppen_5"]],
        levels = vec_climate_5
      )
    ) |>
    tidyr::unnest(.data[["scores_dbrda"]]) |>
    add_climatezone_as_factor()

  get_circle <- function(center = c(0, 0), radius = 1, npoints = 100) {
    angle <- seq(0, 2 * pi, length.out = npoints)
    return(
      data.frame(
        x = center[[1L]] + radius * cos(angle),
        y = center[[2L]] + radius * sin(angle)
      )
    )
  }

  add_circle <- function(
    plot,
    radius,
    line_colour = "grey75",
    line_type = 3,
    line_width = line_size * 4
  ) {
    return(
      plot +
        ggplot2::geom_path(
          data = get_circle(radius = radius),
          mapping = ggplot2::aes(
            x = .data[["x"]],
            y = .data[["y"]]
          ),
          linetype = line_type,
          colour = line_colour,
          linewidth = line_width
        )
    )
  }

  get_empty_plot <- function(
    line_colour = "grey85",
    legend_position = "none",
    draw_circles = TRUE,
    axis_limit = 1.5
  ) {
    result <-
      ggplot2::ggplot() +
      ggplot2::geom_vline(
        xintercept = 0,
        linetype = 2,
        linewidth = line_size,
        colour = line_colour
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2,
        linewidth = line_size,
        colour = line_colour
      ) +
      ggplot2::coord_fixed(
        xlim = c(-axis_limit, axis_limit),
        ylim = c(-axis_limit, axis_limit),
        expand = FALSE
      ) +
      ggplot2::theme_void(base_size = text_size) +
      ggplot2::theme(
        legend.position = legend_position,
        plot.background = ggplot2::element_rect(
          fill = "transparent",
          colour = NA
        ),
        plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")
      )

    if (isTRUE(draw_circles)) {
      result <- add_circle(result, radius = 0.5, line_colour = line_colour)
      result <- add_circle(result, radius = 1, line_colour = line_colour)
      result <- add_circle(result, radius = 1.5, line_colour = line_colour)
    }

    return(result)
  }

  get_trajectory_plot <- function(
    selected_region,
    selected_climatezone,
    legend_position = "none",
    draw_circles = TRUE
  ) {
    data_biplot <-
      data_trajectory |>
      dplyr::filter(
        .data[["region"]] == selected_region,
        .data[["climatezone"]] == selected_climatezone,
        .data[["score"]] == "biplot"
      ) |>
      dplyr::mutate(
        predictor_type = dplyr::if_else(
          .data[["label"]] == "spd",
          "human",
          "climate"
        )
      )

    data_sites <-
      data_trajectory |>
      dplyr::filter(
        .data[["region"]] == selected_region,
        .data[["climatezone"]] == selected_climatezone,
        .data[["score"]] == "sites"
      ) |>
      dplyr::mutate(age = as.numeric(.data[["label"]]) / 1000)

    return(
      get_empty_plot(
        line_colour = "grey55",
        legend_position = legend_position,
        draw_circles = draw_circles
      ) +
        ggplot2::geom_path(
          data = data_sites,
          mapping = ggplot2::aes(
            x = .data[["dbRDA1"]],
            y = .data[["dbRDA2"]],
            colour = .data[["age"]]
          ),
          lineend = "round",
          linejoin = "bevel",
          linewidth = 0.5,
          show.legend = TRUE
        ) +
        ggplot2::scale_colour_gradient(
          "Age ka BP",
          low = paletete_age[["young"]],
          high = paletete_age[["old"]]
        ) +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_segment(
          data = data_biplot,
          mapping = ggplot2::aes(
            x = 0,
            y = 0,
            xend = .data[["dbRDA1"]],
            yend = .data[["dbRDA2"]],
            colour = .data[["predictor_type"]]
          ),
          arrow = ggplot2::arrow(
            length = ggplot2::unit(0.03, "npc")
          ),
          linewidth = 0.75,
          show.legend = FALSE
        ) +
        ggplot2::scale_colour_manual(
          "Predictors",
          values = palette_predictors,
          drop = FALSE
        )
    )
  }

  get_combined_plot <- function(
    selected_region,
    selected_climatezone,
    importance_summary,
    profile
  ) {
    return(
      get_trajectory_plot(
        selected_region = selected_region,
        selected_climatezone = selected_climatezone
      ) +
        ggplot2::annotation_custom(
          grob = cowplot::as_grob(
            plot_hvarpart_h2_importance(
              data_summary = importance_summary,
              selected_region = selected_region,
              selected_climatezone = selected_climatezone,
              profile = profile
            )
          ),
          xmin = -1.42,
          xmax = -0.92,
          ymin = -1.42,
          ymax = 1.42
        )
    )
  }

  build_grid_plots <- function(importance_summary, profile) {
    grid_data <-
      tidyr::expand_grid(
        region = vec_regions,
        climatezone = data_climate_zones$climatezone_label
      ) |>
      dplyr::mutate(
        plot = purrr::map2(
          .data[["region"]],
          .data[["climatezone"]],
          ~ get_combined_plot(
            selected_region = .x,
            selected_climatezone = .y,
            importance_summary = importance_summary,
            profile = profile
          )
        )
      )

    empty_positions <- c(
      5, 8, 10, 13:17, 26:28, 30, 32, 37, 43, 45:50, 52:53, 55
    )
    grid_data$plot[empty_positions] <-
      purrr::map(empty_positions, ~ get_empty_plot())

    return(grid_data$plot)
  }

  predictor_legend <-
    cowplot::get_legend(
      plot_hvarpart_h2_importance(
        data_summary = summary_signed,
        selected_region = "Europe",
        selected_climatezone = "Polar",
        profile = "signed",
        legend_position = "bottom"
      )
    )
  age_legend <-
    cowplot::get_legend(
      ggplot2::ggplot(
        tibble::tibble(
          age = c(0, 8.5),
          x = c(0, 1),
          y = 0
        ),
        ggplot2::aes(
          x = .data[["x"]],
          y = .data[["y"]],
          colour = .data[["age"]]
        )
      ) +
        ggplot2::geom_point() +
        ggplot2::scale_colour_gradient(
          "Age ka BP",
          low = paletete_age[["young"]],
          high = paletete_age[["old"]]
        ) +
        ggplot2::theme_void(base_size = text_size) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.title = ggplot2::element_text(size = text_size),
          legend.text = ggplot2::element_text(size = text_size)
        )
    )

  main_importance_guide <-
    style_hvarpart_importance_guide(
      plot_hvarpart_h2_importance(
        data_summary = summary_zero_truncated,
        selected_region = "Europe",
        selected_climatezone = "Polar",
        profile = "zero_truncated"
      ),
      paste0(
        "Relative importance balance\n",
        "(human impact \u2212 climate)"
      )
    ) +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.3, 0.2, 0.3, 0.2), "cm")
    )

  signed_importance_guide <-
    style_hvarpart_importance_guide(
      plot_hvarpart_h2_importance(
        data_summary = summary_signed,
        selected_region = "Europe",
        selected_climatezone = "Polar",
        profile = "signed"
      ),
      "Signed allocation of adjusted explained variation"
    )

  trajectory_guide <-
    get_trajectory_plot(
      selected_region = "North America",
      selected_climatezone = "Cold - Cold Summer"
    )

  region_labels <- unname(region_labeller[vec_regions])
  climatezone_labels <- data_climate_zones$climatezone_label
  climatezone_colours <-
    unname(palette_ecozones[climatezone_labels])
  main_grid_plots <-
    build_grid_plots(summary_zero_truncated, "zero_truncated")
  signed_grid_plots <-
    build_grid_plots(summary_signed, "signed")
  main_composite_importance_guide <-
    main_importance_guide +
    ggplot2::labs(y = NULL) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  signed_composite_importance_guide <-
    signed_importance_guide +
    ggplot2::labs(y = NULL) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  main_plot <-
    compose_hvarpart_h2_figure(
      plot_list = main_grid_plots,
      region_labels = region_labels,
      climatezone_labels = climatezone_labels,
      climatezone_colours = climatezone_colours,
      importance_guide = main_composite_importance_guide,
      importance_title = "Relative importance",
      predictor_legend = predictor_legend,
      age_legend = age_legend,
      trajectory_guide = trajectory_guide
    )
  signed_full_range_plot <-
    compose_hvarpart_h2_figure(
      plot_list = signed_grid_plots,
      region_labels = region_labels,
      climatezone_labels = climatezone_labels,
      climatezone_colours = climatezone_colours,
      importance_guide = signed_composite_importance_guide,
      importance_title = "Signed allocation",
      predictor_legend = predictor_legend,
      age_legend = age_legend,
      trajectory_guide = trajectory_guide
    )

  return(
    list(
      main_plot = main_plot,
      signed_full_range_plot = signed_full_range_plot
    )
  )
}
