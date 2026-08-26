#' @title Plot temporal HVarPart control profiles
#' @description
#' Keep untruncated hierarchical contributions and unique adjusted R-squared
#' values in distinct unstacked plots with canonical analysis and region order.
#' @param data_components Extracted spatial HVarPart component table.
#' @param data_unique_adjusted_r2 Extracted pure spatial fraction table.
#' @return A named list containing two explicitly labelled `ggplot` objects.
#' @examples
#' \dontrun{
#' plot_h1_temporal_control_profiles(
#'   data_components = components,
#'   data_unique_adjusted_r2 = partial
#' )
#' }
plot_h1_temporal_control_profiles <- function(
  data_components,
  data_unique_adjusted_r2
) {
  key_columns <-
    c("analysis", "region", "age")

  required_components <-
    c(
      key_columns,
      "model_profile",
      "predictor",
      "individual"
    )

  required_unique <-
    c(
      key_columns,
      "fraction",
      "adjusted_r_squared"
    )

  assertthat::assert_that(
    is.data.frame(data_components),
    all(required_components %in% names(data_components)),
    is.data.frame(data_unique_adjusted_r2),
    all(required_unique %in% names(data_unique_adjusted_r2)),
    msg = paste(
      "Spatially controlled temporal-analysis diagnostic inputs are",
      "invalid."
    )
  )

  data_signed <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate_space",
      .data[["predictor"]] %in% c("human", "climate", "space")
    ) |>
    dplyr::mutate(value = .data[["individual"]])

  data_unique <-
    data_unique_adjusted_r2 |>
    dplyr::filter(
      .data[["fraction"]] %in%
        c("pure_human", "pure_climate", "pure_space")
    ) |>
    dplyr::mutate(
      predictor = stringr::str_remove(
        .data[["fraction"]],
        "^pure_"
      ),
      value = .data[["adjusted_r_squared"]]
    )

  data_signed <-
    data_signed |>
    dplyr::mutate(
      age_ka = .data[["age"]] / 1000,
      analysis_label = factor(
        .data[["analysis"]],
        levels = c("temporal_spd", "temporal_events"),
        labels = c("SPD", "Events")
      ),
      region_label = factor(
        .data[["region"]],
        levels = names(region_labeller),
        labels = unname(region_labeller)
      ),
      series = dplyr::case_when(
        .data[["predictor"]] == "human" &
          .data[["analysis"]] == "temporal_events" ~ "human_events",
        .data[["predictor"]] == "human" ~ "human_spd",
        TRUE ~ .data[["predictor"]]
      )
    )

  data_unique <-
    data_unique |>
    dplyr::mutate(
      age_ka = .data[["age"]] / 1000,
      analysis_label = factor(
        .data[["analysis"]],
        levels = c("temporal_spd", "temporal_events"),
        labels = c("SPD", "Events")
      ),
      region_label = factor(
        .data[["region"]],
        levels = names(region_labeller),
        labels = unname(region_labeller)
      ),
      series = dplyr::case_when(
        .data[["predictor"]] == "human" &
          .data[["analysis"]] == "temporal_events" ~ "human_events",
        .data[["predictor"]] == "human" ~ "human_spd",
        TRUE ~ .data[["predictor"]]
      )
    )

  series_colours <-
    c(
      human_spd = palette_predictors[["human"]],
      human_events = "#DC702E",
      climate = palette_predictors[["climate"]],
      space = "#6F5B8B"
    )

  series_labels <-
    c(
      human_spd = "Human (SPD)",
      human_events = "Human (events)",
      climate = "Climate",
      space = "Space"
    )

  plot_signed <-
    ggplot2::ggplot(
      data_signed,
      ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["value"]],
        colour = .data[["series"]],
        group = .data[["series"]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region_label"]]),
      cols = ggplot2::vars(.data[["analysis_label"]]),
      switch = "y"
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_colour_manual(
      values = series_colours,
      breaks = names(series_labels),
      labels = series_labels
    ) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = paste(
        "Relative importance",
        "(Untruncated hierarchical contribution)",
        sep = "\n"
      ),
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background.y = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = common_gray,
      linetype = "dashed",
      linewidth = line_size
    ) +
    ggplot2::geom_line(linewidth = line_size * 1.5) +
    ggplot2::geom_point(size = point_size)

  plot_unique <-
    ggplot2::ggplot(
      data_unique,
      ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["value"]],
        colour = .data[["series"]],
        group = .data[["series"]]
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region_label"]]),
      cols = ggplot2::vars(.data[["analysis_label"]]),
      switch = "y"
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_colour_manual(
      values = series_colours,
      breaks = names(series_labels),
      labels = series_labels
    ) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = paste(
        "Unique adjusted R2",
        "(Pure conditional fraction)",
        sep = "\n"
      ),
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.background.y = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = common_gray,
      linetype = "dashed",
      linewidth = line_size
    ) +
    ggplot2::geom_line(linewidth = line_size * 1.5) +
    ggplot2::geom_point(size = point_size)

  res <-
    list(
      untruncated_hierarchical_contributions = plot_signed,
      unique_adjusted_r2 = plot_unique
    )

  return(res)
}
