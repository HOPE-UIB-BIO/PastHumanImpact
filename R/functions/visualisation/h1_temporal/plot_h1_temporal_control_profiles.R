#' @title Plot temporal HVarPart control profiles
#' @description
#' Keep untruncated hierarchical contributions and unique adjusted R-squared
#' values in distinct unstacked plots with canonical analysis and region order.
#' @param data_components Extracted spatial HVarPart component table.
#' @param data_unique_adjusted_r2 Extracted pure spatial fraction table.
#' @param data_status Optional region-age model-status table used to mark
#' non-estimable slices explicitly.
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
  data_unique_adjusted_r2,
  data_status = NULL
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
    is.null(data_status) || is.data.frame(data_status),
    is.null(data_status) ||
      all(c(key_columns, "status") %in% names(data_status)),
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

  signed_limits <- range(c(data_signed[["value"]], 0), finite = TRUE)
  signed_background_values <-
    seq(signed_limits[[1]], signed_limits[[2]], length.out = 201)
  signed_background_step <-
    signed_background_values[[2]] - signed_background_values[[1]]
  data_signed_background <-
    tidyr::crossing(
      region_label = factor(
        unname(region_labeller),
        levels = unname(region_labeller)
      ),
      analysis_label = factor(
        c("SPD", "Events"),
        levels = c("SPD", "Events")
      ),
      background_value = signed_background_values
    ) |>
    dplyr::mutate(
      ymin = .data[["background_value"]] - signed_background_step / 2,
      ymax = .data[["background_value"]] + signed_background_step / 2
    )

  data_missing <-
    if (is.null(data_status)) {
      tibble::tibble(
        region_label = factor(levels = unname(region_labeller)),
        analysis_label = factor(levels = c("SPD", "Events")),
        age_ka = numeric(),
        xmin = numeric(),
        xmax = numeric()
      )
    } else {
      data_status |>
        dplyr::filter(.data[["status"]] == "missing_predictor_group") |>
        dplyr::mutate(
          age_ka = .data[["age"]] / 1000,
          xmin = .data[["age_ka"]] - 0.25,
          xmax = .data[["age_ka"]] + 0.25,
          analysis_label = factor(
            .data[["analysis"]],
            levels = c("temporal_spd", "temporal_events"),
            labels = c("SPD", "Events")
          ),
          region_label = factor(
            .data[["region"]],
            levels = names(region_labeller),
            labels = unname(region_labeller)
          )
        )
    }

  data_missing_labels <-
    data_missing |>
    dplyr::group_by(
      .data[["region_label"]],
      .data[["analysis_label"]]
    ) |>
    dplyr::summarise(
      age_ka = mean(.data[["age_ka"]]),
      value = signed_limits[[2]] - 0.06 * diff(signed_limits),
      label = "Human proxy has no spatial variation",
      .groups = "drop"
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
    ggplot2::scale_fill_gradient2(
      low = colorspace::lighten(common_gray, amount = 0.35),
      mid = "white",
      high = colorspace::lighten(common_gray, amount = 0.35),
      midpoint = 0,
      limits = signed_limits,
      guide = "none"
    ) +
    ggplot2::labs(
      x = "Age (cal ka BP)",
      y = paste(
        "Untruncated signed hierarchical contribution",
        "(adjusted-R-squared scale)",
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
    ggplot2::geom_rect(
      data = data_signed_background,
      mapping = ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = .data[["ymin"]],
        ymax = .data[["ymax"]],
        fill = .data[["background_value"]]
      ),
      alpha = 0.16,
      colour = NA,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_rect(
      data = data_missing,
      mapping = ggplot2::aes(
        xmin = .data[["xmin"]],
        xmax = .data[["xmax"]],
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "grey82",
      alpha = 0.65,
      colour = NA,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = data_missing_labels,
      mapping = ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["value"]],
        label = .data[["label"]]
      ),
      colour = common_gray,
      size = text_size * 0.25,
      vjust = 1,
      inherit.aes = FALSE
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
      x = "Age (cal ka BP)",
      y = paste(
        "Explained variation",
        "(Unique adjusted R-squared)",
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
