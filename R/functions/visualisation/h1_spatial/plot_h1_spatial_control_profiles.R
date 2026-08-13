#' @title Plot spatial HVarPart control profiles
#' @description
#' Create matched maps for zero-truncated hierarchical composition,
#' untruncated hierarchical contributions, and unique adjusted R-squared.
#' @param data_records Time-controlled dataset-level presentation records.
#' @param data_components Extracted time-controlled HVarPart components.
#' @param data_unique_adjusted_r2 Extracted pure time-controlled fractions.
#' @return A named list containing three ggplot objects.
#' @examples
#' \dontrun{
#' plot_h1_spatial_control_profiles(records, components, partial)
#' }
plot_h1_spatial_control_profiles <- function(
  data_records,
  data_components,
  data_unique_adjusted_r2
) {
  keys <- c("dataset_id", "analysis")
  required_records <-
    c(
      keys,
      "long",
      "lat",
      "human_allocation",
      "climate_allocation",
      "time_allocation"
    )
  required_components <-
    c(keys, "model_profile", "predictor", "Individual")
  required_partial <- c(keys, "fraction", "adjusted_r_squared")
  assertthat::assert_that(
    all(required_records %in% names(data_records)),
    all(required_components %in% names(data_components)),
    all(required_partial %in% names(data_unique_adjusted_r2)),
    msg = "spatiotemporally controlled spatial analysis component inputs are invalid."
  )

  data_coordinates <-
    data_records |>
    dplyr::select(dplyr::all_of(c(keys, "long", "lat")))
  data_allocations <-
    data_records |>
    dplyr::select(
      dplyr::all_of(c(
        keys,
        "long",
        "lat",
        "human_allocation",
        "climate_allocation",
        "time_allocation"
      ))
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::ends_with("_allocation"),
      names_to = "component",
      values_to = "value",
      names_pattern = "(.*)_allocation"
    )
  data_signed <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate_time",
      .data[["predictor"]] %in% c("human", "climate", "time")
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      component = .data[["predictor"]],
      value = .data[["Individual"]]
    ) |>
    dplyr::left_join(data_coordinates, by = keys)
  data_unique <-
    data_unique_adjusted_r2 |>
    dplyr::filter(
      .data[["fraction"]] %in%
        c("pure_human", "pure_climate", "pure_time")
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      component = stringr::str_remove(.data[["fraction"]], "^pure_"),
      value = .data[["adjusted_r_squared"]]
    ) |>
    dplyr::left_join(data_coordinates, by = keys)
  data_world <- ggplot2::map_data("world")
  base_map <-
    ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = data_world,
      ggplot2::aes(
        x = .data[["long"]],
        y = .data[["lat"]],
        group = .data[["group"]]
      ),
      fill = "grey95",
      colour = "grey75",
      linewidth = line_size
    ) +
    ggplot2::coord_quickmap() +
    ggplot2::facet_wrap(ggplot2::vars(.data[["component"]]), ncol = 1) +
    ggplot2::theme_void(base_size = text_size) +
    ggplot2::theme(legend.position = "bottom")
  plot_allocation <-
    base_map +
    ggplot2::geom_point(
      data = data_allocations,
      ggplot2::aes(
        x = .data[["long"]],
        y = .data[["lat"]],
        colour = .data[["value"]]
      ),
      size = point_size,
      alpha = 0.7
    ) +
    ggplot2::scale_colour_viridis_c(limits = c(0, 1)) +
    ggplot2::labs(colour = "Allocation")
  plot_signed <-
    base_map +
    ggplot2::geom_point(
      data = data_signed,
      ggplot2::aes(
        x = .data[["long"]],
        y = .data[["lat"]],
        colour = .data[["value"]]
      ),
      size = point_size,
      alpha = 0.7
    ) +
    ggplot2::scale_colour_gradient2(midpoint = 0) +
    ggplot2::labs(colour = "Signed")
  plot_unique <-
    base_map +
    ggplot2::geom_point(
      data = data_unique,
      ggplot2::aes(
        x = .data[["long"]],
        y = .data[["lat"]],
        colour = .data[["value"]]
      ),
      size = point_size,
      alpha = 0.7
    ) +
    ggplot2::scale_colour_gradient2(midpoint = 0) +
    ggplot2::labs(colour = "Pure adjusted R2")

  return(
    list(
      zero_truncated_hierarchical_composition = plot_allocation,
      untruncated_hierarchical_contributions = plot_signed,
      unique_adjusted_r2 = plot_unique
    )
  )
}
