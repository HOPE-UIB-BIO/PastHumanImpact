#' @title Plot temporal HVarPart control profiles
#' @description
#' Keep untruncated hierarchical contributions and unique adjusted R-squared
#' values in distinct unstacked plots.
#' @param data_components Extracted spatial HVarPart component table.
#' @param data_unique_adjusted_r2 Extracted pure spatial fraction table.
#' @return A named list containing two explicitly labelled ggplot objects.
#' @examples
#' \dontrun{
#' plot_h1_temporal_control_profiles(components, partial)
#' }
plot_h1_temporal_control_profiles <- function(
  data_components,
  data_unique_adjusted_r2
) {
  keys <- c("analysis", "region", "age")
  required_components <-
    c(keys, "model_profile", "predictor", "individual")
  required_partial <- c(keys, "fraction", "adjusted_r_squared")
  assertthat::assert_that(
    all(required_components %in% names(data_components)),
    all(required_partial %in% names(data_unique_adjusted_r2)),
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
      predictor = stringr::str_remove(.data[["fraction"]], "^pure_"),
      value = .data[["adjusted_r_squared"]]
    )
  data_signed <-
    data_signed |>
    dplyr::mutate(age_ka = .data[["age"]] / 1000)
  data_unique <-
    data_unique |>
    dplyr::mutate(age_ka = .data[["age"]] / 1000)
  predictor_colours <-
    c(
      human = palette_predictors[["human"]],
      climate = palette_predictors[["climate"]],
      space = "#6f5b8b"
    )
  plot_signed <-
    ggplot2::ggplot(
      data_signed,
      ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["value"]],
        colour = .data[["predictor"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data[["predictor"]]),
      linewidth = line_size * 2
    ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      cols = ggplot2::vars(.data[["analysis"]])
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_colour_manual(values = predictor_colours) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = "Signed hierarchical contribution",
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size)
  plot_unique <-
    ggplot2::ggplot(
      data_unique,
      ggplot2::aes(
        x = .data[["age_ka"]],
        y = .data[["value"]],
        colour = .data[["predictor"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data[["predictor"]]),
      linewidth = line_size * 2
    ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      cols = ggplot2::vars(.data[["analysis"]])
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::scale_colour_manual(values = predictor_colours) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = "Pure adjusted R2 fraction",
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size)

  return(
    list(
      untruncated_hierarchical_contributions = plot_signed,
      unique_adjusted_r2 = plot_unique
    )
  )
}
