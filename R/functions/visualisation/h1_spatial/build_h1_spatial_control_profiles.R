#' @title Build time-controlled H1 spatial component profiles
#' @description
#' Create matched one-variable spatial figures for human, climate, and time
#' using untruncated hierarchical contributions and unique adjusted R-squared.
#' @param data_records Time-controlled dataset-level records.
#' @param data_components Time-controlled hierarchical contributions.
#' @param data_unique_adjusted_r2 Time-controlled unique fractions.
#' @param data_geo_koppen Spatial climate-zone data.
#' @return Six named spatial component plot results.
#' @examples
#' \dontrun{
#' build_h1_spatial_control_profiles(
#'   records,
#'   components,
#'   fractions,
#'   climate_zones
#' )
#' }
build_h1_spatial_control_profiles <- function(
  data_records,
  data_components,
  data_unique_adjusted_r2,
  data_geo_koppen
) {
  assertthat::assert_that(
    is.data.frame(data_records),
    is.data.frame(data_components),
    is.data.frame(data_unique_adjusted_r2),
    is.data.frame(data_geo_koppen),
    msg = "Spatial control profile inputs must be data frames."
  )

  data_values <-
    prepare_h1_spatial_control_values(
      data_records = data_records,
      data_components = data_components,
      data_unique_adjusted_r2 = data_unique_adjusted_r2
    )

  measure_limits <-
    data_values |>
    dplyr::group_by(.data[["measure"]]) |>
    dplyr::summarise(
      lower = floor(min(.data[["value"]]) * 10) / 10,
      upper = ceiling(max(.data[["value"]]) * 10) / 10,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      lower = pmin(.data[["lower"]], 0),
      upper = pmax(.data[["upper"]], 0.1),
      value_limits = purrr::map2(.data[["lower"]], .data[["upper"]], c),
      value_breaks = purrr::map(
        .data[["value_limits"]],
        ~ scales::breaks_pretty(n = 4)(.x)
      )
    )

  plot_configuration <-
    tidyr::crossing(
      measure = c(
        "untruncated_hierarchical_contribution",
        "unique_adjusted_r2"
      ),
      component = c("human", "climate", "time")
    ) |>
    dplyr::mutate(
      component_colour = dplyr::recode(
        .data[["component"]],
        human = palette_predictors[["human"]],
        climate = palette_predictors[["climate"]],
        time = paletete_age[["old"]]
      ),
      legend_title = stringr::str_to_title(.data[["component"]]),
      y_axis_title = dplyr::if_else(
        .data[["measure"]] ==
          "untruncated_hierarchical_contribution",
        paste0(
          "Relative importance\n",
          "(Untruncated hierarchical contribution)"
        ),
        paste0(
          "Explained variation\n",
          "(Unique adjusted R\u00B2)"
        )
      ),
      output_name = paste(
        .data[["component"]],
        .data[["measure"]],
        sep = "_"
      )
    ) |>
    dplyr::left_join(measure_limits, by = "measure")

  plot_results <-
    plot_configuration |>
    dplyr::mutate(
      result = purrr::pmap(
        list(
          .data[["measure"]],
          .data[["component"]],
          .data[["component_colour"]],
          .data[["value_limits"]],
          .data[["value_breaks"]],
          .data[["y_axis_title"]],
          .data[["legend_title"]]
        ),
        ~ plot_h1_spatial_component_distribution(
          data_values = data_values |>
            dplyr::filter(
              .data[["measure"]] == ..1,
              .data[["component"]] == ..2
            ),
          data_geo_koppen = data_geo_koppen,
          component_colour = ..3,
          value_limits = ..4,
          value_breaks = ..5,
          y_axis_title = ..6,
          legend_title = ..7
        )
      )
    )

  return(
    rlang::set_names(
      plot_results[["result"]],
      plot_results[["output_name"]]
    )
  )
}

