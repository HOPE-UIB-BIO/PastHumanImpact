#' @title Prepare time-controlled H1 spatial component values
#' @description
#' Join within-dataset time-controlled hierarchical contributions and unique
#' adjusted R-squared fractions to their spatial metadata.
#' @param data_records Time-controlled dataset-level records.
#' @param data_components Time-controlled hierarchical contributions.
#' @param data_unique_adjusted_r2 Time-controlled unique fractions.
#' @return A long tibble with one row per dataset, measure, and component.
#' @examples
#' \dontrun{
#' prepare_h1_spatial_control_values(records, components, fractions)
#' }
prepare_h1_spatial_control_values <- function(
  data_records,
  data_components,
  data_unique_adjusted_r2
) {
  keys <- c("dataset_id", "analysis")
  spatial_columns <-
    c(keys, "region", "climatezone", "long", "lat")

  assertthat::assert_that(
    all(spatial_columns %in% names(data_records)),
    all(
      c(keys, "model_profile", "predictor", "individual") %in%
        names(data_components)
    ),
    all(
      c(keys, "fraction", "adjusted_r_squared") %in%
        names(data_unique_adjusted_r2)
    ),
    msg = "Time-controlled spatial profile inputs are invalid."
  )

  data_spatial <-
    data_records |>
    dplyr::select(dplyr::all_of(spatial_columns)) |>
    dplyr::distinct()

  duplicate_spatial_keys <-
    data_spatial |>
    dplyr::count(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::filter(.data[["n"]] != 1L)

  if (
    nrow(duplicate_spatial_keys) > 0L
  ) {
    cli::cli_abort(
      "Each time-controlled dataset must have one spatial record."
    )
  }

  data_hierarchical <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate_time",
      .data[["predictor"]] %in% c("human", "climate", "time")
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      measure = "untruncated_hierarchical_contribution",
      component = .data[["predictor"]],
      value = .data[["individual"]]
    )

  data_unique <-
    data_unique_adjusted_r2 |>
    dplyr::filter(
      .data[["fraction"]] %in%
        c("pure_human", "pure_climate", "pure_time")
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      measure = "unique_adjusted_r2",
      component = stringr::str_remove(
        .data[["fraction"]],
        "^pure_"
      ),
      value = .data[["adjusted_r_squared"]]
    )

  data_values <-
    dplyr::bind_rows(data_hierarchical, data_unique) |>
    dplyr::inner_join(data_spatial, by = keys)

  duplicate_value_keys <-
    data_values |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(c(keys, "measure", "component"))
      )
    ) |>
    dplyr::filter(.data[["n"]] != 1L)

  if (
    nrow(duplicate_value_keys) > 0L ||
      any(!is.finite(data_values[["value"]]))
  ) {
    cli::cli_abort(
      "Time-controlled spatial values must be unique and finite."
    )
  }

  return(data_values)
}
