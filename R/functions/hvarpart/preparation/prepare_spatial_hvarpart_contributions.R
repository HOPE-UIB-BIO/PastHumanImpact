#' @title Prepare signed spatial-control contributions for display
#' @description
#' Retain fitted human, climate, and space contributions and explicitly promote
#' the valid human-climate fit when dbMEM selection finds no spatial terms. In
#' that case, the spatial contribution is exactly zero.
#' @param data_components Extracted region-age HVarPart component table.
#' @param data_status Extracted region-age spatial-selection status table.
#' @return One signed contribution row per analysis, region, age, and predictor.
#' @examples
#' \dontrun{
#' prepare_spatial_hvarpart_contributions(components, statuses)
#' }
prepare_spatial_hvarpart_contributions <- function(
  data_components,
  data_status
) {
  component_keys <- c("analysis", "region", "age")
  required_components <-
    c(component_keys, "model_profile", "predictor", "individual")
  required_status <- c(component_keys, "status")

  assertthat::assert_that(
    is.data.frame(data_components),
    all(required_components %in% names(data_components)),
    is.data.frame(data_status),
    all(required_status %in% names(data_status)),
    msg = "Spatial contribution inputs do not satisfy the required contract."
  )

  data_controlled <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate_space",
      .data[["predictor"]] %in% c("human", "climate", "space")
    )

  data_no_spatial_keys <-
    data_status |>
    dplyr::filter(.data[["status"]] == "no_spatial_terms_selected") |>
    dplyr::select(dplyr::all_of(component_keys))

  data_no_spatial <-
    data_no_spatial_keys |>
    dplyr::left_join(
      data_components |>
        dplyr::filter(
          .data[["model_profile"]] == "human_climate",
          .data[["predictor"]] %in% c("human", "climate")
        ),
      by = component_keys
    ) |>
    dplyr::mutate(model_profile = "human_climate_space") |>
    dplyr::bind_rows(
      data_no_spatial_keys |>
        dplyr::mutate(
          model_profile = "human_climate_space",
          predictor = "space",
          individual = 0
        )
    )

  data_display <-
    dplyr::bind_rows(data_controlled, data_no_spatial) |>
    dplyr::arrange(
      .data[["analysis"]],
      .data[["region"]],
      .data[["age"]],
      factor(
        .data[["predictor"]],
        levels = c("human", "climate", "space")
      )
    )

  duplicate_keys <-
    data_display |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(c(component_keys, "model_profile", "predictor"))
      )
    ) |>
    dplyr::filter(.data[["n"]] != 1L)

  if (
    nrow(duplicate_keys) > 0L ||
      any(!is.finite(data_display[["individual"]]))
  ) {
    cli::cli_abort(
      "Spatial-control display contributions must be unique and finite."
    )
  }

  return(data_display)
}
