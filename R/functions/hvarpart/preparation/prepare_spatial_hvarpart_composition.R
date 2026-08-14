#' @title Prepare three-group Figure 3 stack values
#' @description
#' Convert region-age HVarPart components into exact-zero-truncated human,
#' climate, and space allocations, retaining explicit no-signal statuses.
#' @param data_components Region-age HVarPart component table.
#' @param data_status Region-age spatial-selection status table.
#' @return One row per analysis, region, age, and predictor.
#' @examples
#' \dontrun{
#' prepare_spatial_hvarpart_composition(components, statuses)
#' }
prepare_spatial_hvarpart_composition <- function(
  data_components,
  data_status
) {
  component_keys <- c("analysis", "region", "age")
  required_components <-
    c(component_keys, "model_profile", "predictor", "Individual")
  required_status <- c(component_keys, "status", "selection_status")
  assertthat::assert_that(
    is.data.frame(data_components),
    all(required_components %in% names(data_components)),
    is.data.frame(data_status),
    all(required_status %in% names(data_status)),
    msg = "Spatial stack inputs do not satisfy the required contract."
  )

  data_controlled <-
    data_components |>
    dplyr::filter(
      .data[["model_profile"]] == "human_climate_space"
    )
  data_no_signal <-
    data_status |>
    dplyr::filter(
      .data[["status"]] == "no_spatial_terms_selected"
    ) |>
    dplyr::select(dplyr::all_of(component_keys)) |>
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
      data_status |>
        dplyr::filter(
          .data[["status"]] == "no_spatial_terms_selected"
        ) |>
        dplyr::select(dplyr::all_of(component_keys)) |>
        dplyr::mutate(
          model_profile = "human_climate_space",
          predictor = "space",
          Individual = 0
        )
    )
  data_stack <-
    dplyr::bind_rows(data_controlled, data_no_signal) |>
    dplyr::filter(.data[["predictor"]] %in% c("human", "climate", "space")) |>
    dplyr::mutate(zero_individual = pmax(.data[["Individual"]], 0)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(component_keys))) |>
    dplyr::mutate(
      zero_total = sum(.data[["zero_individual"]]),
      allocation = dplyr::if_else(
        .data[["zero_total"]] > 0,
        .data[["zero_individual"]] / .data[["zero_total"]],
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(data_status, by = component_keys) |>
    dplyr::arrange(
      .data[["analysis"]],
      .data[["region"]],
      .data[["age"]],
      factor(
        .data[["predictor"]],
        levels = c("human", "climate", "space")
      )
    )

  return(data_stack)
}
