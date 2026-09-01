#' Add regional predictor provenance to an H1 result table
#'
#' @param data_source H1 result table containing `dataset_id` or `region`.
#' @param data_meta Dataset metadata used to resolve dataset continents.
#' @param proxy_variant Human-proxy variant key.
#'
#' @return `data_source` with requested events, retained human predictors, and
#'   reference category appended when a region can be resolved.
#'
#' @export
add_human_event_predictor_provenance <- function(
  data_source,
  data_meta,
  proxy_variant
) {
  if (!is.data.frame(data_source)) {
    return(data_source)
  }
  had_region <- "region" %in% names(data_source)
  if (!had_region && !"dataset_id" %in% names(data_source)) {
    return(data_source)
  }

  result <- data_source
  if (!had_region) {
    result <-
      result |>
      dplyr::left_join(
        data_meta |>
          dplyr::select(dplyr::all_of(c("dataset_id", "region"))) |>
          dplyr::distinct(),
        by = "dataset_id",
        relationship = "many-to-one"
      )
  }
  result <- result |> dplyr::mutate(.provenance_row = dplyr::row_number())
  aggregate_result <-
    result |>
    dplyr::filter(.data[["region"]] == "All") |>
    dplyr::mutate(
      requested_regional_events =
        "continent_specific;see_regional_model_audit",
      retained_human_predictors =
        "continent_specific;see_regional_model_audit",
      reference_category =
        "continent_specific;see_regional_model_audit"
    )
  regional_result <-
    result |>
    dplyr::filter(.data[["region"]] != "All") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      predictor_specification = list(
        resolve_region_event_predictor_specification(
          region = .data[["region"]],
          proxy_variant = proxy_variant
        )
      ),
      requested_regional_events = stringr::str_c(
        .data[["predictor_specification"]][["events"]],
        collapse = ";"
      ),
      retained_human_predictors = stringr::str_c(
        .data[["predictor_specification"]][["human"]],
        collapse = ";"
      ),
      reference_category =
        .data[["predictor_specification"]][["reference"]]
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of("predictor_specification"))
  result <-
    dplyr::bind_rows(regional_result, aggregate_result) |>
    dplyr::arrange(.data[[".provenance_row"]]) |>
    dplyr::select(-dplyr::all_of(".provenance_row"))

  if (!had_region) {
    result <- result |> dplyr::select(-dplyr::all_of("region"))
  }

  result
}
