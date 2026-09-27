#' @title Validate shared climate selections across local HVarPart models
#' @description Compare the retained climate columns for two model
#' specifications within every analytical unit and stop if any differ.
#' @param data_designs Local HVarPart design table.
#' @param unit_columns Columns identifying an analytical unit.
#' @param reference_model Reference model identifier.
#' @param comparison_model Comparison model identifier.
#' @return A tibble auditing the climate set used by both models per unit.
#' @examples
#' \dontrun{
#' validate_shared_local_climate_selections(
#'   designs, "dataset_id", "joint_filtered", "spd_matched_bridge"
#' )
#' }
validate_shared_local_climate_selections <- function(
  data_designs,
  unit_columns,
  reference_model = "joint_filtered",
  comparison_model = "spd_matched_bridge"
) {
  assertthat::assert_that(
    is.data.frame(data_designs),
    all(c(unit_columns, "model_id", "predictor_vars") %in% names(data_designs)),
    is.list(data_designs[["predictor_vars"]]),
    reference_model %in% data_designs[["model_id"]],
    comparison_model %in% data_designs[["model_id"]],
    msg = "Shared-climate validation inputs do not satisfy the contract."
  )

  audit <- data_designs |>
    dplyr::filter(.data[["model_id"]] %in% c(
      .env[["reference_model"]], .env[["comparison_model"]]
    )) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(unit_columns)),
      model_id = .data[["model_id"]],
      climate_predictors = purrr::map_chr(
        .data[["predictor_vars"]],
        ~ paste(sort(.x[["climate"]]), collapse = " + ")
      )
    ) |>
    tidyr::pivot_wider(
      names_from = "model_id",
      values_from = "climate_predictors",
      names_prefix = "climate__"
    ) |>
    dplyr::mutate(
      identical_climate_set =
        .data[[paste0("climate__", reference_model)]] ==
        .data[[paste0("climate__", comparison_model)]]
    )

  expected_rows <- data_designs |>
    dplyr::distinct(dplyr::across(dplyr::all_of(unit_columns))) |>
    nrow()
  assertthat::assert_that(
    nrow(audit) == expected_rows,
    all(audit[["identical_climate_set"]] %in% TRUE),
    msg = paste(
      "Climate predictors must be identical between the matched SPD bridge",
      "and filtered joint model within every analytical unit."
    )
  )
  return(audit)
}
