#' @title Prepare filtered joint-human HVarPart comparison tables
#' @description Retain eligible estimates from the single locally filtered
#' joint-human model and prepare the spatial balance and temporal composition
#' source tables used by the two primary figures.
#' @param temporal_summary Canonical temporal summary list with model IDs.
#' @param spatial_summary Canonical spatial summary list with model IDs.
#' @param data_metadata Canonical dataset metadata.
#' @param model_id Identifier of the single filtered joint model.
#' @return Eligibility reconciliation, spatial balances, and temporal
#' compositions for the single model.
#' @examples
#' \dontrun{
#' prepare_human_proxy_comparison_tables(time_summary, space_summary, metadata)
#' }
prepare_human_proxy_comparison_tables <- function(
  temporal_summary,
  spatial_summary,
  data_metadata,
  model_id = "joint_filtered"
) {
  assertthat::assert_that(
    is.list(temporal_summary),
    all(c("status", "components") %in% names(temporal_summary)),
    is.list(spatial_summary),
    all(c("status", "components") %in% names(spatial_summary)),
    is.data.frame(data_metadata),
    assertthat::is.string(model_id),
    msg = "Human-proxy comparison inputs do not satisfy the contract."
  )
  temporal_status <- temporal_summary$status |>
    dplyr::filter(.data[["model_id"]] == .env[["model_id"]])
  spatial_status <- spatial_summary$status |>
    dplyr::filter(.data[["model_id"]] == .env[["model_id"]])
  temporal_ok <- c("estimated", "estimated_residual_temporal_dependence")
  spatial_ok <- c("spatial_model_estimated", "no_spatial_terms_selected")

  temporal_records <- prepare_time_controlled_importance_records(
    data_components = temporal_summary$components |>
      dplyr::filter(.data[["model_id"]] == .env[["model_id"]]),
    data_status = temporal_status,
    data_meta = data_metadata
  ) |>
    dplyr::mutate(model_id = .env[["model_id"]], .before = 1L)
  balances <- temporal_records |>
    dplyr::filter(
      .data[["status"]] %in% temporal_ok,
      is.finite(.data[["signed_difference"]]),
      .data[["signed_weight"]] > 0,
      is.finite(.data[["zero_balance"]]),
      .data[["zero_weight"]] > 0
    )

  compositions <- prepare_spatial_hvarpart_composition(
    spatial_summary$components |>
      dplyr::filter(.data[["model_id"]] == .env[["model_id"]]),
    spatial_status
  ) |>
    dplyr::mutate(model_id = .env[["model_id"]], .before = 1L) |>
    dplyr::filter(
      .data[["status"]] %in% spatial_ok,
      is.finite(.data[["allocation"]])
    )
  stack_sums <- compositions |>
    dplyr::summarise(
      allocation_sum = sum(.data[["allocation"]]),
      n_predictors = dplyr::n_distinct(.data[["predictor"]]),
      .by = c("model_id", "region", "age")
    )
  valid_units <- stack_sums |>
    dplyr::filter(
      .data[["n_predictors"]] == 3L,
      abs(.data[["allocation_sum"]] - 1) < 1e-10
    ) |>
    dplyr::select(dplyr::all_of(c("region", "age")))
  compositions <- compositions |>
    dplyr::inner_join(valid_units, by = c("region", "age"))
  final_sums <- compositions |>
    dplyr::summarise(
      allocation_sum = sum(.data[["allocation"]]),
      .by = c("model_id", "region", "age")
    )
  assertthat::assert_that(
    nrow(final_sums) == 0L ||
      all(abs(final_sums$allocation_sum - 1) < 1e-10),
    msg = "Every plotted temporal stack must sum exactly to one."
  )

  res <- list(
    temporal_common_ids = unique(balances$dataset_id),
    spatial_common_units = valid_units,
    temporal_eligibility = temporal_status |>
      dplyr::mutate(eligible = .data[["status"]] %in% temporal_ok),
    spatial_eligibility = spatial_status |>
      dplyr::mutate(eligible = .data[["status"]] %in% spatial_ok),
    balance_all_available = temporal_records,
    balance_common = balances,
    composition_common = compositions
  )

  return(res)
}
