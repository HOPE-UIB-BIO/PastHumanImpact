#' @title Prepare one spatial human-proxy model balance
#' @description Standardise one model balance table on an exact common dataset
#' cohort for decision-evidence assembly.
#' @param data_source Dataset-level balance table.
#' @param common_ids One-column common-cohort dataset table.
#' @param model_id Stable model identifier.
#' @param model_label Human-readable model label.
#' @return A standardised dataset-level spatial balance table.
#' @examples
#' \dontrun{
#' prepare_human_proxy_spatial_model_balance(x, ids, "model", "Model")
#' }
prepare_human_proxy_spatial_model_balance <- function(
  data_source,
  common_ids,
  model_id,
  model_label
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    is.data.frame(common_ids),
    "dataset_id" %in% names(common_ids),
    assertthat::is.string(model_id),
    assertthat::is.string(model_label),
    msg = "Spatial model-balance inputs do not satisfy the contract."
  )
  res <- data_source |>
    dplyr::semi_join(common_ids, by = "dataset_id") |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      model_id = .env[["model_id"]],
      model_label = .env[["model_label"]],
      total_adjusted_r_squared = .data[["total_adjusted_r_squared"]],
      human_contribution = .data[["human"]],
      climate_contribution = .data[["climate"]],
      control_contribution = .data[["time"]],
      signed_difference = .data[["signed_difference"]],
      signed_balance = .data[["signed_balance"]],
      signed_weight = .data[["signed_weight"]],
      zero_balance = .data[["zero_balance"]],
      zero_weight = .data[["zero_weight"]]
    )

  return(res)
}
