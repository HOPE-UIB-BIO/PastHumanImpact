#' @title Prepare paired sequence R-squared and selection evidence
#' @description Combine matched adjusted R-squared comparisons with local
#' predictor-retention summaries by continental region and region.
#' @param canonical_balance Canonical SPD dataset-level balance table.
#' @param canonical_components Canonical long component-profile table.
#' @param filtered_balance Filtered joint-human dataset-level balance table.
#' @param filtered_unique_r2 Filtered joint-human unique adjusted R-squared.
#' @param predictor_selection Long local predictor-selection audit table.
#' @return A named list containing paired sequence values, plotting values,
#' adjusted R-squared summaries, and predictor-selection summaries.
#' @examples
#' \dontrun{
#' prepare_sequence_r2_predictor_overview(a, b, c, d, e)
#' }
prepare_sequence_r2_predictor_overview <- function(
  canonical_balance,
  canonical_components,
  filtered_balance,
  filtered_unique_r2,
  predictor_selection
) {
  assertthat::assert_that(
    is.data.frame(canonical_balance),
    is.data.frame(canonical_components),
    is.data.frame(filtered_balance),
    is.data.frame(filtered_unique_r2),
    is.data.frame(predictor_selection),
    msg = "Sequence overview inputs do not satisfy the contract."
  )
  r2 <- prepare_sequence_r2_comparison(
    canonical_balance = canonical_balance,
    canonical_components = canonical_components,
    filtered_balance = filtered_balance,
    filtered_unique_r2 = filtered_unique_r2
  )
  selection <- prepare_sequence_predictor_selection_overview(
    filtered_balance = filtered_balance,
    predictor_selection = predictor_selection
  )
  sequence_values <- r2[["sequence_values"]] |>
    dplyr::left_join(
      selection[["selection_by_dataset"]] |>
        dplyr::select(-dplyr::any_of(c(
          "continental_region", "region", "long", "lat"
        ))),
      by = "dataset_id"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("n_selected_"),
        ~ tidyr::replace_na(.x, 0L)
      )
    )
  res <- list(
    sequence_values = sequence_values,
    comparison_long = r2[["comparison_long"]],
    r2_summary = r2[["r2_summary"]],
    selection_by_dataset = selection[["selection_by_dataset"]],
    selection_frequency_by_continental_region =
      selection[["selection_frequency_by_continental_region"]],
    selection_frequency_by_region =
      selection[["selection_frequency_by_region"]],
    selection_sets_by_continental_region =
      selection[["selection_sets_by_continental_region"]],
    selection_sets_by_region = selection[["selection_sets_by_region"]]
  )

  return(res)
}
