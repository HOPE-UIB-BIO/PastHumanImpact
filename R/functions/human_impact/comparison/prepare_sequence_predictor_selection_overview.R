#' @title Prepare sequence predictor-selection evidence
#' @description Attach geography to local selection records and summarise
#' retained predictors and predictor sets by continental region and region.
#' @param filtered_balance Filtered joint-human dataset-level balance table.
#' @param predictor_selection Long local predictor-selection audit table.
#' @return A named list of dataset selections and geographic summaries.
#' @examples
#' \dontrun{
#' prepare_sequence_predictor_selection_overview(balance, selection)
#' }
prepare_sequence_predictor_selection_overview <- function(
  filtered_balance,
  predictor_selection
) {
  assertthat::assert_that(
    is.data.frame(filtered_balance),
    is.data.frame(predictor_selection),
    all(c(
      "dataset_id", "region", "climatezone", "long", "lat"
    ) %in% names(filtered_balance)),
    all(c(
      "dataset_id", "group", "predictor", "preference_rank", "selected",
      "reason"
    ) %in% names(predictor_selection)),
    msg = "Sequence predictor-selection inputs do not satisfy the contract."
  )
  duplicate_selection <- predictor_selection |>
    dplyr::count(
      .data[["dataset_id"]], .data[["group"]], .data[["predictor"]]
    ) |>
    dplyr::filter(.data[["n"]] > 1L)
  if (nrow(duplicate_selection) > 0L) {
    cli::cli_abort(
      "Predictor selection must contain one row per dataset, group, and predictor."
    )
  }
  selected_by_dataset <- predictor_selection |>
    dplyr::filter(.data[["selected"]]) |>
    dplyr::arrange(
      .data[["dataset_id"]],
      .data[["group"]],
      .data[["preference_rank"]]
    ) |>
    dplyr::summarise(
      selected_predictors = paste(.data[["predictor"]], collapse = " + "),
      n_selected = dplyr::n(),
      .by = c("dataset_id", "group")
    ) |>
    tidyr::pivot_wider(
      names_from = "group",
      values_from = c("selected_predictors", "n_selected"),
      names_glue = "{.value}_{group}"
    )
  eligible_metadata <- filtered_balance |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      continental_region = .data[["region"]],
      region = .data[["climatezone"]],
      long = .data[["long"]],
      lat = .data[["lat"]]
    )
  selection_records <- predictor_selection |>
    dplyr::inner_join(eligible_metadata, by = "dataset_id")
  selection_by_dataset <- eligible_metadata |>
    dplyr::left_join(selected_by_dataset, by = "dataset_id") |>
    dplyr::arrange(.data[["continental_region"]], .data[["dataset_id"]])
  res <- list(
    selection_by_dataset = selection_by_dataset,
    selection_frequency_by_continental_region =
      summarise_predictor_selection_frequency(
        selection_records = selection_records,
        grouping_variable = "continental_region"
      ),
    selection_frequency_by_region = summarise_predictor_selection_frequency(
      selection_records = selection_records,
      grouping_variable = "region"
    ),
    selection_sets_by_continental_region = summarise_predictor_selection_sets(
      selection_by_dataset = selection_by_dataset,
      grouping_variable = "continental_region"
    ),
    selection_sets_by_region = summarise_predictor_selection_sets(
      selection_by_dataset = selection_by_dataset,
      grouping_variable = "region"
    )
  )

  return(res)
}
