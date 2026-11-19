#' @title Summarise one matched comparison of time-controlled HVarPart models
#' @description
#' Match records across a cohort of models, then summarise the paired
#' balance change and the human-time commonality of a reference and a
#' comparison model on those records, labelling every table.
#' @param data_records
#' Balance records with `model_id`, as required by
#' `prepare_matched_hvarpart_balance_records()` and
#' `summarise_paired_hvarpart_balance_change()`.
#' @param data_status
#' Status table with `model_id`, `dataset_id`, `status`, `n_samples`, and
#' `residual_df`.
#' @param data_commonality
#' Output of `summarise_hvarpart_commonality_fractions()` with `model_id` and
#' `dataset_id`.
#' @param cohort_model_ids
#' Models whose records must all be estimable; must contain the reference
#' and comparison models.
#' @param reference_model_id
#' Identifier of the reference model.
#' @param comparison_model_id
#' Identifier of the comparison model.
#' @param min_residual_df
#' Minimum residual degrees of freedom.
#' @param comparison_labels
#' A one-row data frame of label columns added to the front of every
#' returned table except `records`.
#' @return
#' A list with `cohort`, `exclusions`, `records` (matched records of all
#' cohort models, unlabelled, for spatial aggregation), `pairs`,
#' `quantity_summary`, `ranking_summary`, `commonality_values`, and
#' `commonality_summary`. Commonality tables cover the reference and
#' comparison models on the matched records only.
#' @examples
#' \dontrun{
#' summarise_matched_hvarpart_model_comparison(
#'   data_records = records,
#'   data_status = status,
#'   data_commonality = commonality,
#'   cohort_model_ids = c("spd__full_climate", "spd__filtered_climate"),
#'   reference_model_id = "spd__full_climate",
#'   comparison_model_id = "spd__filtered_climate",
#'   min_residual_df = 5L
#' )
#' }
summarise_matched_hvarpart_model_comparison <- function(
  data_records,
  data_status,
  data_commonality,
  cohort_model_ids,
  reference_model_id,
  comparison_model_id,
  min_residual_df,
  comparison_labels = tibble::tibble(.rows = 1L)
) {
  assertthat::assert_that(
    is.data.frame(data_commonality),
    all(c("model_id", "dataset_id") %in% names(data_commonality)),
    is.character(cohort_model_ids),
    all(c(reference_model_id, comparison_model_id) %in% cohort_model_ids),
    is.data.frame(comparison_labels),
    nrow(comparison_labels) == 1L,
    msg = "Matched comparison inputs do not satisfy the contract."
  )

  list_matched <-
    prepare_matched_hvarpart_balance_records(
      data_records = data_records,
      data_status = data_status,
      model_ids = cohort_model_ids,
      min_residual_df = min_residual_df
    )

  list_change <-
    summarise_paired_hvarpart_balance_change(
      data_records = list_matched[["records"]],
      reference_model_id = reference_model_id,
      comparison_model_id = comparison_model_id
    )

  data_pair_keys <-
    list_matched[["records"]] |>
    dplyr::filter(
      .data[["model_id"]] %in% c(reference_model_id, comparison_model_id)
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c("model_id", "dataset_id")))
    )

  list_commonality <-
    data_commonality |>
    dplyr::semi_join(data_pair_keys, by = c("model_id", "dataset_id")) |>
    summarise_human_structure_commonality()

  data_labels <-
    comparison_labels |>
    dplyr::mutate(min_residual_df = min_residual_df)

  list_tables <-
    list(
      cohort = list_matched[["cohort"]] |>
        dplyr::select(-dplyr::all_of("min_residual_df")),
      exclusions = list_matched[["exclusions"]],
      pairs = list_change[["pairs"]],
      quantity_summary = list_change[["quantity_summary"]],
      ranking_summary = list_change[["ranking_summary"]],
      commonality_values = list_commonality[["dataset_values"]],
      commonality_summary = list_commonality[["summary"]]
    )

  list_labelled <-
    list_tables |>
    purrr::map(
      .f = ~ dplyr::bind_cols(
        data_labels[rep(1L, nrow(.x)), , drop = FALSE],
        .x
      )
    )

  res <-
    list(
      cohort = list_labelled[["cohort"]],
      exclusions = list_labelled[["exclusions"]],
      records = list_matched[["records"]],
      pairs = list_labelled[["pairs"]],
      quantity_summary = list_labelled[["quantity_summary"]],
      ranking_summary = list_labelled[["ranking_summary"]],
      commonality_values = list_labelled[["commonality_values"]],
      commonality_summary = list_labelled[["commonality_summary"]]
    )

  return(res)
}
