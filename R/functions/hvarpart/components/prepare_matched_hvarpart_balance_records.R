#' @title Prepare balance records matched across HVarPart models
#' @description
#' Restrict time-controlled human-climate balance records to the records that
#' are estimable, meet a residual degree-of-freedom minimum, have a valid
#' balance, and use the same number of samples in every listed model, so
#' that the models can be compared on identical records.
#' @param data_records
#' Balance records with `model_id`, the `id_column`, `signed_difference`,
#' `signed_weight`, `zero_balance`, and `zero_weight`, such as the output of
#' `prepare_time_controlled_importance_records()` with a `model_id` column.
#' @param data_status
#' Status table with `model_id`, the `id_column`, `status`, `n_samples`, and
#' `residual_df`.
#' @param model_ids
#' Character vector of at least two model identifiers to match.
#' @param min_residual_df
#' Minimum residual degrees of freedom.
#' @param id_column
#' Name of the record identifier column.
#' @param estimable_statuses
#' Statuses that count as an estimated model.
#' @return
#' A list with `records` (the balance records of `model_ids` for matched
#' records, with `n_samples` and `residual_df` added), `exclusions` (one row
#' per excluded record and failing model: the `id_column`, `model_id`, and
#' `reason`), and `cohort` (one row: `min_residual_df`, `n_candidates`,
#' `n_matched`, `n_excluded`).
#' @details
#' Candidates are all records listed in `data_status` for `model_ids`. Per
#' model, the reasons are checked in order: `not_estimable`,
#' `below_min_residual_df`, `invalid_balance` (no record, a non-finite
#' difference or balance, or a non-positive weight). A record that passes in
#' every model but whose `n_samples` differ is excluded once with reason
#' `unequal_n_samples` and `model_id = NA`.
#' @examples
#' \dontrun{
#' prepare_matched_hvarpart_balance_records(
#'   data_records = records,
#'   data_status = status,
#'   model_ids = c("spd__full_climate", "spd__filtered_climate"),
#'   min_residual_df = 5L
#' )
#' }
prepare_matched_hvarpart_balance_records <- function(
  data_records,
  data_status,
  model_ids,
  min_residual_df,
  id_column = "dataset_id",
  estimable_statuses = c(
    "estimated", "estimated_residual_temporal_dependence"
  )
) {
  vec_balance_columns <-
    c("signed_difference", "signed_weight", "zero_balance", "zero_weight")

  assertthat::assert_that(
    is.data.frame(data_records),
    is.data.frame(data_status),
    assertthat::is.string(id_column),
    all(c("model_id", id_column, vec_balance_columns) %in%
      names(data_records)),
    all(c("model_id", id_column, "status", "n_samples", "residual_df") %in%
      names(data_status)),
    is.character(model_ids),
    length(model_ids) >= 2L,
    !anyDuplicated(model_ids),
    all(model_ids %in% data_status[["model_id"]]),
    assertthat::is.number(min_residual_df),
    is.character(estimable_statuses),
    msg = "Matched balance-record inputs do not satisfy the contract."
  )

  data_status_models <-
    data_status |>
    dplyr::filter(.data[["model_id"]] %in% model_ids) |>
    dplyr::select(
      dplyr::all_of(c(
        "model_id", id_column, "status", "n_samples", "residual_df"
      ))
    )

  assertthat::assert_that(
    !anyDuplicated(data_status_models[c("model_id", id_column)]),
    msg = "Each record may appear only once per model in `data_status`."
  )

  data_balance_valid <-
    data_records |>
    dplyr::filter(.data[["model_id"]] %in% model_ids) |>
    dplyr::mutate(
      flag_valid_balance =
        is.finite(.data[["signed_difference"]]) &
          is.finite(.data[["signed_weight"]]) &
          .data[["signed_weight"]] > 0 &
          is.finite(.data[["zero_balance"]]) &
          is.finite(.data[["zero_weight"]]) &
          .data[["zero_weight"]] > 0
    ) |>
    dplyr::select(dplyr::all_of(c("model_id", id_column, "flag_valid_balance")))

  data_candidates <-
    data_status_models |>
    dplyr::distinct(dplyr::across(dplyr::all_of(id_column)))

  data_eligibility <-
    tidyr::expand_grid(
      model_id = model_ids,
      data_candidates
    ) |>
    dplyr::left_join(data_status_models, by = c("model_id", id_column)) |>
    dplyr::left_join(data_balance_valid, by = c("model_id", id_column)) |>
    dplyr::mutate(
      reason = dplyr::case_when(
        !(.data[["status"]] %in% estimable_statuses) ~ "not_estimable",
        is.na(.data[["residual_df"]]) |
          .data[["residual_df"]] < min_residual_df ~ "below_min_residual_df",
        !dplyr::coalesce(.data[["flag_valid_balance"]], FALSE) ~
          "invalid_balance",
        .default = "eligible"
      )
    )

  data_per_record <-
    data_eligibility |>
    dplyr::summarise(
      flag_all_eligible = all(.data[["reason"]] == "eligible"),
      n_sample_sizes = dplyr::n_distinct(.data[["n_samples"]]),
      .by = dplyr::all_of(id_column)
    )

  vec_matched_ids <-
    data_per_record |>
    dplyr::filter(
      .data[["flag_all_eligible"]],
      .data[["n_sample_sizes"]] == 1L
    ) |>
    dplyr::pull(dplyr::all_of(id_column))

  data_exclusions_model <-
    data_eligibility |>
    dplyr::filter(.data[["reason"]] != "eligible") |>
    dplyr::select(dplyr::all_of(c(id_column, "model_id", "reason")))

  data_exclusions_samples <-
    data_per_record |>
    dplyr::filter(
      .data[["flag_all_eligible"]],
      .data[["n_sample_sizes"]] > 1L
    ) |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(id_column)),
      model_id = NA_character_,
      reason = "unequal_n_samples"
    )

  data_exclusions <-
    dplyr::bind_rows(data_exclusions_model, data_exclusions_samples)

  data_matched <-
    data_records |>
    dplyr::filter(
      .data[["model_id"]] %in% model_ids,
      .data[[id_column]] %in% vec_matched_ids
    ) |>
    dplyr::left_join(
      data_status_models |>
        dplyr::select(
          dplyr::all_of(c("model_id", id_column, "n_samples", "residual_df"))
        ),
      by = c("model_id", id_column)
    )

  data_cohort <-
    tibble::tibble(
      min_residual_df = min_residual_df,
      n_candidates = nrow(data_candidates),
      n_matched = length(vec_matched_ids),
      n_excluded = nrow(data_candidates) - length(vec_matched_ids)
    )

  res <-
    list(
      records = data_matched,
      exclusions = data_exclusions,
      cohort = data_cohort
    )

  return(res)
}
