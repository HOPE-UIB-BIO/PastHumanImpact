#' @title Summarise record loss under residual degree-of-freedom minimums
#' @description
#' Count, per model and residual degree-of-freedom minimum, how many records
#' are estimable and why the others are lost, from fitted status tables.
#' @param data_status
#' Status table with `model_column`, `status`, `residual_df`, and any
#' `group_columns`, one row per record and model.
#' @param thresholds
#' Integer vector of residual degree-of-freedom minimums to report.
#' @param fit_min_residual_df
#' The residual degree-of-freedom minimum used when the models were fitted.
#' @param group_columns
#' Optional character vector of grouping columns, for example `"region"`.
#' @param model_column
#' Name of the model identifier column.
#' @param estimable_statuses
#' Statuses that count as an estimated model.
#' @return
#' A tibble with one row per model, threshold, and group: `n_records`,
#' `n_estimable`, `n_lost`, `n_lost_insufficient_residual_df`,
#' `n_lost_insufficient_unique_ages`, `n_lost_missing_predictor`,
#' `n_lost_other`, and `n_lost_relative_to_lowest_threshold`.
#' @details
#' A record is estimable at a threshold when its status is in
#' `estimable_statuses` and `residual_df` is at least the threshold. Fitted
#' records below the threshold and records with status
#' `insufficient_residual_df` count as lost to residual degrees of freedom.
#' Missing-predictor statuses (`missing_human_predictor`,
#' `missing_climate_predictor`, `missing_predictor_group`) are grouped; all
#' other statuses count as other losses. Every threshold must be at least
#' `fit_min_residual_df`, because records skipped at fitting time cannot be
#' re-evaluated at a lower minimum.
#' @examples
#' \dontrun{
#' summarise_residual_df_record_loss(data_status = status_table)
#' }
summarise_residual_df_record_loss <- function(
  data_status,
  thresholds = c(4L, 5L),
  fit_min_residual_df = 4L,
  group_columns = character(),
  model_column = "model_id",
  estimable_statuses = c(
    "estimated", "estimated_residual_temporal_dependence"
  )
) {
  assertthat::assert_that(
    is.data.frame(data_status),
    assertthat::is.string(model_column),
    is.character(group_columns),
    all(c(model_column, "status", "residual_df", group_columns) %in%
      names(data_status)),
    is.numeric(thresholds),
    length(thresholds) > 0L,
    all(thresholds == round(thresholds)),
    assertthat::is.number(fit_min_residual_df),
    all(thresholds >= fit_min_residual_df),
    is.character(estimable_statuses),
    msg = "Record-loss inputs do not satisfy the contract."
  )

  vec_missing_statuses <-
    c(
      "missing_human_predictor",
      "missing_climate_predictor",
      "missing_predictor_group"
    )

  vec_thresholds <-
    sort(unique(as.integer(thresholds)))

  data_classified <-
    vec_thresholds |>
    purrr::map(
      .f = ~ data_status |>
        dplyr::mutate(
          min_residual_df = .x,
          flag_fitted = .data[["status"]] %in% estimable_statuses,
          loss_reason = dplyr::case_when(
            .data[["flag_fitted"]] &
              !is.na(.data[["residual_df"]]) &
              .data[["residual_df"]] >= .x ~ "estimable",
            .data[["flag_fitted"]] |
              .data[["status"]] == "insufficient_residual_df" ~
              "insufficient_residual_df",
            .data[["status"]] == "insufficient_unique_ages" ~
              "insufficient_unique_ages",
            .data[["status"]] %in% vec_missing_statuses ~
              "missing_predictor",
            .default = "other"
          )
        )
    ) |>
    dplyr::bind_rows()

  vec_group_by <-
    c(model_column, "min_residual_df", group_columns)

  data_counts <-
    data_classified |>
    dplyr::summarise(
      n_records = dplyr::n(),
      n_estimable = sum(.data[["loss_reason"]] == "estimable"),
      n_lost = sum(.data[["loss_reason"]] != "estimable"),
      n_lost_insufficient_residual_df =
        sum(.data[["loss_reason"]] == "insufficient_residual_df"),
      n_lost_insufficient_unique_ages =
        sum(.data[["loss_reason"]] == "insufficient_unique_ages"),
      n_lost_missing_predictor =
        sum(.data[["loss_reason"]] == "missing_predictor"),
      n_lost_other = sum(.data[["loss_reason"]] == "other"),
      .by = dplyr::all_of(vec_group_by)
    )

  res <-
    data_counts |>
    dplyr::mutate(
      n_lost_relative_to_lowest_threshold =
        dplyr::first(
          .data[["n_estimable"]],
          order_by = .data[["min_residual_df"]]
        ) - .data[["n_estimable"]],
      .by = dplyr::all_of(c(model_column, group_columns))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("n_"),
        .fns = as.integer
      )
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(vec_group_by))
    )

  return(res)
}
