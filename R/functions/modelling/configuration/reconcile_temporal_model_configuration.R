#' @title Reconcile temporal-model lifecycle configuration
#' @description
#' Preserve lifecycle state for unchanged model definitions, mark new or
#' changed definitions as requiring a fit, and deactivate removed definitions.
#' @param data_current Existing lifecycle configuration.
#' @param data_candidate Configuration generated from current inputs.
#' @return Reconciled temporal-model configuration tibble.
#' @examples
#' \dontrun{
#' reconciled <- reconcile_temporal_model_configuration(
#'   data_current = current,
#'   data_candidate = candidate
#' )
#' }
reconcile_temporal_model_configuration <- function(
  data_current,
  data_candidate
) {
  required_columns <-
    c(
      "model_id",
      "specification_hash",
      "need_to_run",
      "need_to_be_evaluated",
      "prediction_written"
    )

  assertthat::assert_that(
    is.data.frame(data_current),
    is.data.frame(data_candidate),
    all(required_columns %in% names(data_current)),
    all(required_columns %in% names(data_candidate)),
    !anyDuplicated(data_current[["model_id"]]),
    !anyDuplicated(data_candidate[["model_id"]]),
    msg = "Temporal-model configuration inputs are invalid."
  )

  data_current_cast <-
    cast_temporal_model_configuration(
      data_current = data_current,
      data_candidate = data_candidate
    )

  unchanged_ids <-
    data_candidate |>
    dplyr::select(
      dplyr::all_of(c("model_id", "specification_hash"))
    ) |>
    dplyr::rename(
      candidate_hash = "specification_hash"
    ) |>
    dplyr::inner_join(
      data_current_cast |>
        dplyr::select(
          dplyr::all_of(c("model_id", "specification_hash"))
        ) |>
        dplyr::rename(
          current_hash = "specification_hash"
        ),
      by = "model_id"
    ) |>
    dplyr::filter(.data[["candidate_hash"]] == .data[["current_hash"]]) |>
    dplyr::pull(.data[["model_id"]])

  data_preserved <-
    data_current_cast |>
    dplyr::filter(.data[["model_id"]] %in% unchanged_ids) |>
    dplyr::select(dplyr::all_of(names(data_candidate))) |>
    dplyr::mutate(is_active_model = TRUE)

  data_active <-
    data_candidate |>
    dplyr::mutate(is_active_model = TRUE) |>
    dplyr::rows_update(
      data_preserved,
      by = "model_id",
      unmatched = "ignore"
    )

  data_removed <-
    data_current_cast |>
    dplyr::filter(
      !.data[["model_id"]] %in% data_candidate[["model_id"]]
    ) |>
    dplyr::mutate(
      is_active_model = FALSE,
      need_to_run = FALSE,
      need_to_be_evaluated = FALSE
    ) |>
    dplyr::select(dplyr::all_of(names(data_active)))

  res_config <-
    dplyr::bind_rows(data_active, data_removed) |>
    dplyr::arrange(.data[["model_id"]])

  return(res_config)
}
