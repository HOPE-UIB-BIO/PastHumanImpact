#' @title Select authorized temporal-model runs
#' @description
#' Apply the complete two-key fitting gate and report accepted and rejected
#' temporal-model run requests.
#' @param data_config Temporal-model lifecycle configuration.
#' @param data_requests Validated temporal-model request ledger.
#' @param data_history Model run-event history containing optional request IDs.
#' @return List with `authorized` and `audit` tibbles.
#' @examples
#' \dontrun{
#' selection <- select_authorized_temporal_model_runs(
#'   data_config = config,
#'   data_requests = requests,
#'   data_history = history
#' )
#' }
select_authorized_temporal_model_runs <- function(
  data_config,
  data_requests,
  data_history = tibble::tibble()
) {
  required_config_columns <-
    c(
      "model_id",
      "definition_hash",
      "is_active_model",
      "is_model_eligible",
      "need_to_run"
    )

  assertthat::assert_that(
    is.data.frame(data_config),
    all(required_config_columns %in% names(data_config)),
    is.data.frame(data_history),
    msg = "Temporal-model lifecycle inputs are invalid."
  )

  validate_temporal_model_run_requests(data_requests = data_requests)

  consumed_request_ids <-
    if (
      all(c("request_id", "event") %in% names(data_history))
    ) {
      data_history |>
      dplyr::filter(
        .data[["event"]] == "fit_started",
        !is.na(.data[["request_id"]]),
        nzchar(.data[["request_id"]])
      ) |>
      dplyr::pull(.data[["request_id"]]) |>
      unique()
    } else {
      character()
    }

  data_audit <-
    data_requests |>
    dplyr::left_join(
      data_config |>
        dplyr::select(dplyr::all_of(required_config_columns)) |>
        dplyr::rename(
          configured_definition_hash = definition_hash
        ),
      by = "model_id"
    ) |>
    dplyr::mutate(
      request_status = dplyr::case_when(
        !.data[["run_requested"]] ~ "not_requested",
        is.na(.data[["configured_definition_hash"]]) ~ "unknown_model",
        .data[["request_id"]] %in% consumed_request_ids ~ "consumed",
        .data[["definition_hash"]] !=
          .data[["configured_definition_hash"]] ~ "stale_definition",
        !.data[["is_active_model"]] ~ "inactive_model",
        !.data[["is_model_eligible"]] ~ "ineligible_model",
        !.data[["need_to_run"]] ~ "fit_not_needed",
        .default = "authorized"
      )
    )

  data_authorized <-
    data_audit |>
    dplyr::filter(.data[["request_status"]] == "authorized")

  res_selection <-
    list(
      authorized = data_authorized,
      audit = data_audit
    )

  return(res_selection)
}
