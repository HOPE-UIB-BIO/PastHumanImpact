#' @title Validate temporal-model run requests
#' @description
#' Validate the append-only ledger that explicitly authorizes temporal-model
#' fitting attempts.
#' @param data_requests Data frame containing temporal-model run requests.
#' @return Invisibly returns `data_requests` after validation.
#' @examples
#' \dontrun{
#' validate_temporal_model_run_requests(data_requests = requests)
#' }
validate_temporal_model_run_requests <- function(data_requests) {
  required_columns <-
    c(
      "request_id",
      "model_id",
      "definition_hash",
      "run_requested",
      "request_reason",
      "requested_at"
    )

  assertthat::assert_that(
    is.data.frame(data_requests),
    all(required_columns %in% names(data_requests)),
    msg = "Temporal-model requests are missing required columns."
  )

  if (
    nrow(data_requests) == 0L
  ) {
    return(invisible(data_requests))
  }

  assertthat::assert_that(
    !anyDuplicated(data_requests[["request_id"]]),
    all(!is.na(data_requests[["request_id"]])),
    all(nzchar(data_requests[["request_id"]])),
    all(!is.na(data_requests[["model_id"]])),
    all(nzchar(data_requests[["model_id"]])),
    all(!is.na(data_requests[["definition_hash"]])),
    all(nzchar(data_requests[["definition_hash"]])),
    msg = "Temporal-model request identifiers must be unique and non-empty."
  )

  assertthat::assert_that(
    is.logical(data_requests[["run_requested"]]),
    all(!is.na(data_requests[["run_requested"]])),
    msg = "`run_requested` must contain non-missing logical values."
  )

  return(invisible(data_requests))
}
