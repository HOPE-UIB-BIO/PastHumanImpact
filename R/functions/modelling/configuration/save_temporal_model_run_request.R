#' @title Save a temporal-model run request
#' @description
#' Append one explicit, definition-bound fitting request to the external
#' temporal-model request ledger.
#' @param path Character scalar request-ledger CSV path.
#' @param model_id Character scalar configured model identifier.
#' @param definition_hash Character scalar current model-definition hash.
#' @param request_reason Character scalar explanation for the fitting request.
#' @param request_id Optional unique request identifier.
#' @param requested_at Optional request timestamp.
#' @return Invisibly returns the updated request ledger.
#' @examples
#' \dontrun{
#' save_temporal_model_run_request(
#'   path = "Data/Temporal_models/general_model_run_requests.csv",
#'   model_id = "pap_temporal__n0__Europe__Temperate",
#'   definition_hash = "current-hash",
#'   request_reason = "Approved model-definition update"
#' )
#' }
save_temporal_model_run_request <- function(
  path,
  model_id,
  definition_hash,
  request_reason,
  request_id = stringr::str_c(
    model_id,
    format(Sys.time(), "%Y%m%dT%H%M%S"),
    sep = "__"
  ),
  requested_at = as.character(Sys.time())
) {
  assertthat::assert_that(
    is.character(model_id),
    length(model_id) == 1L,
    !is.na(model_id),
    nzchar(model_id),
    is.character(definition_hash),
    length(definition_hash) == 1L,
    !is.na(definition_hash),
    nzchar(definition_hash),
    is.character(request_reason),
    length(request_reason) == 1L,
    !is.na(request_reason),
    nzchar(request_reason),
    is.character(request_id),
    length(request_id) == 1L,
    !is.na(request_id),
    nzchar(request_id),
    msg = "Temporal-model request values must be non-empty scalars."
  )

  data_requests <-
    load_temporal_model_run_requests(path = path)

  assertthat::assert_that(
    !request_id %in% data_requests[["request_id"]],
    msg = "`request_id` must be unique in the request ledger."
  )

  data_request <-
    tibble::tibble(
      request_id = request_id,
      model_id = model_id,
      definition_hash = definition_hash,
      run_requested = TRUE,
      request_reason = request_reason,
      requested_at = requested_at
    )

  res_requests <-
    dplyr::bind_rows(data_requests, data_request)

  validate_temporal_model_run_requests(data_requests = res_requests)

  dir.create(
    path = dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    x = res_requests,
    file = path
  )

  return(invisible(res_requests))
}
