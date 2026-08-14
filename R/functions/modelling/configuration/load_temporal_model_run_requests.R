#' @title Load temporal-model run requests
#' @description
#' Load the append-only temporal-model request ledger, returning a typed empty
#' ledger when it does not yet exist.
#' @param path Character scalar request-ledger CSV path.
#' @return Tibble containing validated temporal-model run requests.
#' @examples
#' \dontrun{
#' requests <- load_temporal_model_run_requests(
#'   path = "Data/Temporal_models/general_model_run_requests.csv"
#' )
#' }
load_temporal_model_run_requests <- function(path) {
  assertthat::assert_that(
    is.character(path),
    length(path) == 1L,
    !is.na(path),
    nzchar(path),
    msg = "`path` must be a non-empty character scalar."
  )

  res_requests <-
    if (
      file.exists(path)
    ) {
      readr::read_csv(
        file = path,
        show_col_types = FALSE,
        col_types = readr::cols(
          request_id = readr::col_character(),
          model_id = readr::col_character(),
          definition_hash = readr::col_character(),
          run_requested = readr::col_logical(),
          request_reason = readr::col_character(),
          requested_at = readr::col_character()
        )
      )
    } else {
      tibble::tibble(
        request_id = character(),
        model_id = character(),
        definition_hash = character(),
        run_requested = logical(),
        request_reason = character(),
        requested_at = character()
      )
    }

  validate_temporal_model_run_requests(data_requests = res_requests)

  return(res_requests)
}
