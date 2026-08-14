#' @title Initialize temporal-model run requests
#' @description
#' Create a header-only temporal-model request ledger when no ledger exists.
#' This function never creates an affirmative fitting request.
#' @param path Character scalar request-ledger CSV path.
#' @return Invisibly returns the normalized request-ledger path.
#' @examples
#' \dontrun{
#' initialize_temporal_model_run_requests(
#'   path = "Data/Temporal_models/general_model_run_requests.csv"
#' )
#' }
initialize_temporal_model_run_requests <- function(path) {
  assertthat::assert_that(
    is.character(path),
    length(path) == 1L,
    !is.na(path),
    nzchar(path),
    msg = "`path` must be a non-empty character scalar."
  )

  if (
    !file.exists(path)
  ) {
    dir.create(
      path = dirname(path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    data_empty <-
      load_temporal_model_run_requests(path = path)

    readr::write_csv(
      x = data_empty,
      file = path
    )
  }

  validate_temporal_model_run_requests(
    data_requests = load_temporal_model_run_requests(path = path)
  )

  res_path <-
    normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    )

  return(invisible(res_path))
}
