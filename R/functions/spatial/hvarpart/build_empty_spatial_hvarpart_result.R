#' @title Build an empty spatial HVarPart result
#' @description Construct the stable result schema for a failed or
#' non-estimable region-age spatial analysis.
#' @param status Analysis-level status.
#' @param n_samples Number of available samples.
#' @param selection_status dbMEM-selection status.
#' @return An empty spatial HVarPart result list.
#' @examples
#' \dontrun{
#' build_empty_spatial_hvarpart_result("model_error", 0L)
#' }
build_empty_spatial_hvarpart_result <- function(
  status,
  n_samples,
  selection_status = status
) {
  assertthat::assert_that(
    assertthat::is.string(status),
    is.numeric(n_samples),
    length(n_samples) == 1L,
    assertthat::is.string(selection_status),
    msg = "Empty spatial HVarPart result inputs are invalid."
  )

  n_complete <-
    if (
      is.finite(n_samples)
    ) {
      max(as.integer(n_samples), 0L)
    } else {
      0L
    }
  res_result <-
    list(
      status = status,
      n_samples = as.integer(n_samples),
      human_climate_only_hvarpart = NULL,
      spatial_hvarpart = NULL,
      dbmem = list(diagnostics = tibble::tibble()),
      selection = build_empty_dbmem_selection(
        status_value = selection_status,
        n_complete = n_complete,
        n_candidates = 0L
      ),
      unique_adjusted_r2 = tibble::tibble(),
      residual_moran = tibble::tibble(),
      remaining_spatial_test = tibble::tibble()
    )

  return(res_result)
}
