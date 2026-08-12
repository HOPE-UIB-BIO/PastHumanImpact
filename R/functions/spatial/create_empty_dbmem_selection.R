#' @title Create an empty dbMEM selection result
#' @description
#' Construct the common result schema for a dbMEM selection that cannot
#' proceed or selects no candidate terms.
#' @param status_value Character status label.
#' @param n_complete Number of complete observations.
#' @param n_candidates Number of candidate dbMEM predictors.
#' @return A dbMEM selection result list with no selected terms.
#' @examples
#' \dontrun{
#' create_empty_dbmem_selection(
#'   status_value = "no_spatial_signal",
#'   n_complete = 40L,
#'   n_candidates = 5L
#' )
#' }
create_empty_dbmem_selection <- function(
  status_value,
  n_complete,
  n_candidates
) {
  assertthat::assert_that(
    assertthat::is.string(status_value),
    is.numeric(n_complete),
    length(n_complete) == 1L,
    is.finite(n_complete),
    n_complete >= 0L,
    is.numeric(n_candidates),
    length(n_candidates) == 1L,
    is.finite(n_candidates),
    n_candidates >= 0L,
    msg = "Empty dbMEM selection inputs do not satisfy the contract."
  )

  res_selection <-
    list(
      status = status_value,
      n_complete = as.integer(n_complete),
      n_candidates = as.integer(n_candidates),
      global_p_value = NA_real_,
      full_adjusted_r_squared = NA_real_,
      selected_names = character(),
      selection_table = tibble::tibble()
    )

  return(res_selection)
}
