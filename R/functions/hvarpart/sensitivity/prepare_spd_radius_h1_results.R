#' @title Bind one table from SPD-radius H1 branch results
#' @description Extract and row-bind a named result table from every radius.
#' @param data_results List of branch result lists.
#' @param result_name Name of the table to extract.
#' @return Combined data frame ordered by radius when available.
#' @examples
#' \dontrun{
#' prepare_spd_radius_h1_results(results, "time_control_status")
#' }
prepare_spd_radius_h1_results <- function(data_results, result_name) {
  assertthat::assert_that(
    is.list(data_results),
    length(data_results) > 0L,
    assertthat::is.string(result_name),
    all(purrr::map_lgl(data_results, is.list)),
    all(purrr::map_lgl(
      data_results,
      ~ result_name %in% names(.x)
    )),
    msg = "SPD radius branch results do not satisfy the contract."
  )

  res <-
    data_results |>
    purrr::map(result_name) |>
    dplyr::bind_rows()

  if ("radius_km" %in% names(res)) {
    res <- dplyr::arrange(res, .data[["radius_km"]])
  }

  return(res)
}
