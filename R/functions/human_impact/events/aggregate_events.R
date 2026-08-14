#' @title Aggregate events from all sources
#' @description Row-bind event tables from multiple sources.
#' @param ... Event data frames to merge.
#' @return A data frame containing all rows from provided event tables.
aggregate_events <- function(...) {
  data_sources <-
    list(...)

  if (length(data_sources) > 0) {
    assertthat::assert_that(
      all(purrr::map_lgl(data_sources, is.data.frame)),
      msg = "All inputs to `aggregate_events()` must be data frames."
    )
  }

  res_data <-
    dplyr::bind_rows(
      ...
    )

  return(res_data)
}
