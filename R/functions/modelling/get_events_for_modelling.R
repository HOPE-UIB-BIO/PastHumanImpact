#' @title Prepare event data for modelling
#' @description
#' Unnest updated event tables, pivot to long format, and nest modelling data by
#' event variable.
#' @param data_source_events Data frame with list-column `events_updated`.
#' @return Data frame with `var_name` and nested `data_to_fit` tables.
get_events_for_modelling <- function(data_source_events) {
  assertthat::assert_that(
    is.data.frame(data_source_events),
    msg = "`data_source_events` must be a data frame."
  )

  assertthat::assert_that(
    "events_updated" %in% names(data_source_events),
    msg = "`data_source_events` must contain `events_updated`."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(data_source_events$events_updated, is.data.frame)),
    msg = "`events_updated` entries must be data frames."
  )

  res_data <-
    data_source_events %>%
    tidyr::unnest(events_updated) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value))

  return(res_data)
}
