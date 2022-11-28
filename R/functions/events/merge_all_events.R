merge_all_events <- function(...) {
  dplyr::bind_rows(
    ...
  ) %>%
    return()
}
