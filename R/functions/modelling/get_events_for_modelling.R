get_events_for_modelling <- function(data_source_events) {
  data_source_events %>%
    tidyr::unnest(events_updated) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value)) %>%
    return()
}
