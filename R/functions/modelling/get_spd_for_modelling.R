get_spd_for_modelling <- function(data_source_spd) {
  data_source_spd %>%
    tidyr::unnest(
      spd
    ) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(
      value = round(value, 3)
    ) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value)) %>%
    return()
}
