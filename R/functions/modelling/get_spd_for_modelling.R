get_spd_for_modelling <- function(data_source_spd) {
  data_source_spd %>%
    tidyr::unnest(
      spd
    ) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = "var_name",
      values_to = "var"
    ) %>%
    tidyr::drop_na(var) %>%
    dplyr::mutate(
      var = round(var, 3)
    ) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, var)) %>%
    return()
}
