get_roc_for_modelling <- function(data_source_roc) {
  data_source_roc %>%
    tidyr::unnest(PAP_roc) %>%
    dplyr::mutate(
      roc_error = abs(ROC_up - ROC_dw)
    ) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(
      roc_error_avg = mean(roc_error),
      var_weight = roc_error_avg / roc_error
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dataset_id,
      age = Age,
      roc = ROC,
      var_weight
    ) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age, var_weight),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value, var_weight)) %>%
    return()
}
