get_diversity_and_dcca_for_modelling <- function(data_source_diversity,
                                                 data_source_dcca,
                                                 data_source_pollen) {
  data_ages <-
    data_source_pollen %>%
    tidyr::unnest(levels) %>%
    dplyr::mutate(
      age_error = abs(lower - upper)
    ) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(
      age_error_avg = mean(age_error),
      var_weight = age_error_avg / age_error
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dataset_id, sample_id, age, var_weight)

  data_diversity <-
    data_source_diversity %>%
    tidyr::unnest(PAP_diversity)

  data_dcca <-
    data_source_dcca %>%
    tidyr::unnest(dcca_scores) %>%
    dplyr::select(dataset_id, sample_id, dcca_axis_1 = axis_1)

  # wragler data so that there are grouped by col
  data_to_fit <-
    dplyr::full_join(
      data_diversity,
      data_dcca,
      by = c("dataset_id", "sample_id")
    ) %>%
    dplyr::inner_join(
      data_ages,
      by = c("dataset_id", "sample_id")
    ) %>%
    dplyr::select(-sample_id) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age, var_weight),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value, var_weight))

  return(data_to_fit)
}
