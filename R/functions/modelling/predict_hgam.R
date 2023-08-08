predict_hgam <- function(data_model,
                         var_name,
                         data_dummy,
                         time_step = 500) {
  sel_data <-
    insight::get_data(data_model) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      dataset_id = as.factor(dataset_id)
    )

  sel_data_age_range <-
    sel_data %>%
    purrr::pluck("age") %>%
    range()

  dummy_to_predict <-
    data_dummy %>%
    dplyr::mutate(
      dataset_id = sel_data$dataset_id[1]
    ) %>%
    dplyr::filter(
      age >= min(sel_data_age_range) - time_step &
        age <= max(sel_data_age_range) + time_step
    )

  # Predict the models
  data_pred <-
    REcopol::predic_model(
      model_source = data_model,
      data_source = dummy_to_predict,
      exclude_var = data_model %>%
        gratia::smooths() %>%
        stringr::str_subset(., "dataset_id")
    ) %>%
    dplyr::rename(
      !!var_name := fit
    ) %>%
    dplyr::select(-dataset_id)

  return(data_pred)
}
