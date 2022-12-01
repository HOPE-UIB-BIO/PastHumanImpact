get_per_timeslice_all_col <- function(data_source,
                                      col_to_unnest,
                                      sel_name,
                                      smooth_basis,
                                      max_k = 24,
                                      error_family,
                                      data_source_dummy_time) {
  # wragler data so that there are grouped by col
  data_to_fit <-
    data_source %>%
    tidyr::unnest(dplyr::all_of(col_to_unnest)) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age),
      names_to = sel_name,
      values_to = "var"
    ) %>%
    tidyr::drop_na(var) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, var))

  # fit GAM for each dataset of reach type
  data_gams <-
    data_to_fit$data_to_fit %>%
    purrr::set_names(
      nm = data_to_fit %>%
        purrr::pluck(sel_name)
    ) %>%
    purrr::map_dfr(
      .x = .,
      .id = sel_name,
      .f = ~ REcopol:::fit_multiple_gams(
        data_source = .x,
        x_var = "age",
        y_var = "var",
        smooth_basis = smooth_basis,
        max_k = max_k,
        error_family = error_family,
        verbose = TRUE,
        max_iterations = 200
      )
    )

  # predict each GAM using `data_source_dummy_time`
  data_predicted <-
    data_gams %>%
    dplyr::mutate(
      pred_data = purrr::map(
        .x = mod,
        .f = ~ REcopol::predic_model(
          data_source = data_source_dummy_time,
          model_source = .x
        )
      )
    ) %>%
    tidyr::unnest(pred_data) %>%
    dplyr::select(
      dataset_id,
      age,
      dplyr::all_of(sel_name),
      var = fit
    ) %>%
    dplyr::mutate(
      var = round(var, 3)
    ) %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(sel_name),
      values_from = "var"
    ) %>%
    tidyr::nest(!!col_to_unnest := -c(dataset_id))

  res <-
    data_predicted %>%
    dplyr::select(dataset_id, dplyr::all_of(col_to_unnest))

  return(res)
}
