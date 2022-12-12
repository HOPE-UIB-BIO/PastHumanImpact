get_hgam_density_vars <- function(data_source_density,
                                  data_source_meta = NULL,
                                  data_error_family = "mgcv::betar(link = 'logit')",
                                  data_source_dummy_time,
                                  smooth_basis = c("tp", "cr"),
                                  group_var = "dataset_id",
                                  weights_var = NULL,
                                  sel_k = 10,
                                  sel_m = NULL,
                                  common_trend = TRUE,
                                  use_parallel = TRUE,
                                  use_discrete = FALSE,
                                  max_iterations = 200,
                                  limit_length = TRUE
                                  ) {
  
  
  if (
    is.data.frame(data_error_family)
  ) {
    assertthat::assert_that(
      all(data_source_density$var_name %in% data_error_family$var_name) &&
        all(data_error_family$var_name %in% data_source_density$var_name),
      msg = paste(
        "'data_source' and 'data_error_family' must have",
        "same values is `var_name`"
      )
    )
    
    data_with_error <-
      dplyr::inner_join(
        data_source,
        data_error_family,
        by = "var_name"
      )
  } else {
    data_with_error <-
      data_source_density %>%
      dplyr::mutate(
        sel_error = data_error_family
      )
  }
  
  # fit GAM for each dataset of reach type
  data_density_hgams <-
    data_with_error %>%
    dplyr::mutate(
      hgam_models = purrr::map2(
        .x = data_to_fit,
        .y = sel_error,
        .f = ~ REcopol:::fit_hgam(
          data_source = .x,
          x_var = "age",
          y_var = "value",
          group_var = group_var,
          smooth_basis = smooth_basis,
          weights_var = weights_var,
          sel_k = sel_k,
          error_family = .y,
          common_trend = common_trend,
          use_parallel = use_parallel,
          use_discrete = use_discrete,
          verbose = TRUE
        )
      )
    )
  
  
  
  # add data.frame to predict on (age vector)
  data_to_predict <-
    data_density_hgams %>%
    tidyr::unnest(hgam_models) %>%
    dplyr::mutate(
      dummy_table = list(data_source_dummy_time)
    )
  
  # this can be limited by the length of the data if
  #   `limit_length` is TRUE
  if (
    isTRUE(limit_length)
  ) {
    data_age_lim <-
      data_source_meta %>%
      dplyr::select(
        dataset_id, age_min, age_max
      )
    
    data_to_predict <-
      data_to_predict %>%
      dplyr::left_join(
        data_age_lim,
        by = "dataset_id"
      ) %>%
      dplyr::mutate(
        dummy_table = purrr::pmap(
          .l = list(dummy_table, age_min, age_max),
          .f = ~ ..1 %>%
            dplyr::filter(
              age >= ..2 & age <= ..3
            )
        )
      ) %>%
      dplyr::select(-c(age_min, age_max))
  }
  
  # predict each GAM
  data_predicted <-
    data_to_predict %>%
    dplyr::mutate(
      pred_data = purrr::map2(
        .x = mod,
        .y = dummy_table,
        .f = ~ REcopol::predic_model(
          data_source = .y,
          model_source = .x
        )
      )
    ) %>%
    tidyr::unnest(pred_data) %>%
    dplyr::select(
      var_name, dataset_id, age, fit
    ) %>%
    tidyr::pivot_wider(
      names_from = var_name,
      values_from = "fit"
    ) %>%
    tidyr::nest(data = -c(dataset_id))
  
  return(data_predicted)
}
  
