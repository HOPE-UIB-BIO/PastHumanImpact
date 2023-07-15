get_hgam_density_vars <- function(data_source_density,
                                  data_source_meta = NULL,
                                  data_source_dummy_time,
                                  diversity_vars = c(
                                    "n0", "n1", "n2",
                                    "n2_divided_by_n1", "n1_divided_by_n0"
                                  ),
                                  turnover_vars = c(
                                    "mvrt", "roc", "peakpoints", "dcca"
                                  ),
                                  used_rescales = TRUE,
                                  error_family = "mgcv::betar(link = 'logit')",
                                  smooth_basis = c("tp", "cr"),
                                  sel_k = 10,
                                  limit_length = TRUE) {
  

  # helper functions
  get_vars_for_modelling <- function(data_source,
                                     select_vars) {
    data_source %>%
      dplyr::select(
        age,
        dplyr::all_of(select_vars)
      ) %>%
      tidyr::pivot_longer(
        cols = -age,
        names_to = "var_name",
        values_to = "value"
      ) %>%
      tidyr::drop_na(value) %>%
      return()
  }

  fit_and_predict_hgam <- function(data_source_to_fit,
                                   data_source_time,
                                   error_family = "mgcv::betar(link = 'logit')",
                                   smooth_basis = c("tp", "cr"),
                                   sel_k = 10) {

    data_mod_hgam <-
      REcopol:::fit_hgam(
        data_source = data_source_to_fit,
        x_var = "age",
        y_var = "value",
        group_var = "var_name",
        smooth_basis = smooth_basis,
        sel_k = sel_k,
        error_family = error_family,
        common_trend = TRUE,
        use_parallel = FALSE,
        use_discrete = FALSE,
        max_iterations = 2e3,
        verbose = TRUE
      )

    data_predicted <-
      REcopol::predic_model(
        model_source = data_mod_hgam,
        data_source = data_source_time %>%
          dplyr::mutate(var_name = data_source_to_fit$var_name[1]),
        exclude_var = gratia::smooths(data_mod_hgam) %>%
          stringr::str_subset(., "var_name")
      ) %>%
      dplyr::select(-var_name)

    return(data_predicted)


  }


  # select which data to use
  if (
    isTRUE(used_rescales)
  ) {
    data_for_hgam <-
      data_source_density %>%
      dplyr::mutate(
        data_work = pap_density_rescale
      ) %>%
      dplyr::select(dataset_id, data_work)
  } else {
    data_for_hgam <-
      data_source_density %>%
      dplyr::mutate(
        data_work = pap_density
      ) %>%
      dplyr::select(dataset_id, data_work)
  }

  data_cols_selected <-
    data_for_hgam %>%
    dplyr::mutate(
      # diversity
      data_to_fit_diversity = purrr::map(
        .x = data_work,
        .f = ~ get_vars_for_modelling(.x,
          select_vars = diversity_vars
        )
      ),
      # turnover
      data_to_fit_turnover = purrr::map(
        .x = data_work,
        .f = ~ get_vars_for_modelling(.x,
          select_vars = turnover_vars
        )
      )
    ) %>%
    dplyr::select(dataset_id, data_to_fit_diversity, data_to_fit_turnover) %>%
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

    data_cols_selected <-
      data_cols_selected %>%
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
  
 

  # fit GAM for each dataset of reach type
  data_density_res <-
    data_cols_selected %>%
    dplyr::mutate(
      density_diversity = purrr::map2(
        .x = data_to_fit_diversity,
        .y = dummy_table,
        .f = ~ fit_and_predict_hgam(
          data_source_to_fit = .x,
          data_source_time = .y,
          error_family = error_family,
          smooth_basis = smooth_basis,
          sel_k = sel_k
        ) %>%
          dplyr::select(
            age,
            density_diversity = fit
          )
      ),
      density_turnover = purrr::map2(
        .x = data_to_fit_turnover,
        .y = dummy_table,
        .f = ~ fit_and_predict_hgam(
          data_source_to_fit = .x,
          data_source_time = .y,
          error_family = error_family,
          smooth_basis = smooth_basis,
          sel_k = sel_k
        ) %>%
          dplyr::select(
            age,
            density_turnover = fit
          )
      )
    ) %>%
    dplyr::select(dataset_id, density_diversity, density_turnover)

  return(data_density_res)

 
  }



  

    
