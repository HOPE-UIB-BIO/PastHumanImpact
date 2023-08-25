make_figure_spatial_trend <-
  function(data_source,
           side = c("long", "lat"),
           var_name,
           error_family) {
    model_trend <-
      REcopol::fit_custom_gam(
        x_var = side,
        y_var = var_name,
        error_family = error_family,
        smooth_basis = "tp",
        data_source = data_source,
        sel_k = 10
        )
    
    x_range <-
      data_source %>%
      purrr::pluck(side) %>%
      range()
    
    # Predict the models
    suppressWarnings(
      data_pred <-
        REcopol::predic_model(
          data_source = tibble::tibble(
            !!side := seq(
            from = min(x_range),
            to = max(x_range)
            )
          ),
          model_source = model_trend
          ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(!!var_name := fit)
      )
    
    return(data_pred)
  }
