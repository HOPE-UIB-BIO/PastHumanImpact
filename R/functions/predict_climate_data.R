predict_climate_data <- 
  function(data_source_climate, data_source_level){
    
    var_names <-
      data_source_climate %>% 
      dplyr::select(
        !dplyr::any_of(
          c("age", "time_id"))) %>% 
      names()
    
    clim_pred <-
      var_names %>%
      purrr::set_names() %>% 
      purrr::map_dfc(
        .f = ~ stats::spline(
          x = data_source_climate$age,
          y = data_source_climate %>% 
            purrr::pluck(.x),
          ties = min,
          xout = data_source_level$age,
          method = "natural") %>% 
          purrr::pluck("y"))
    
    res <- 
      data_source_level %>% 
      dplyr::select(sample_id, depth, age) %>% 
      bind_cols(clim_pred)
    
    return(res)
  }