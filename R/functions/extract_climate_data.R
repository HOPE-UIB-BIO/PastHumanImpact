extract_climate_data <- 
  function(clim_data_raster_list,
           data_source,
           sel_type){
    
    coord <- 
      data_source %>% 
      dplyr::select(long, lat)
    
    future::plan(multisession)
    
    data_pred <- 
      clim_data_raster_list %>% 
      purrr::set_names(nm = names(clim_data_raster_list)) %>% 
      furrr::future_map_dfr(
        .x = .,
        .f = ~ coord %>% 
          dplyr::mutate(
            dataset_id = data_source$dataset_id,
            value = raster::extract(.x, coord)),
        .id = "time_id",
        .progress = TRUE,
        .options = furrr_options(seed = NULL))
    
    if(stringr::str_detect(sel_type, "bio")){
      
      data_id_fix <- 
        data_pred %>% 
        dplyr::mutate(
          time_id_fix = as.double(time_id),
          time_month_value = 0)
      
    } else {
      
      data_id_fix <- 
        data_pred %>% 
        dplyr::mutate(
          time_id_month = stringr::str_extract(time_id, "_.*") %>% 
            stringr::str_replace(., "_", "") %>% 
            as.double(),
          time_month_value = (1/12) * (time_id_month -1),
          time_id_fix = stringr::str_replace(time_id, "_.*", "") %>% 
            as.double())
      
    }
    
    # merge with temperature reference to get age
    data_merge_age <-
      transform_ages(
        data_source = data_id_fix,
        trans_data = temp_ref)
    
    # data_id_fix %>% 
    # dplyr::inner_join(temp_ref, by = c("time_id_fix" = "timeID")) %>% 
    # dplyr::mutate(
    #   mid_age = round((startyear + endyear)/ 2, digits = 0), 
    #   # convert from gregorian to before present (BP)
    #   age_greg = - (mid_age - 1950),
    #   age = age_greg + time_month_value) %>% 
    # dplyr::arrange(dataset_id, age)
    
    res <- 
      data_merge_age %>% 
      dplyr::arrange(dataset_id, age) %>% 
      dplyr::select(dataset_id, value, time_id, age) %>% 
      tidyr::nest(var = c(value, time_id, age))
    
    return(res)
  }