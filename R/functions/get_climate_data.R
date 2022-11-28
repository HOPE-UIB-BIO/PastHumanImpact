




get_climate_data <- 
  function(
    data_source,
    sel_type = "bio01",
    var_name,
    path_vector,
    path_to_save){
    
    if(missing(var_name)){
      var_name <- sel_type
    }
    
    if(missing(path_to_save)){
      path_to_save <- here::here("Outputs/Data", var_name)
    }
    
    clim_data <- 
      load_climate_data(sel_type = sel_type,
                path_vector)
    
    data_ext <-
      extract_climate_data(
        clim_data_raster_list = clim_data,
        data_source = data_source,
        sel_type = sel_type) 
    
    var_name_dat <- paste0(var_name, "_data")
    
    data_w <- 
      data_ext %>% 
      dplyr::rename(
        !!var_name_dat := var)
    
    return(data_w)
  }