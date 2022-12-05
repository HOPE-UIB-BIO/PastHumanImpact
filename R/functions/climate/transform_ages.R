#' @title Transform time_id into ages
#' @description This function use the time reference table in the CHELSA technical documentation for TraCE21k to transform the time_id into ages comparable with the other data.
#' @return A nested tibble with climate variables per dataset_id

transform_ages <-
  function(data_source,
           trans_data){
    
    
    if(!"time_month_value" %in% names(data_source)){
      data_source <-
        data_source %>% 
        dplyr::mutate(
          time_month_value = 0)
      
    }
    
    
    data_source %>% 
      dplyr::inner_join(trans_data, by = c("time_id" = "timeID")) %>% 
      dplyr::mutate(
        mid_age = round((startyear + endyear)/ 2, digits = 0), 
        # convert from gregorian to before present (BP)
        age_greg = - (mid_age - 1950),
        age = age_greg + time_month_value) %>% 
      return()
    
  }
