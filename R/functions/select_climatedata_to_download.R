select_climatedata_to_download <-
  function(sel_type = "bio01",
           age_sel = age_seq,
           path_vector){
    
    filter_type <-
      path_vector %>% 
      stringr::str_detect(., sel_type) 
    
    path_vector_type_sub <- 
      path_vector %>% 
      subset(., filter_type)
    
    age_id <- 
      get_age_vector(path_vector_type_sub, sel_type)
    
    filter_age <- 
      age_id %in% age_sel
    
    path_vector_type_sub <-
      path_vector_type_sub %>% 
      subset(., filter_age)
    
    return(path_vector_type_sub)
  }