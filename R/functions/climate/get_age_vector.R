get_age_vector <-
  function(data_source_vector, sel_type){
    
    if(stringr::str_detect(sel_type, "bio")){
      age_vector <-
        data_source_vector %>% 
        stringr::str_extract(.,"CHELSA_TraCE21k_[:alpha:][:alpha:][:alpha:].*") %>% 
        stringr::str_replace(., "CHELSA_TraCE21k_bio[:digit:][:digit:]_", "") %>% 
        stringr::str_replace(., "_.*", "") %>% 
        as.double()
    } else {
      age_vector <-
        data_source_vector %>% 
        stringr::str_extract(.,paste0("CHELSA_TraCE21k_",sel_type,"_[:digit:].*")) %>% 
        stringr::str_replace(., paste0("CHELSA_TraCE21k_",sel_type,"_"), "") %>% 
        stringr::str_replace(., "_V.*", "") %>% 
        stringr::str_replace(., ".*_", "") %>%  
        as.double()   
      
    }
    
    return(age_vector)
    
  }