load_climate_data <-
  function(sel_type = "bio01",
           path_vector){
    
    dir_list <-
      list.dirs(
        here::here("Data/Processed"))
    
    # check the presence of the data folder and creat it if necessary
    if(!any(stringr::str_detect(dir_list, sel_type))){
      
      dir.create(here::here("Data", "Processed", sel_type))
    }
    
    RUtilpol::output_message(
      msg = paste("Detected the folder", sel_type))
    
    list_of_downloaded_files_names <- 
      list.files(here("Data", "Processed", sel_type), 
                 pattern = ".tif$", full.names = TRUE) %>% 
      stringr::str_replace(
        .,
        paste0(".*/",sel_type, "/"),
        "")
    
    list_of_all_files <-
      select_climatedata_to_download(
        sel_type = sel_type,
        age_sel = age_seq,
        path_vector) 
    
    
    sel_type_short <-
      sel_type %>% 
      stringr::str_replace(., "[:digit:].*", "")
    
    list_of_all_files_names <-
      list_of_all_files %>% 
      stringr::str_replace(
        .,
        paste0(".*/",sel_type_short, "/"),
        "")
    
    # check if everything is present
    if(any(!list_of_all_files_names %in% list_of_downloaded_files_names)){
      
      missing_data <- 
        list_of_all_files %>% 
        subset(., !list_of_all_files_names %in% list_of_downloaded_files_names) %>% 
        na.omit()
      
      if(length(missing_data) >0 ){
        
        RUtilpol::output_comment(
          msg = paste("Downloading dataset N =", length(missing_data)))
        
        agree_to_download <- 
          usethis::ui_yeah("Do you want to proceed with the download")
        
        if(agree_to_download){
          download_data(sel_type = sel_type,
                        file_vector = missing_data)  
        }
        
      } else {
        
        RUtilpol::output_comment(
          msg = paste("All data are already downloaded"))
      }
    } 
    
    temp_table <- 
      list.files(here("Data", "Processed", sel_type), 
                 pattern = ".tif$", full.names = TRUE) %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(
        time_id =   get_age_vector(value, sel_type)) %>% 
      # order by time ID for merging later
      dplyr::arrange(dplyr::desc(time_id))
    
    RUtilpol::output_comment(
      msg = paste("Loading all", nrow(temp_table), "TIF files"))
    
    
    # add id code (name of the raster)
    if(stringr::str_detect(sel_type, "bio")){
      
      name_vec <- 
        temp_table$time_id
      
    } else {
      # add moths if needed
      
      # extract the month info
      name_vec <-
        temp_table$value %>% 
        stringr::str_extract(., paste0("CHELSA_TraCE21k_",sel_type,"_[:digit:].*")) %>% 
        stringr::str_replace(., paste0("CHELSA_TraCE21k_",sel_type,"_"), "") %>% 
        stringr::str_replace(., "_V.*", "") %>% 
        stringr::str_replace(., "_.*", "") %>%  
        as.double() %>% 
        paste(temp_table$time_id, ., sep = "_")
      
    }
    
    temp_list <-
      temp_table %>% 
      dplyr::pull(value) %>% 
      # load data
      purrr::map(raster) %>% 
      purrr::set_names(nm = name_vec)  
    
    
    return(temp_list)
    
  }