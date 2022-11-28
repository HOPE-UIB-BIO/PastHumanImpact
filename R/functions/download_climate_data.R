download_climate_data <- 
  function(sel_type = "bio01",
           file_vector){
    
    file_vector %>% 
      noquote() %>% 
      write.table(.,
                  file = here::here("Data/Input/selected_files.txt"),
                  quote = FALSE,
                  col.names = FALSE,
                  row.names = FALSE)
    
    here("Data", "Input", "selected_files.txt") %>%
      paste0("wget --no-host-directories  --input-file=", .,
             " --directory-prefix=", here("Data", "Processed", sel_type)) %>%
      system(.)
    
  }