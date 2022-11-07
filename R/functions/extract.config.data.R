.extract.config.data <- function() {
  
  #' @title Extract information form config file
  
  #' @return List with all the important (data-related) settings
  
  #' @description Gather information about current setting (config).
  
  res_list <-
    list(
      
      date = current_date
     
      # Add more settings from config file here 
     )
  
  return(res_list)
  
} 

