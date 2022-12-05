#' @title A wrapper function to download palaeoclimatic variables from CHELSA 
#' @description This is wrapper function that create a tibble with selected variables to be downloaded, download, extract relevant data and delete the downloaded .tif files. 
#' @return Provide the selected climatic data for each location and a meta data table with the file and url links

get_climate_data <- function(variables_selected = c("bio", "tasmin"),
                             bio_var_selected = c(1, 6, 12, 15, 18, 19),
                             time_var_selected = c(20:-200),
                             month_var_selected = c(1:12),
                             xy = data_meta) {
  
  
  # meta data into data.frame with x (long) y (lat) data only
  xy_data <- xy %>% 
    dplyr::select(dataset_id, long, lat) %>% 
    column_to_rownames("dataset_id")
  
  # select variables and download CHELSA data
  climate_dl <- get_chelsa_trace21k_urls(variables = variables_selected, 
                                           bio_var = bio_var_selected,
                                           month_var = month_var_selected,
                                           time_var = time_var_selected) %>%  
    get_chelsa_download(., extract_data = xy_data) 
  
 # restructure downloaded climate data
  climate_tables <-  climate_dl %>% 
    dplyr::select(variable, time_id, bio, month, climate) %>%
    unnest(cols = climate) %>%
    dplyr::select(variable:month, dataset_id, value) %>%
    mutate(variable = case_when(
      variable == "bio" ~ paste0(variable, bio),
      TRUE ~ paste0(variable, month))) %>%
    dplyr::select(-c(bio, month)) %>%
    nest(climate = -dataset_id) %>%
    mutate(climate = purrr::map(climate, function(.x) {.x %>% arrange(variable)}))
  
  
  # table of meta data
  climate_meta <- climate_dl %>% 
    dplyr::select(-c(histdir, path, climate))
  
  climate_data <- list(data = climate_tables, meta = climate_meta)
  
  return(climate_data)
}


  






