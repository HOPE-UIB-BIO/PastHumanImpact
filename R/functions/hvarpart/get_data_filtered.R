get_data_filtered <- function(data_source,
                              data_meta,
                              age_from = 2000,
                              age_to = 8500,
                              remove_private = TRUE){
  
  
    # - filter datasets 
    if(isTRUE(remove_private)){
      keep_dataset <- 
        data_meta %>%
        dplyr::select(dataset_id, 
                      region, 
                      data_publicity) %>%
        dplyr::filter(!(region == "Latin America" 
                        & data_publicity == "private")) %>% 
        dplyr::filter(!region == "Africa") %>% 
        pluck("dataset_id")
      
    } else {
      keep_dataset <- meta_data$dataset_id
    }
    
  # - filter age limits within datasets
  data_source %>%
    dplyr::filter(dataset_id %in% keep_dataset) %>%
    tidyr::unnest(data_merge) %>%
    dplyr::filter(
      age >= age_from & age <= age_to
    ) %>%
    tidyr::nest(data_merge = -c(dataset_id)) %>%
    return()
    
} 
