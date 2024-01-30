get_data_filtered <- function(data_source_combined,
                              data_source_meta,
                              age_from = 2000,
                              age_to = 8500,
                              remove_private = TRUE){
  
  # - filter datasets 
  if(isTRUE(remove_private)){
    data_filtered <- 
      data_source_combined %>%
      dplyr::left_join(data_meta %>%
                         dplyr::select(dataset_id, 
                                       region, 
                                       data_publicity),
                       by = "dataset_id") %>%
      dplyr::filter(!(region == "Latin America" & data_publicity == "private")) 
    } else {
      data_filtered <- data_source_combined
    }
    
  # - filter age limits within datasets
  data_filtered <- 
    data_filter %>%
    tidyr::unnest(data_merge) %>%
    dplyr::filter(
            age >= age_from & age <= age_to
          ) %>%
    tidyr::nest(data_merge = -c(dataset_id)) %>%
    return()
    
} 