get_data_timebin <- function(data_source,
                             data_meta) {
  
  data_source %>%
    tidyr::unnest(data_merge) %>%
    dplyr::left_join(data_meta %>% 
                       dplyr::select(dataset_id, lat, long, region),
                     by = "dataset_id") %>%
    tidyr::nest(data_merge = -c(region, age)) %>%
    mutate(n_samples = purrr::map_int(data_merge, nrow))  %>%
    return()
 
}
