get_data_hvar_timebin <- function(data_source,
                                  data_meta) {
  
  data_source %>%
    unnest(data_merge) %>%
    drop_na() %>%
    left_join(data_meta %>% 
                dplyr::select(dataset_id, lat, long, region),
              by = "dataset_id") %>%
    nest(data_merge = -c(region, age)) %>%
    mutate(n_samples = purrr::map_dbl(data_merge, 
                                      .f = function(x){ 
                                        nrow(x) %>% 
                                          return()})) %>%
    return()
 
}
