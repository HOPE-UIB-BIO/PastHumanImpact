

get_data_properties <- function(data_source_diversity,
                                data_source_roc,
                                data_source_density,
                                used_rescale = TRUE) {
  
  
  
  if(
    isTRUE(used_rescale)
  ) {
    data_source_density <- data_source_density %>%
      dplyr::select(dataset_id, pap_density_rescale) %>%
      dplyr::rename(density = pap_density_rescale)
  } else {
    data_source_density <- data_source_density %>%
      dplyr::select(dataset_id, pap_density) %>%
      dplyr::rename(density = pap_density)
    
  }
  
  dplyr::inner_join(
    data_source_diversity %>%
      dplyr::rename(
        diversity = data
      ),
    data_source_roc %>%
      dplyr::rename(
        roc = data
      ),
    by = "dataset_id"
  ) %>%
    dplyr::inner_join(
      data_source_density,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      data_merge = purrr::pmap(
        .l = list(
          diversity, # ..1
          roc, # ..2
          density #..3
        ),
        .f = ~ dplyr::inner_join(
          ..1,
          ..2,
          by = "age"
        ) %>%
          dplyr::inner_join(
            ..3,
            by = "age"
          )  %>%
          drop_na()
      ) 
    ) %>%
    dplyr::select(
      dataset_id, data_merge
    ) %>%
    
    return()
}