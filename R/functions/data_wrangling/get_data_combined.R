get_data_combined <- function(data_source_properties, 
                              data_source_predictors) {
  
  
  
  dplyr::left_join(
    data_source_properties %>%
      dplyr::rename(
        properties = data_merge
      ),
      data_source_predictors %>%
      dplyr::rename(
        predictors = data_merge
      ),
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      data_merge = purrr::map2(
        .x = properties,
        .y = predictors,
        .f = ~ dplyr::inner_join(
          .x,
          .y,
          by = "age"
        ) %>%
          drop_na()
      ) 
    ) %>%
    dplyr::select(
      dataset_id, data_merge
    ) %>%
    return()
  
}