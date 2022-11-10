# join datasets for hypothesis 1 analysis

get_data_h1 <- function(data_meta, data_combined_pap, data_density) {
  data_join <-
    data_meta %>%
    dplyr::left_join(
      data_combine_paps,
      by = "dataset_id"
    ) %>%
    dplyr::left_join(
      data_density,
      by = "dataset_id"
    )
  
  data_for_analyses <-
    data_join %>%
    dplyr::mutate(
      PAP_merge = purrr::pmap(
        .l = list(
          levels, # ..1
          PAP_diversity, # ..2
          dcca_scores # ..3
        ),
        .f = ~ ..1 %>%
          dplyr::select(
            sample_id,
            age
          ) %>%
          dplyr::inner_join(
            ..2,
            by = "sample_id"
          ) %>%
          dplyr::inner_join(
            ..3 %>%
              dplyr::select(
                sample_id,
                dcca_axis_1 = axis_1
              ),
            by = "sample_id"
          ) 
      )
    ) %>%
    dplyr::select(
      dataset_id,
      long, lat,
      Climate_zone,
      dcca_grad_length,
      mvrt_groups_n,
      PAP_merge,
      PAP_roc,
      pap_density
    )
  
  return(data_for_analyses)
  
}