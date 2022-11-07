# Combine data that represent pollen assembly properties to get all PAP combined 

combine_pap <- function(data_assembly, data_diversity, data_mrt, data_roc, data_dcca){
  
  data_levels <- data_assembly %>%
    dplyr::select(
      dataset_id,
      levels
    )
  
  subset_by_vector <-
    function(data_source, var_name, id_vec) {
      data_source %>%
        dplyr::mutate(
          !!var_name := purrr::map2(
            .x = get(var_name),
            .y = get(id_vec),
            .f = ~ .x %>%
              dplyr::filter(.data$sample_id %in% .y)
          )
        ) %>%
        return()
    }
  
  data_combine_paps <-
    data_levels %>%
    dplyr::inner_join(
      data_diversity,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_mrt,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_roc,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_turnover,
      by = "dataset_id"
    ) %>%
    # in order to make sure we have same levels across all data
    dplyr::mutate(
      # get a list of intercept of all samples across data
      valid_sample_id = purrr::pmap(
        .l = list(
          PAP_diversity, # ..1
          mvrt_partitions, # ..2
          dcca_scores, # ..3
          levels # ..4
        ),
        .f = ~ dplyr::inner_join(
          ..1,
          ..2,
          by = "sample_id"
        ) %>%
          dplyr::inner_join(
            ..3,
            by = "sample_id"
          ) %>%
          dplyr::inner_join(..4,
                            by = "sample_id"
          ) %>%
          purrr::pluck("sample_id")
      )
    ) %>%
    # subset all data.frames by the list of common sample_id
    subset_by_vector(
      var_name = "PAP_diversity",
      id_vec = "valid_sample_id"
    ) %>%
    subset_by_vector(
      var_name = "mvrt_partitions",
      id_vec = "valid_sample_id"
    ) %>%
    subset_by_vector(
      var_name = "dcca_scores",
      id_vec = "valid_sample_id"
    ) %>%
    subset_by_vector(
      var_name = "levels",
      id_vec = "valid_sample_id"
    )  %>% 
    dplyr::select(-valid_sample_id)
  

  # readr::write_rds(
  #   data_combine_paps,
  #   here::here(
  #     "Data/Processed/PAP_all/pap_all_2022-09-29.rds"
  #   ),
  #   compress = "gz"
  # )
  
  return(data_combine_paps)
} 