# function to get change points of pap estimates using regression trees

get_change_points_pap <- function(data_combined_pap){
  
  data_diversity_cp <-
    data_combine_paps %>%
    dplyr::mutate(
      diversity_cp = purrr::map2(
        .x = PAP_diversity,
        .y = levels,
        .f = ~ {
          var_list <-
            dplyr::inner_join(
              .x, .y %>%
                dplyr::select(sample_id, age),
              by = "sample_id"
            ) %>%
            tibble::column_to_rownames("sample_id") %>%
            tidyr::pivot_longer(
              cols = -age,
              names_to = "var_name"
            ) %>%
            split(.$var_name)
          
          cp_list <-
            purrr::map(
              .x = var_list,
              .f = ~ REcopol::regression_partition(
                data_source = .x,
                var = "value",
                age_var = "age"
              ) %>%
                purrr::pluck("rpart_change_points")
            )
          
          purrr::map_dfr(
            .x = cp_list,
            .id = "var_name",
            .f = ~ data.frame(
              age = .x
            )
          ) %>%
            tibble::as_tibble() %>%
            return()
        }
      ),
    ) %>%
    dplyr::select(dataset_id, diversity_cp)
  
  # roc
  data_roc_cp <-
    data_combine_paps %>%
    dplyr::mutate(
      roc_cp = purrr::map(
        .x = PAP_roc,
        .f = ~ REcopol::regression_partition(
          data_source = .x,
          var = "ROC",
          age_var = "Age"
        ) %>%
          purrr::pluck("rpart_change_points")
      ),
      roc_pp = purrr::map(
        .x = PAP_roc,
        .f = ~ .x %>%
          dplyr::filter(Peak == TRUE) %>%
          purrr::pluck("Age")
      )
    ) %>%
    dplyr::select(dataset_id, roc_cp, roc_pp)
  
  # turnover
  data_turnover_cp <-
    data_combine_paps %>%
    dplyr::mutate(
      dcca_cp = purrr::map2(
        .x = dcca_scores,
        .y = levels,
        .f = ~ dplyr::inner_join(
          .x, .y,
          by = "sample_id"
        ) %>%
          REcopol::regression_partition(
            data_source = .,
            var = "axis_1",
            age_var = "age"
          ) %>%
          purrr::pluck("rpart_change_points")
      )
    ) %>%
    dplyr::select(dataset_id, dcca_cp)

  
  data_change_points <-
    data_combine_paps %>%
    dplyr::select(
      dataset_id,
      mvrt_cp
    ) %>%
    dplyr::inner_join(
      data_diversity_cp,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_roc_cp,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_turnover_cp,
      by = "dataset_id"
    )
  
  # readr::write_rds(
  #   data_change_points,
  #   here::here(
  #     "Data/Processed/Partitions/PAP_change_points_2022-09-29.rds"
  #   ),
  #   compress = "gz"
  # )
  return(data_change_points)
  
}