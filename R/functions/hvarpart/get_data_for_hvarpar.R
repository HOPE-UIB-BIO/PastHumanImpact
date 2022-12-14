get_data_for_hvarpar <- function(data_source_diversity,
                                 data_source_roc,
                                 data_source_density,
                                 data_source_spd,
                                 data_source_climate) {
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
    )
  dplyr::inner_join(
    data_source_spd,
    by = "dataset_id"
  ) %>%
    dplyr::inner_join(
      data_source_climate,
      by = "dataset_id"
    ) %>%
    dplyr::mutate(
      data_merge = purrr::pmap(
        .l = list(
          diversity, # ..1
          roc, # ..2
          density_diversity, # ..3
          density_turnover, # ..4
          climate_data, # ..5
          spd
        ),
        .f = ~ dplyr::inner_join(
          ..1,
          ..2,
          by = "age"
        ) %>%
          dplyr::inner_join(
            ..3,
            by = "age"
          ) %>%
          dplyr::inner_join(
            ..4,
            by = "age"
          ) %>%
          dplyr::inner_join(
            ..5,
            by = "age"
          )
      )
    ) %>%
    dplyr::select(
      dataset_id, data_merged
    ) %>%
    return()
}
