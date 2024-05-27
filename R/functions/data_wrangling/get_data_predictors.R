get_data_predictors <- function(data_source_spd_events,
                                data_source_climate) {
  dplyr::inner_join(
    data_source_spd_events %>%
      dplyr::select(
        dataset_id, data_merge
      ) %>%
      dplyr::rename(
        spd_events = data_merge
      ),
    data_source_climate %>%
      dplyr::rename(
        climate_data = data
      ),
    by = "dataset_id"
  ) %>%
    dplyr::mutate(
      data_merge = purrr::map2(
        .x = spd_events,
        .y = climate_data,
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
