#' @title Merge SPD Events And Climate Predictors
#' @description
#' Join nested SPD-event tables with nested climate tables per dataset and keep
#' complete merged rows by age.
#' @param data_source_spd_events A data frame with columns `dataset_id` and
#'   `data_merge` (list-column of SPD/event tables containing `age`).
#' @param data_source_climate A data frame with columns `dataset_id` and `data`
#'   (list-column of climate tables containing `age`).
#' @return
#' A data frame with columns `dataset_id` and `data_merge` (list-column of
#' merged predictor tables).
get_data_predictors <- function(data_source_spd_events,
                                data_source_climate) {
  assertthat::assert_that(
    is.data.frame(data_source_spd_events),
    msg = "`data_source_spd_events` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_climate),
    msg = "`data_source_climate` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "data_merge") %in% names(data_source_spd_events)),
    msg = "`data_source_spd_events` must contain dataset_id and data_merge."
  )
  assertthat::assert_that(
    all(c("dataset_id", "data") %in% names(data_source_climate)),
    msg = "`data_source_climate` must contain dataset_id and data."
  )

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
