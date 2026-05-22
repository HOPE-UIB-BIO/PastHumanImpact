#' @title Combine Property And Predictor Tables
#' @description
#' Join nested property and predictor tables by `dataset_id` and merge each
#' dataset pair by `age`.
#' @param data_source_properties A data frame with columns `dataset_id` and
#'   `data_merge` (list-column of property tables containing `age`).
#' @param data_source_predictors A data frame with columns `dataset_id` and
#'   `data_merge` (list-column of predictor tables containing `age`).
#' @return
#' A data frame with columns `dataset_id` and `data_merge` (list-column of
#' per-dataset property/predictor joins).
get_data_combined <- function(data_source_properties,
                              data_source_predictors) {
  assertthat::assert_that(
    is.data.frame(data_source_properties),
    msg = "`data_source_properties` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_predictors),
    msg = "`data_source_predictors` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "data_merge") %in% names(data_source_properties)),
    msg = "`data_source_properties` must contain dataset_id and data_merge."
  )
  assertthat::assert_that(
    all(c("dataset_id", "data_merge") %in% names(data_source_predictors)),
    msg = "`data_source_predictors` must contain dataset_id and data_merge."
  )

  dplyr::inner_join(
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
        )
      )
    ) %>%
    dplyr::select(
      dataset_id, data_merge
    ) %>%
    return()
}
