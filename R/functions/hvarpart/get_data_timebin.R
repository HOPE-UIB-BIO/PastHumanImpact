#' @title Aggregate nested data by region and age bins
#' @description
#' Unnest nested `data_merge`, attach metadata, and nest again by `region` and
#' `age` with sample counts.
#' @param data_source Data frame with columns `dataset_id` and `data_merge`.
#' @param data_meta Data frame with columns `dataset_id`, `lat`, `long`, `region`.
#' @return Tibble with columns `region`, `age`, `data_merge`, and `n_samples`.
get_data_timebin <- function(data_source,
                             data_meta) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )

  assertthat::assert_that(
    is.data.frame(data_meta),
    msg = "`data_meta` must be a data frame."
  )

  assertthat::assert_that(
    all(c("dataset_id", "data_merge") %in% names(data_source)),
    msg = "`data_source` must contain `dataset_id` and `data_merge`."
  )

  assertthat::assert_that(
    all(c("dataset_id", "lat", "long", "region") %in% names(data_meta)),
    msg = "`data_meta` must contain `dataset_id`, `lat`, `long`, and `region`."
  )

  res_timebin <-
    data_source %>%
    tidyr::unnest(data_merge) %>%
    dplyr::left_join(
      data_meta %>%
        dplyr::select(dataset_id, lat, long, region),
      by = "dataset_id"
    ) %>%
    tidyr::nest(data_merge = -c(region, age)) %>%
    dplyr::mutate(n_samples = purrr::map_int(data_merge, nrow))

  return(res_timebin)
}
