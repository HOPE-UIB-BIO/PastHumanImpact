#' @title Filter hvarpart source data by dataset and age
#' @description
#' Filter nested `data_merge` payloads by dataset-level metadata constraints and
#' age range.
#' @param data_source Data frame with columns `dataset_id` and `data_merge`.
#' @param data_meta Data frame with `dataset_id`, `region`, and `data_publicity`.
#' @param age_from Numeric lower age bound.
#' @param age_to Numeric upper age bound.
#' @param remove_private Logical. If `TRUE`, private Latin America datasets are
#' removed.
#' @return Data frame with columns `dataset_id` and nested `data_merge`.
get_data_filtered <- function(data_source,
                              data_meta,
                              age_from = 2000,
                              age_to = 8500,
                              remove_private = TRUE) {
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
    all(c("dataset_id", "region", "data_publicity") %in% names(data_meta)),
    msg = "`data_meta` must contain `dataset_id`, `region`, and `data_publicity`."
  )

  assertthat::assert_that(
    is.logical(remove_private) && length(remove_private) == 1,
    msg = "`remove_private` must be a single logical value."
  )

  assertthat::assert_that(
    is.numeric(age_from) && length(age_from) == 1,
    msg = "`age_from` must be a single numeric value."
  )

  assertthat::assert_that(
    is.numeric(age_to) && length(age_to) == 1,
    msg = "`age_to` must be a single numeric value."
  )

  assertthat::assert_that(
    age_from <= age_to,
    msg = "`age_from` must be less than or equal to `age_to`."
  )

  # - filter datasets
  if (
    isTRUE(remove_private)
  ) {
    keep_dataset <-
      data_meta %>%
      dplyr::select(
        dataset_id,
        region,
        data_publicity
      ) %>%
      dplyr::filter(
        !(region == "Latin America" &
          data_publicity == "private")
      ) %>%
      dplyr::filter(!region == "Africa") %>%
      purrr::chuck("dataset_id")
  } else {
    keep_dataset <-
      data_meta %>%
      purrr::chuck("dataset_id")
  }

  # - filter age limits within datasets
  res_data <-
    data_source %>%
    dplyr::filter(dataset_id %in% keep_dataset) %>%
    tidyr::unnest(data_merge) %>%
    dplyr::filter(
      age >= age_from & age <= age_to
    ) %>%
    tidyr::nest(data_merge = -c(dataset_id))

  return(res_data)
}
