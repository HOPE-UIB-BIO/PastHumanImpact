#' @title Merge Diversity, ROC, And Density Properties
#' @description
#' Join per-dataset diversity, ROC, and density list-column tables into a
#' single nested property table per dataset.
#' @param data_source_diversity A data frame with columns `dataset_id` and
#'   `data` where `data` is a list-column of tables containing `age`.
#' @param data_source_roc A data frame with columns `dataset_id` and `data`
#'   where `data` is a list-column of tables containing `age`.
#' @param data_source_density A data frame with column `dataset_id` and either
#'   `pap_density_rescale` (when `used_rescale = TRUE`) or `pap_density` (when
#'   `used_rescale = FALSE`) as list-columns of tables containing `age`.
#' @param used_rescale Logical. If `TRUE` (default), use
#'   `pap_density_rescale`; otherwise use `pap_density`.
#' @return
#' A data frame with columns `dataset_id` and `data_merge` (list-column of
#' merged property tables).
get_data_properties <- function(data_source_diversity,
                                data_source_roc,
                                data_source_density,
                                used_rescale = TRUE) {
  assertthat::assert_that(
    is.data.frame(data_source_diversity),
    msg = "`data_source_diversity` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_roc),
    msg = "`data_source_roc` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_density),
    msg = "`data_source_density` must be a data frame."
  )
  assertthat::assert_that(
    is.logical(used_rescale),
    length(used_rescale) == 1,
    !is.na(used_rescale),
    msg = "`used_rescale` must be TRUE or FALSE."
  )

  assertthat::assert_that(
    all(c("dataset_id", "data") %in% names(data_source_diversity)),
    msg = "`data_source_diversity` must contain dataset_id and data."
  )
  assertthat::assert_that(
    all(c("dataset_id", "data") %in% names(data_source_roc)),
    msg = "`data_source_roc` must contain dataset_id and data."
  )

  density_column <- if (isTRUE(used_rescale)) {
    "pap_density_rescale"
  } else {
    "pap_density"
  }

  assertthat::assert_that(
    all(c("dataset_id", density_column) %in% names(data_source_density)),
    msg = paste0(
      "`data_source_density` must contain dataset_id and ",
      density_column,
      "."
    )
  )

  if (
    isTRUE(used_rescale)
  ) {
    data_source_density <- data_source_density %>%
      dplyr::select(dataset_id, pap_density_rescale) %>%
      dplyr::rename(density = pap_density_rescale)
  } else {
    data_source_density <- data_source_density %>%
      dplyr::select(dataset_id, pap_density) %>%
      dplyr::rename(density = pap_density)
  }

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
    ) %>%
    dplyr::mutate(
      data_merge = purrr::pmap(
        .l = list(
          diversity, # ..1
          roc, # ..2
          density # ..3
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
          drop_na()
      )
    ) %>%
    dplyr::select(
      dataset_id, data_merge
    ) %>%
    return()
}
