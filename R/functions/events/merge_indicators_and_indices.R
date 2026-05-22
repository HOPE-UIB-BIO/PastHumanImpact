#' @title Merge Indicator And Index Event Signals
#' @description
#' Merge per-level event indicators and indices. A level is flagged for an
#' event class if either source marks it as present.
#' @param data_source_indices A data frame with columns `dataset_id`, `age`,
#'   `weak_indicies`, and `strong_indicies`.
#' @param data_source_indicators A data frame with columns `dataset_id`, `age`,
#'   `weak_indicators`, and `strong_indicators`.
#' @return
#' A data frame with one row per `dataset_id` and a nested `events_updated`
#' table containing `age`, `no_impact`, `weak`, and `strong` as numeric binary
#' columns.
merge_indicators_and_indices <- function(data_source_indices,
                                         data_source_indicators) {
  assertthat::assert_that(
    is.data.frame(data_source_indices),
    msg = "`data_source_indices` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_indicators),
    msg = "`data_source_indicators` must be a data frame."
  )

  req_indices <- c("dataset_id", "age", "weak_indicies", "strong_indicies")
  req_indicators <-
    c("dataset_id", "age", "weak_indicators", "strong_indicators")

  assertthat::assert_that(
    all(req_indices %in% names(data_source_indices)),
    msg = "`data_source_indices` must contain dataset_id, age, weak_indicies, and strong_indicies."
  )
  assertthat::assert_that(
    all(req_indicators %in% names(data_source_indicators)),
    msg = "`data_source_indicators` must contain dataset_id, age, weak_indicators, and strong_indicators."
  )

  dplyr::full_join(
    data_source_indicators,
    data_source_indices,
    by = c("dataset_id", "age"),
    suffix = c("_indicators", "_indicies")
  ) %>%
    dplyr::mutate(
      weak = ifelse(
        weak_indicators == TRUE | weak_indicies == TRUE,
        TRUE,
        FALSE
      ),
      strong = ifelse(
        strong_indicators == TRUE | strong_indicies == TRUE,
        TRUE,
        FALSE
      ),
      no_impact = ifelse(
        strong == FALSE & weak == FALSE,
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::select(
      dataset_id, age, no_impact, weak, strong
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.logical),
        as.numeric
      )
    ) %>%
    tidyr::nest(
      events_updated = -dataset_id
    ) %>%
    return()
}
