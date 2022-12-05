#' @title Merge indicator and indices
#' @description Level is flagged as specific event type if it is present in any 
#' source of data (indices or indicators)
merge_indicators_and_indices <- function(data_source_indices,
                                         data_source_indicators) {
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
