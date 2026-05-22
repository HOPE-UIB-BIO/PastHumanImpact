#' @title Prepare diversity and DCCA data for modelling
#' @description
#' Combines diversity, DCCA, and pollen-age uncertainty inputs into nested
#' long-format modelling tables with variable-specific weights.
#' @param data_source_diversity Data frame with list-column `PAP_diversity`.
#' @param data_source_dcca Data frame with list-column `dcca_scores`.
#' @param data_source_pollen Data frame with columns `dataset_id` and list-column `levels`.
#' @return Data frame with columns `var_name` and nested `data_to_fit` tables.
get_diversity_and_dcca_for_modelling <- function(data_source_diversity,
                                                 data_source_dcca,
                                                 data_source_pollen) {
  assertthat::assert_that(
    is.data.frame(data_source_diversity),
    msg = "`data_source_diversity` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_dcca),
    msg = "`data_source_dcca` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_source_pollen),
    msg = "`data_source_pollen` must be a data frame."
  )

  assertthat::assert_that(
    "PAP_diversity" %in% names(data_source_diversity),
    msg = "`data_source_diversity` must contain `PAP_diversity`."
  )
  assertthat::assert_that(
    "dcca_scores" %in% names(data_source_dcca),
    msg = "`data_source_dcca` must contain `dcca_scores`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "levels") %in% names(data_source_pollen)),
    msg = "`data_source_pollen` must contain `dataset_id` and `levels`."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(data_source_diversity$PAP_diversity, is.data.frame)),
    msg = "`PAP_diversity` entries must be data frames."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_source_dcca$dcca_scores, is.data.frame)),
    msg = "`dcca_scores` entries must be data frames."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_source_pollen$levels, is.data.frame)),
    msg = "`levels` entries must be data frames."
  )

  data_ages <-
    data_source_pollen %>%
    tidyr::unnest(levels) %>%
    dplyr::mutate(
      age_error = abs(lower - upper)
    ) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::mutate(
      age_error_avg = mean(age_error),
      var_weight = age_error_avg / age_error
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dataset_id, sample_id, age, var_weight)

  data_diversity <-
    data_source_diversity %>%
    tidyr::unnest(PAP_diversity)

  data_dcca <-
    data_source_dcca %>%
    tidyr::unnest(dcca_scores) %>%
    dplyr::select(dataset_id, sample_id, dcca_axis_1 = axis_1)

  # wragler data so that there are grouped by col
  data_to_fit <-
    dplyr::full_join(
      data_diversity,
      data_dcca,
      by = c("dataset_id", "sample_id")
    ) %>%
    dplyr::inner_join(
      data_ages,
      by = c("dataset_id", "sample_id")
    ) %>%
    dplyr::select(-sample_id) %>%
    tidyr::pivot_longer(
      cols = -c(dataset_id, age, var_weight),
      names_to = "var_name",
      values_to = "value"
    ) %>%
    tidyr::drop_na(value) %>%
    tidyr::nest(data_to_fit = c(dataset_id, age, value, var_weight))

  return(data_to_fit)
}
