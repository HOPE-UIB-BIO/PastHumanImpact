#' @title Combine all variables that represent pollen assembly properties
#' @description Combining the data of the different pollen assemblage properties
#' @param data_pollen Data frame with `dataset_id` and list-column `levels`.
#' @param data_diversity Data frame with `dataset_id` and list-column `PAP_diversity`.
#' @param data_mrt Data frame with `dataset_id` and list-column `mvrt_partitions`.
#' @param data_roc Data frame with `dataset_id`.
#' @param data_dcca Data frame with `dataset_id` and list-column `dcca_scores`.
#' @return A new data set of the relevant PAP estimations
#'
prepare_data_cp <- function(data_pollen,
                            data_diversity,
                            data_mrt,
                            data_roc,
                            data_dcca) {
  assertthat::assert_that(
    is.data.frame(data_pollen),
    msg = "`data_pollen` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_diversity),
    msg = "`data_diversity` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_mrt),
    msg = "`data_mrt` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_roc),
    msg = "`data_roc` must be a data frame."
  )
  assertthat::assert_that(
    is.data.frame(data_dcca),
    msg = "`data_dcca` must be a data frame."
  )

  assertthat::assert_that(
    all(c("dataset_id", "levels") %in% names(data_pollen)),
    msg = "`data_pollen` must contain `dataset_id` and `levels`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "PAP_diversity") %in% names(data_diversity)),
    msg = "`data_diversity` must contain `dataset_id` and `PAP_diversity`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "mvrt_partitions") %in% names(data_mrt)),
    msg = "`data_mrt` must contain `dataset_id` and `mvrt_partitions`."
  )
  assertthat::assert_that(
    "dataset_id" %in% names(data_roc),
    msg = "`data_roc` must contain `dataset_id`."
  )
  assertthat::assert_that(
    all(c("dataset_id", "dcca_scores") %in% names(data_dcca)),
    msg = "`data_dcca` must contain `dataset_id` and `dcca_scores`."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(data_pollen$levels, is.data.frame)),
    msg = "`levels` entries must be data frames."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_diversity$PAP_diversity, is.data.frame)),
    msg = "`PAP_diversity` entries must be data frames."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_mrt$mvrt_partitions, is.data.frame)),
    msg = "`mvrt_partitions` entries must be data frames."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_dcca$dcca_scores, is.data.frame)),
    msg = "`dcca_scores` entries must be data frames."
  )

  data_levels <-
    data_pollen %>%
    dplyr::select(
      dataset_id,
      levels
    )

  subset_by_vector <-
    function(data_source, var_name, id_vec) {
      data_source %>%
        dplyr::mutate(
          !!var_name := purrr::map2(
            .x = get(var_name),
            .y = get(id_vec),
            .f = ~ .x %>%
              dplyr::filter(.data$sample_id %in% .y)
          )
        ) %>%
        return()
    }

  data_for_cp <-
    data_levels %>%
    dplyr::inner_join(
      data_diversity,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_mrt,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_roc,
      by = "dataset_id"
    ) %>%
    dplyr::inner_join(
      data_dcca,
      by = "dataset_id"
    ) %>%
    # in order to make sure we have same levels across all data
    dplyr::mutate(
      # get a list of intercept of all samples across data
      valid_sample_id = purrr::pmap(
        .l = list(
          PAP_diversity, # ..1
          mvrt_partitions, # ..2
          dcca_scores, # ..3
          levels # ..4
        ),
        .f = ~ dplyr::inner_join(
          ..1,
          ..2,
          by = "sample_id"
        ) %>%
          dplyr::inner_join(
            ..3,
            by = "sample_id"
          ) %>%
          dplyr::inner_join(..4,
            by = "sample_id"
          ) %>%
          purrr::pluck("sample_id")
      )
    ) %>%
    # subset all data.frames by the list of common sample_id
    subset_by_vector(
      var_name = "PAP_diversity",
      id_vec = "valid_sample_id"
    ) %>%
    subset_by_vector(
      var_name = "mvrt_partitions",
      id_vec = "valid_sample_id"
    ) %>%
    subset_by_vector(
      var_name = "dcca_scores",
      id_vec = "valid_sample_id"
    ) %>%
    subset_by_vector(
      var_name = "levels",
      id_vec = "valid_sample_id"
    ) %>%
    dplyr::select(-valid_sample_id)


  return(data_for_cp)
}
