#' @title Get multivariate regression trees (MRT)
#' @description Use pollen assemblage data and age/time as constraint
#' @param data_pollen Use pollen percentages
#' @param n_rand number of randomisations
#' @param transformation_coef which distance coefficient to use.
#' Recommended for pollen data is chi-squared distances of pollen percentage
#' with no data transformations
#' @return For each sequence returns a vector of the zonation,
#' a vector of the change points, and the total number of zones

get_mrt <- function(data_pollen,
                    n_rand = 999,
                    transformation_coef = "chisq") {
  assertthat::assert_that(
    is.data.frame(data_pollen),
    msg = "`data_pollen` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "percentages_harmonised", "levels") %in% names(data_pollen)),
    msg = "`data_pollen` must contain `dataset_id`, `percentages_harmonised`, and `levels`."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_pollen$percentages_harmonised, is.data.frame)),
    msg = "`percentages_harmonised` entries must be data frames."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_pollen$levels, is.data.frame)),
    msg = "`levels` entries must be data frames."
  )
  assertthat::assert_that(
    is.numeric(n_rand) && length(n_rand) == 1 && n_rand > 0,
    msg = "`n_rand` must be a single positive numeric value."
  )
  assertthat::assert_that(
    is.character(transformation_coef) && length(transformation_coef) == 1,
    msg = "`transformation_coef` must be a single character value."
  )

  data_work_mrt <-
    data_pollen %>%
    dplyr::mutate(
      PAP_mrt = purrr::map2(
        .x = percentages_harmonised,
        .y = levels,
        .f = ~ REcopol::mv_regression_partition(
          data_source_counts = .x,
          data_source_levels = .y,
          rand = n_rand,
          transformation = transformation_coef
        )
      )
    )

  data_mrt_proc <-
    data_work_mrt %>%
    dplyr::mutate(
      mvrt_partitions = purrr::map(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("partitions") %>%
          dplyr::rename(MRT_partitions = partition)
      ),
      mvrt_cp = purrr::map(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("change_points")
      ),
      mvrt_groups_n = purrr::map_dbl(
        .x = PAP_mrt,
        .f = ~ .x %>%
          purrr::pluck("mrt_groups")
      )
    )

  data_mrt <-
    data_mrt_proc %>%
    dplyr::select(dataset_id, PAP_mrt, mvrt_partitions, mvrt_cp, mvrt_groups_n)

  return(data_mrt)
}
