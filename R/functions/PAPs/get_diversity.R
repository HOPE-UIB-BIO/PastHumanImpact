#' @title Get diversity estimates
#' @description Use pollen assemblage data
#' @param data_pollen Data frame with `dataset_id` and list-column `counts_harmonised`.
#' @param n_rand Number of randomisations.
#' @param sel_method Method passed to `REcopol::diversity_estimate()`.
#' @return For each site dataset the rarefied estimates of Hill´s effective
#' species numbers (which are the estimated number of taxa (N0), estimated
#' number of equally common taxa (N1), estimated number of equally abundant
#' taxa (N2)), and the associated evenness ratios N1/N0 and N2/N1

get_diversity <- function(data_pollen,
                          n_rand = 999,
                          sel_method = "taxonomic") {
  assertthat::assert_that(
    is.data.frame(data_pollen),
    msg = "`data_pollen` must be a data frame."
  )
  assertthat::assert_that(
    all(c("dataset_id", "counts_harmonised") %in% names(data_pollen)),
    msg = "`data_pollen` must contain `dataset_id` and `counts_harmonised`."
  )
  assertthat::assert_that(
    all(purrr::map_lgl(data_pollen$counts_harmonised, is.data.frame)),
    msg = "`counts_harmonised` entries must be data frames."
  )
  assertthat::assert_that(
    is.numeric(n_rand) && length(n_rand) == 1 && !is.na(n_rand) && n_rand > 0,
    msg = "`n_rand` must be a single positive number."
  )
  assertthat::assert_that(
    is.character(sel_method) && length(sel_method) == 1,
    msg = "`sel_method` must be a single character value."
  )

  data_work_diversity <-
    data_pollen %>%
    dplyr::mutate(
      PAP_diversity = purrr::map(
        .x = counts_harmonised,
        .f = ~ REcopol::diversity_estimate(
          data_source = .x,
          sel_method = sel_method,
          rand = n_rand
        )
      )
    )

  data_diversity <-
    data_work_diversity %>%
    dplyr::select(dataset_id, PAP_diversity)

  return(data_diversity)
}
