#' @title Prepare one matched human-proxy dataset history
#' @description Restrict a canonical dataset history to the comparison interval,
#' join unique proxy values, and verify canonical SPD agreement.
#' @param data_dataset One canonical dataset history.
#' @param dataset_id Dataset identifier used for the join.
#' @param data_proxy_model Unique dataset-age proxy values.
#' @param age_min Youngest retained age in years BP.
#' @param age_max Oldest retained age in years BP.
#' @param tolerance Numerical tolerance for canonical SPD agreement.
#' @return The matched dataset history ordered by age.
#' @examples
#' \dontrun{
#' prepare_matched_human_proxy_dataset(data, "1", proxies, 2000, 8000)
#' }
prepare_matched_human_proxy_dataset <- function(
  data_dataset,
  dataset_id,
  data_proxy_model,
  age_min = 2000,
  age_max = 8000,
  tolerance = 5e-4
) {
  assertthat::assert_that(
    is.data.frame(data_dataset),
    "age" %in% names(data_dataset),
    length(dataset_id) == 1L,
    is.data.frame(data_proxy_model),
    all(c("dataset_id", "age", "spd_raw") %in% names(data_proxy_model)),
    !anyDuplicated(data_proxy_model[c("dataset_id", "age")]),
    age_min < age_max,
    msg = "Matched dataset inputs do not satisfy the contract."
  )
  joined <- data_dataset |>
    dplyr::filter(dplyr::between(.data[["age"]], age_min, age_max)) |>
    dplyr::mutate(.join_dataset_id = as.character(dataset_id)) |>
    dplyr::inner_join(
      data_proxy_model,
      by = c(".join_dataset_id" = "dataset_id", "age"),
      relationship = "many-to-one"
    )
  canonical_spd <- if ("spd" %in% names(joined)) {
    joined[["spd"]]
  } else {
    joined[["spd_raw"]]
  }
  assertthat::assert_that(
    all(abs(canonical_spd - joined[["spd_raw"]]) <= tolerance),
    msg = paste0(
      "Matched spd_raw does not agree with canonical SPD for dataset ",
      dataset_id, "."
    )
  )
  res <- joined |>
    dplyr::select(-dplyr::all_of(".join_dataset_id")) |>
    dplyr::arrange(.data[["age"]])

  return(res)
}
