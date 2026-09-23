#' @title Prepare focal SPD observations for external-proxy comparison
#' @description
#' Unnest a selected SPD product, standardize its keys, and retain the common
#' KK10 comparison interval at a requested temporal resolution.
#' @param data_spd Nested SPD product with `dataset_id`, `distance`, and `spd`.
#' @param age_min Youngest retained age in years BP.
#' @param age_max Oldest retained age in years BP.
#' @param age_step Temporal interval in years.
#' @return A tibble with `dataset_id`, `age_bp`, `spd`, and `radius_km`.
#' @examples
#' \dontrun{
#' prepare_spd_human_proxy_input(data_spd_250_with_500_fallback)
#' }
prepare_spd_human_proxy_input <- function(
  data_spd,
  age_min = 2000,
  age_max = 8000,
  age_step = 500
) {
  assertthat::assert_that(
    is.data.frame(data_spd),
    all(c("dataset_id", "distance", "spd") %in% names(data_spd)),
    is.list(data_spd[["spd"]]),
    all(purrr::map_lgl(
      data_spd[["spd"]],
      ~ is.data.frame(.x) && all(c("age", "value") %in% names(.x))
    )),
    is.numeric(age_min),
    is.numeric(age_max),
    is.numeric(age_step),
    age_min < age_max,
    age_step > 0,
    msg = "Focal SPD input does not satisfy the comparison contract."
  )

  retained_ages <- seq(age_min, age_max, by = age_step)

  res_spd <-
    data_spd |>
    dplyr::transmute(
      dataset_id = as.character(.data[["dataset_id"]]),
      radius_km = as.numeric(.data[["distance"]]),
      spd_series = .data[["spd"]]
    ) |>
    tidyr::unnest(cols = dplyr::all_of("spd_series")) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      age_bp = as.numeric(.data[["age"]]),
      spd = as.numeric(.data[["value"]]),
      radius_km = .data[["radius_km"]]
    ) |>
    dplyr::filter(.data[["age_bp"]] %in% retained_ages) |>
    dplyr::arrange(.data[["dataset_id"]], dplyr::desc(.data[["age_bp"]]))

  assertthat::assert_that(
    !anyDuplicated(res_spd[c("dataset_id", "age_bp")]),
    msg = "Prepared SPD data contain duplicate dataset-age keys."
  )

  return(res_spd)
}
