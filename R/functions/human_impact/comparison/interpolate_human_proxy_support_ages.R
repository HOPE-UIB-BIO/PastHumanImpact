#' @title Interpolate extracted human-proxy values to target ages
#' @description
#' Linearly interpolate values extracted at source-layer ages to target ages
#' separately for each record and support, using
#' `interpolate_human_proxy_ages()`. Values are never extrapolated.
#' @param data_extracted
#' Data frame with `dataset_id`, `support`, `radius_km`, `age_bp`, and
#' `value`, as from `aggregate_human_proxy_supports()`.
#' @param target_ages_bp
#' Numeric vector of finite target ages in years BP.
#' @return
#' Tibble with one row per record, support, and target age and the columns
#' `dataset_id`, `support`, `radius_km`, `age_bp`, `value`,
#' `source_age_younger`, `source_age_older`, and `interpolation_weight`.
#' `value` is `NA` where a target age is not bracketed by two finite source
#' values of that record and support.
#' @details
#' Missing source values are skipped, so a target age is interpolated
#' between the nearest finite source values on either side.
#' @examples
#' \dontrun{
#' interpolate_human_proxy_support_ages(data_extracted, c(500, 1000))
#' }
interpolate_human_proxy_support_ages <- function(
  data_extracted,
  target_ages_bp
) {
  assertthat::assert_that(
    is.data.frame(data_extracted),
    all(
      c("dataset_id", "support", "radius_km", "age_bp", "value") %in%
        names(data_extracted)
    ),
    is.numeric(target_ages_bp),
    length(target_ages_bp) > 0L,
    all(is.finite(target_ages_bp)),
    msg = "Support-age interpolation inputs do not satisfy the contract."
  )

  data_location_keys <-
    data_extracted |>
    dplyr::distinct(
      .data[["dataset_id"]],
      .data[["support"]],
      .data[["radius_km"]]
    ) |>
    dplyr::mutate(
      location_id = stringr::str_c(
        .data[["dataset_id"]],
        "__",
        .data[["support"]]
      )
    )

  assertthat::assert_that(
    !anyDuplicated(data_location_keys[["location_id"]]),
    msg = "Each record and support must have one radius."
  )

  # The interpolation helper groups by `dataset_id`, so each record and
  #   support is passed as its own location key.
  data_by_location <-
    data_extracted |>
    dplyr::mutate(
      location_id = stringr::str_c(
        .data[["dataset_id"]],
        "__",
        .data[["support"]]
      )
    ) |>
    dplyr::select(
      dataset_id = "location_id",
      dplyr::all_of(c("age_bp", "value"))
    )

  res_interpolated <-
    interpolate_human_proxy_ages(
      data_source = data_by_location,
      target_ages = target_ages_bp
    ) |>
    dplyr::rename(location_id = "dataset_id") |>
    dplyr::inner_join(
      data_location_keys,
      by = dplyr::join_by("location_id"),
      relationship = "many-to-one",
      unmatched = "error"
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "dataset_id",
          "support",
          "radius_km",
          "age_bp",
          "value",
          "source_age_younger",
          "source_age_older",
          "interpolation_weight"
        )
      )
    )

  return(res_interpolated)
}
