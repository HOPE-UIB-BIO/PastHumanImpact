#' @title Prepare human-proxy extraction supports for each record
#' @description
#' Build one extraction location per record and spatial support: the grid
#' cell that contains the record, fixed small radii around it, and the
#' record-specific SPD buffer.
#' @param data_meta
#' Data frame with `dataset_id`, `long`, and `lat`, one row per record.
#' Coordinates must be finite.
#' @param data_spd_support
#' Data frame with `dataset_id` and `radius_km`, the SPD buffer radius of
#' each record (one row per record). Records absent from it receive no
#' `spd_buffer` support; records absent from `data_meta` are ignored.
#' @param radii_km
#' Numeric vector of distinct, positive, finite buffer radii in km.
#' @return
#' Tibble with one row per record and support and the columns
#' `location_id`, `dataset_id`, `support`, `radius_km`, `extraction`, `long`,
#' and `lat`. `support` is `cell`, `radius_<r>km` (for example `radius_10km`),
#' or `spd_buffer`. `radius_km` is `NA` for `cell`, `r` for `radius_<r>km`,
#' and the record's SPD radius for `spd_buffer`. `extraction` is `point` for
#' `cell` and `buffer` otherwise. `location_id` is `<dataset_id>__<support>`
#' and is unique. Rows are ordered by `dataset_id`, then `cell`, the radii in
#' increasing order, and `spd_buffer`.
#' @examples
#' \dontrun{
#' prepare_human_proxy_support_locations(
#'   data_meta = data_meta,
#'   data_spd_support = data_spd_support,
#'   radii_km = c(10, 25, 50)
#' )
#' }
prepare_human_proxy_support_locations <- function(
  data_meta,
  data_spd_support,
  radii_km = c(10, 25, 50)
) {
  assertthat::assert_that(
    is.data.frame(data_meta),
    all(c("dataset_id", "long", "lat") %in% names(data_meta)),
    is.data.frame(data_spd_support),
    all(c("dataset_id", "radius_km") %in% names(data_spd_support)),
    is.numeric(radii_km),
    length(radii_km) > 0L,
    msg = "Human-proxy support inputs do not satisfy the contract."
  )

  data_meta_standardised <-
    data_meta |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      long = as.numeric(.data[["long"]]),
      lat = as.numeric(.data[["lat"]])
    ) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "long", "lat")))

  data_spd_standardised <-
    data_spd_support |>
    dplyr::mutate(
      dataset_id = as.character(.data[["dataset_id"]]),
      radius_km = as.numeric(.data[["radius_km"]])
    ) |>
    dplyr::select(dplyr::all_of(c("dataset_id", "radius_km")))

  assertthat::assert_that(
    !anyDuplicated(data_meta_standardised[["dataset_id"]]),
    all(is.finite(data_meta_standardised[["long"]])),
    all(is.finite(data_meta_standardised[["lat"]])),
    !anyDuplicated(data_spd_standardised[["dataset_id"]]),
    all(is.finite(data_spd_standardised[["radius_km"]])),
    all(data_spd_standardised[["radius_km"]] > 0),
    all(is.finite(radii_km)),
    all(radii_km > 0),
    !anyDuplicated(radii_km),
    msg = "Human-proxy support inputs do not satisfy the contract."
  )

  vec_radii_sorted <-
    sort(radii_km)

  data_fixed_supports <-
    tibble::tibble(
      support = c(
        "cell",
        stringr::str_c("radius_", vec_radii_sorted, "km")
      ),
      radius_km = c(NA_real_, vec_radii_sorted),
      extraction = c(
        "point",
        rep("buffer", length(vec_radii_sorted))
      ),
      support_order = seq_len(length(vec_radii_sorted) + 1L)
    )

  data_fixed_locations <-
    data_meta_standardised |>
    tidyr::crossing(data_fixed_supports)

  data_spd_locations <-
    data_meta_standardised |>
    dplyr::inner_join(
      data_spd_standardised,
      by = dplyr::join_by("dataset_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      support = "spd_buffer",
      extraction = "buffer",
      support_order = nrow(data_fixed_supports) + 1L
    )

  res_locations <-
    dplyr::bind_rows(
      data_fixed_locations,
      data_spd_locations
    ) |>
    dplyr::arrange(
      .data[["dataset_id"]],
      .data[["support_order"]]
    ) |>
    dplyr::mutate(
      location_id = stringr::str_c(
        .data[["dataset_id"]],
        "__",
        .data[["support"]]
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          "location_id",
          "dataset_id",
          "support",
          "radius_km",
          "extraction",
          "long",
          "lat"
        )
      )
    )

  return(res_locations)
}
