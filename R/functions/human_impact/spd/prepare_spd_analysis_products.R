#' @title Prepare strict and fallback SPD analysis products
#' @description
#' Convert matched radius products into legacy-compatible strict 250 km,
#' strict 500 km, and 250 km with 500 km fallback datasets.
#' @param data_spd_by_radius Matched SPD data with one row per dataset and
#' radius, nested `spd` series, and an `available` flag.
#' @return Named list containing `data_spd_strict_250`,
#' `data_spd_strict_500`, and `data_spd_250_with_500_fallback`. Each element
#' has `dataset_id`, nested `spd`, and selected `distance` columns.
#' @details
#' The fallback selects 250 km whenever its series is available. It selects
#' 500 km otherwise, including when both radii are explicitly unavailable.
#' Availability diagnostics remain in the source matched-radius product.
#' @examples
#' \dontrun{
#' products <- prepare_spd_analysis_products(data_spd_by_radius)
#' }
prepare_spd_analysis_products <- function(data_spd_by_radius) {
  assertthat::assert_that(
    is.data.frame(data_spd_by_radius),
    all(
      c("dataset_id", "radius_km", "spd", "available") %in%
        names(data_spd_by_radius)
    ),
    is.list(data_spd_by_radius[["spd"]]),
    all(purrr::map_lgl(data_spd_by_radius[["spd"]], is.data.frame)),
    all(purrr::map_lgl(
      data_spd_by_radius[["spd"]],
      ~ all(c("age", "value") %in% names(.x))
    )),
    is.logical(data_spd_by_radius[["available"]]),
    !anyNA(data_spd_by_radius[["available"]]),
    all(data_spd_by_radius[["radius_km"]] %in% c(250L, 500L)),
    !anyDuplicated(
      data_spd_by_radius[c("dataset_id", "radius_km")]
    ),
    msg = "Matched SPD radius data do not satisfy the product contract."
  )

  data_radius_counts <-
    data_spd_by_radius |>
    dplyr::count(.data[["dataset_id"]], name = "n_radii")

  if (
    !setequal(
      unique(data_spd_by_radius[["radius_km"]]),
      c(250L, 500L)
    ) ||
      any(data_radius_counts[["n_radii"]] != 2L)
  ) {
    cli::cli_abort(
      "Every dataset must contain one 250 km and one 500 km SPD series."
    )
  }

  data_strict_250 <-
    data_spd_by_radius |>
    dplyr::filter(.data[["radius_km"]] == 250L) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd = .data[["spd"]],
      distance = as.numeric(.data[["radius_km"]])
    ) |>
    dplyr::arrange(.data[["dataset_id"]])

  data_strict_500 <-
    data_spd_by_radius |>
    dplyr::filter(.data[["radius_km"]] == 500L) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd = .data[["spd"]],
      distance = as.numeric(.data[["radius_km"]])
    ) |>
    dplyr::arrange(.data[["dataset_id"]])

  data_radius_choice <-
    data_spd_by_radius |>
    dplyr::select(
      dplyr::all_of(c("dataset_id", "radius_km", "available"))
    ) |>
    tidyr::pivot_wider(
      names_from = "radius_km",
      values_from = "available",
      names_prefix = "available_"
    ) |>
    dplyr::mutate(
      selected_radius_km = dplyr::if_else(
        .data[["available_250"]],
        250L,
        500L
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c("dataset_id", "selected_radius_km"))
    )

  data_fallback <-
    data_spd_by_radius |>
    dplyr::inner_join(
      data_radius_choice,
      by = "dataset_id",
      relationship = "many-to-one"
    ) |>
    dplyr::filter(
      .data[["radius_km"]] == .data[["selected_radius_km"]]
    ) |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd = .data[["spd"]],
      distance = as.numeric(.data[["radius_km"]])
    ) |>
    dplyr::arrange(.data[["dataset_id"]])

  res_products <-
    list(
      data_spd_strict_250 = data_strict_250,
      data_spd_strict_500 = data_strict_500,
      data_spd_250_with_500_fallback = data_fallback
    )

  return(res_products)
}
