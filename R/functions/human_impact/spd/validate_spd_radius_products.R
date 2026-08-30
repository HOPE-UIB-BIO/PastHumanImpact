#' @title Validate matched SPD radius products
#' @description
#' Enforce unique dataset-radius keys, common within-dataset age grids, and the
#' expected nested-radius availability relationship.
#' @param data_spd Radius-specific SPD product table.
#' @param expected_radii Integer radii required for every dataset.
#' @return Invisibly returns `data_spd` when the contract is satisfied.
#' @examples
#' \dontrun{
#' validate_spd_radius_products(products, c(250L, 500L))
#' }
validate_spd_radius_products <- function(
  data_spd,
  expected_radii = c(250L, 500L)
) {
  required_columns <-
    c(
      "dataset_id",
      "radius_km",
      "spd",
      "n_time_points",
      "n_finite_values",
      "n_nonzero_values",
      "available",
      "availability_status"
    )

  assertthat::assert_that(
    is.data.frame(data_spd),
    all(required_columns %in% names(data_spd)),
    is.numeric(expected_radii),
    length(expected_radii) > 0L,
    all(is.finite(expected_radii)),
    !anyDuplicated(expected_radii),
    msg = "SPD radius validation inputs do not satisfy the contract."
  )

  data_keys <-
    data_spd |>
    dplyr::transmute(
      key = stringr::str_c(
        .data[["dataset_id"]],
        .data[["radius_km"]],
        sep = "|"
      )
    )

  if (
    anyDuplicated(data_keys[["key"]]) > 0L
  ) {
    cli::cli_abort("SPD radius products contain duplicate dataset-radius keys.")
  }

  if (
    !setequal(unique(data_spd[["radius_km"]]), expected_radii)
  ) {
    cli::cli_abort("SPD radius products do not contain the expected radii.")
  }

  data_radius_counts <-
    data_spd |>
    dplyr::count(.data[["dataset_id"]], name = "n_radii")

  if (
    any(data_radius_counts[["n_radii"]] != length(expected_radii))
  ) {
    cli::cli_abort("Every dataset must contain every expected SPD radius.")
  }

  if (
    !all(purrr::map_lgl(
      data_spd[["spd"]],
      ~ is.data.frame(.x) && identical(names(.x), c("age", "value"))
    ))
  ) {
    cli::cli_abort("Every SPD radius series must contain age and value columns.")
  }

  if (
    any(
      data_spd[["availability_status"]] !=
        dplyr::if_else(
          data_spd[["available"]],
          "available",
          "unavailable"
        )
    )
  ) {
    cli::cli_abort("SPD availability labels disagree with valid-signal flags.")
  }

  data_with_age_grids <-
    data_spd |>
    dplyr::mutate(
      age_grid_hash = purrr::map_chr(
        .data[["spd"]],
        ~ rlang::hash(.x[["age"]])
      )
    )
  data_age_grids <-
    data_with_age_grids |>
    dplyr::group_by(.data[["dataset_id"]]) |>
    dplyr::summarise(
      n_age_grids = dplyr::n_distinct(.data[["age_grid_hash"]]),
      .groups = "drop"
    )

  if (
    any(data_age_grids[["n_age_grids"]] != 1L)
  ) {
    cli::cli_abort("SPD radii use different age grids within a dataset.")
  }

  if (
    dplyr::n_distinct(data_with_age_grids[["age_grid_hash"]]) != 1L
  ) {
    cli::cli_abort("SPD radius products do not share one common age grid.")
  }

  if (
    all(c(250L, 500L) %in% expected_radii)
  ) {
    data_availability <-
      data_spd |>
      dplyr::filter(.data[["radius_km"]] %in% c(250L, 500L)) |>
      dplyr::select(
        dplyr::all_of(c("dataset_id", "radius_km", "available"))
      ) |>
      tidyr::pivot_wider(
        names_from = "radius_km",
        values_from = "available",
        names_prefix = "radius_"
      )

    if (
      any(
        data_availability[["radius_250"]] &
          !data_availability[["radius_500"]]
      )
    ) {
      cli::cli_abort(
        "A valid 250 km SPD cannot be unavailable at 500 km."
      )
    }
  }

  return(invisible(data_spd))
}
