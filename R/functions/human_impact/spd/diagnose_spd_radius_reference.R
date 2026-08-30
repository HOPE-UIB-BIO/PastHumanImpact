#' @title Compare an SPD radius with a reference artifact
#' @description
#' Compare recalculated nested SPD series with a historical radius-specific
#' artifact without changing either product.
#' @param data_spd Radius-specific SPD products.
#' @param data_reference Historical data with `dataset_id` and nested `spd`.
#' @param radius_km Radius to compare.
#' @param tolerance Maximum accepted absolute numeric difference.
#' @return Dataset-level comparison table.
#' @examples
#' \dontrun{
#' audit <- diagnose_spd_radius_reference(products, old_spd, 250L)
#' }
diagnose_spd_radius_reference <- function(
  data_spd,
  data_reference,
  radius_km,
  tolerance = sqrt(.Machine$double.eps)
) {
  assertthat::assert_that(
    is.data.frame(data_spd),
    all(c("dataset_id", "radius_km", "spd") %in% names(data_spd)),
    is.data.frame(data_reference),
    all(c("dataset_id", "spd") %in% names(data_reference)),
    is.numeric(radius_km),
    length(radius_km) == 1L,
    is.finite(radius_km),
    is.numeric(tolerance),
    length(tolerance) == 1L,
    is.finite(tolerance),
    tolerance >= 0,
    msg = "SPD reference comparison inputs do not satisfy the contract."
  )

  radius_column <-
    as.character(as.integer(radius_km))

  data_new <-
    data_spd |>
    dplyr::filter(.data[["radius_km"]] == .env$radius_km) |>
    dplyr::select(
      dplyr::all_of("dataset_id"),
      spd_new = dplyr::all_of("spd")
    )

  data_old <-
    data_reference |>
    dplyr::transmute(
      dataset_id = .data[["dataset_id"]],
      spd_reference = purrr::map(
        .data[["spd"]],
        ~ .x |>
          dplyr::transmute(
            age = .data[["age"]],
            value = .data[[radius_column]]
          )
      )
    )

  data_paired <-
    dplyr::full_join(
      data_new,
      data_old,
      by = "dataset_id"
    )

  res_comparison <-
    data_paired |>
    dplyr::mutate(
      present_new = !purrr::map_lgl(.data[["spd_new"]], is.null),
      present_reference =
        !purrr::map_lgl(.data[["spd_reference"]], is.null),
      identical_age_grid = purrr::map2_lgl(
        .data[["spd_new"]],
        .data[["spd_reference"]],
        ~ !is.null(.x) &&
          !is.null(.y) &&
          identical(.x[["age"]], .y[["age"]])
      ),
      max_absolute_difference = purrr::map2_dbl(
        .data[["spd_new"]],
        .data[["spd_reference"]],
        ~ {
          if (
            is.null(.x) || is.null(.y) ||
              !identical(.x[["age"]], .y[["age"]])
          ) {
            return(NA_real_)
          }

          max(abs(.x[["value"]] - .y[["value"]]), na.rm = TRUE)
        }
      ),
      within_tolerance =
        .data[["present_new"]] &
        .data[["present_reference"]] &
        .data[["identical_age_grid"]] &
        is.finite(.data[["max_absolute_difference"]]) &
        .data[["max_absolute_difference"]] <= tolerance
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        "dataset_id",
        "present_new",
        "present_reference",
        "identical_age_grid",
        "max_absolute_difference",
        "within_tolerance"
      ))
    )

  return(res_comparison)
}
