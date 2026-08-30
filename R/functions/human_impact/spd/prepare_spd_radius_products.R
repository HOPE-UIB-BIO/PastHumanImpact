#' @title Prepare matched SPD radius products
#' @description
#' Convert one wide nested SPD table per dataset into one explicit row per
#' dataset and radius with availability diagnostics.
#' @param data_source Wide SPD data containing `dataset_id` and nested `spd`
#'   tables with `age` and one column per radius.
#' @return Tibble with dataset, radius, nested age-value series, and coverage
#'   diagnostics.
#' @examples
#' \dontrun{
#' products <- prepare_spd_radius_products(data_spd_wide)
#' }
prepare_spd_radius_products <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c("dataset_id", "spd") %in% names(data_source)),
    is.list(data_source[["spd"]]),
    nrow(data_source) > 0L,
    !anyDuplicated(data_source[["dataset_id"]]),
    all(purrr::map_lgl(data_source[["spd"]], is.data.frame)),
    all(purrr::map_lgl(
      data_source[["spd"]],
      ~ "age" %in% names(.x)
    )),
    msg = "Wide SPD radius products do not satisfy the required contract."
  )

  res_products <-
    purrr::map2(
      .x = data_source[["dataset_id"]],
      .y = data_source[["spd"]],
      .f = ~ .y |>
        tidyr::pivot_longer(
          cols = -dplyr::all_of("age"),
          names_to = "radius_km",
          values_to = "value"
        ) |>
        dplyr::mutate(
          radius_km = as.integer(.data[["radius_km"]])
        ) |>
        dplyr::group_by(.data[["radius_km"]]) |>
        tidyr::nest(spd = c("age", "value")) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          dataset_id = .x,
          n_time_points = purrr::map_int(
            .data[["spd"]],
            nrow
          ),
          n_finite_values = purrr::map_int(
            .data[["spd"]],
            ~ sum(is.finite(.x[["value"]]))
          ),
          n_nonzero_values = purrr::map_int(
            .data[["spd"]],
            ~ sum(
              is.finite(.x[["value"]]) & .x[["value"]] != 0
            )
          ),
          available = .data[["n_nonzero_values"]] > 0L,
          availability_status = dplyr::if_else(
            .data[["available"]],
            "available",
            "unavailable"
          ),
          .before = 1L
        )
    ) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data[["dataset_id"]], .data[["radius_km"]])

  return(res_products)
}
