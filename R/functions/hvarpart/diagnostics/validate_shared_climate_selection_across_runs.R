#' @title Validate that every run reuses the shared climate selection
#' @description
#' Check that each human-proxy run used, in every analytical unit, exactly the
#' climate set selected once for that unit, so runs sharing a unit have
#' identical climate blocks. Stops with an error otherwise.
#' @param data_used
#' Data frame with columns `profile_id`, the unit columns, and `climate_set`
#' (the climate block used by each run in each unit).
#' @param data_shared
#' Data frame with the unit columns and `climate_set` (the shared selection).
#' @param unit_columns
#' Character vector of columns identifying an analytical unit.
#' @return
#' A tibble with one row per unit used by at least one run: the unit columns,
#' `n_runs`, `climate_set`, and `identical_climate_set` (always `TRUE` when
#' the function returns).
#' @details
#' Missing climate sets (`NA`, no climate retained) count as equal. The
#' function stops with an error when a used unit has no shared selection or
#' when any run's climate set differs from the shared one.
#' @examples
#' \dontrun{
#' validate_shared_climate_selection_across_runs(used, shared, "dataset_id")
#' }
validate_shared_climate_selection_across_runs <- function(
  data_used,
  data_shared,
  unit_columns
) {
  assertthat::assert_that(
    is.data.frame(data_used),
    all(c("profile_id", unit_columns, "climate_set") %in% names(data_used)),
    is.data.frame(data_shared),
    all(c(unit_columns, "climate_set") %in% names(data_shared)),
    !anyDuplicated(data_shared[unit_columns]),
    msg = "Shared-climate check inputs do not satisfy the contract."
  )

  data_compared <-
    data_used |>
    dplyr::select(
      dplyr::all_of(c("profile_id", unit_columns, "climate_set"))
    ) |>
    dplyr::left_join(
      data_shared |>
        dplyr::select(dplyr::all_of(c(unit_columns, "climate_set"))) |>
        dplyr::rename(shared_climate_set = dplyr::all_of("climate_set")) |>
        dplyr::mutate(.has_shared = TRUE),
      by = unit_columns,
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      is_identical = .data[[".has_shared"]] %in% TRUE &
        dplyr::coalesce(.data[["climate_set"]], "<none>") ==
          dplyr::coalesce(.data[["shared_climate_set"]], "<none>")
    )

  data_mismatches <-
    data_compared |>
    dplyr::filter(!.data[["is_identical"]])

  if (
    nrow(data_mismatches) > 0L
  ) {
    cli::cli_abort(
      c(
        "Climate sets used by some runs differ from the shared selection.",
        "x" = "{nrow(data_mismatches)} run-unit combination{?s} differ."
      )
    )
  }

  res <-
    data_compared |>
    dplyr::summarise(
      n_runs = dplyr::n_distinct(.data[["profile_id"]]),
      climate_set = dplyr::first(.data[["shared_climate_set"]]),
      identical_climate_set = all(.data[["is_identical"]]),
      .by = dplyr::all_of(unit_columns)
    ) |>
    dplyr::mutate(n_runs = as.integer(.data[["n_runs"]]))

  return(res)
}
