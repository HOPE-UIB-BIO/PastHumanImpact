#' @title Select units estimable in every compared run
#' @description
#' Keep the analytical units that are present (estimable) in every required
#' run, so a comparison between runs uses identical units.
#' @param data_units
#' Data frame with the group column and the unit columns, one row per run
#' and estimable unit.
#' @param unit_columns
#' Character vector of columns identifying an analytical unit.
#' @param required_groups
#' Character vector of runs that must all contain a unit.
#' @param group_column
#' Column identifying the run.
#' @return
#' A tibble with the unit columns and one row per shared unit, sorted by the
#' unit columns.
#' @details
#' The function stops with an error when a required run has no row at all,
#' because an empty run would silently empty the comparison.
#' @examples
#' \dontrun{
#' select_shared_estimable_units(records, "dataset_id", c("spd", "kk10"))
#' }
select_shared_estimable_units <- function(
  data_units,
  unit_columns,
  required_groups,
  group_column = "profile_id"
) {
  assertthat::assert_that(
    is.data.frame(data_units),
    is.character(unit_columns),
    length(unit_columns) > 0L,
    assertthat::is.string(group_column),
    all(c(group_column, unit_columns) %in% names(data_units)),
    is.character(required_groups),
    length(required_groups) > 0L,
    all(required_groups %in% data_units[[group_column]]),
    msg = "Shared-unit inputs do not satisfy the contract."
  )

  res <-
    data_units |>
    dplyr::filter(.data[[group_column]] %in% required_groups) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(group_column, unit_columns)))
    ) |>
    dplyr::count(
      dplyr::across(dplyr::all_of(unit_columns)),
      name = ".n_groups"
    ) |>
    dplyr::filter(.data[[".n_groups"]] == length(unique(required_groups))) |>
    dplyr::select(dplyr::all_of(unit_columns)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(unit_columns)))

  return(res)
}
