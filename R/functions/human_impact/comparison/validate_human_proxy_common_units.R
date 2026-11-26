#' @title Validate that common-cohort runs share identical units
#' @description
#' Check that every common-cohort human-proxy run was given exactly the same
#' analytical units, so the proxy is the only thing that differs between
#' them. Stops with an error otherwise.
#' @param data_run_units
#' Data frame with the group column and the unit columns, one row per run
#' and unit.
#' @param unit_columns
#' Character vector of columns identifying an analytical unit.
#' @param group_column
#' Column identifying the run.
#' @return
#' A tibble with one row per run and columns named by `group_column`,
#' `n_units`, and `identical_units` (always `TRUE` when the function
#' returns).
#' @details
#' Unit sets are compared after removing duplicate rows within a run.
#' @examples
#' \dontrun{
#' validate_human_proxy_common_units(run_units, "dataset_id")
#' }
validate_human_proxy_common_units <- function(
  data_run_units,
  unit_columns,
  group_column = "profile_id"
) {
  assertthat::assert_that(
    is.data.frame(data_run_units),
    is.character(unit_columns),
    length(unit_columns) > 0L,
    assertthat::is.string(group_column),
    all(c(group_column, unit_columns) %in% names(data_run_units)),
    nrow(data_run_units) > 0L,
    msg = "Common-unit check inputs do not satisfy the contract."
  )

  data_keys <-
    data_run_units |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(group_column, unit_columns)))
    ) |>
    tidyr::unite(
      col = ".unit_key",
      dplyr::all_of(unit_columns),
      sep = "\r"
    )

  list_keys <-
    split(data_keys[[".unit_key"]], data_keys[[group_column]]) |>
    purrr::map(sort)

  vec_reference <-
    list_keys[[1]]

  res <-
    tibble::tibble(
      !!group_column := names(list_keys),
      n_units = unname(purrr::map_int(list_keys, length)),
      identical_units = unname(
        purrr::map_lgl(
          list_keys,
          .f = ~ identical(.x, vec_reference)
        )
      )
    )

  vec_differing <-
    res |>
    dplyr::filter(!.data[["identical_units"]]) |>
    dplyr::pull(dplyr::all_of(group_column))

  if (
    length(vec_differing) > 0L
  ) {
    cli::cli_abort(
      c(
        "Common-cohort runs do not share identical units.",
        "x" = "Runs differing from {.val {names(list_keys)[1]}}:
          {.val {vec_differing}}."
      )
    )
  }

  return(res)
}
