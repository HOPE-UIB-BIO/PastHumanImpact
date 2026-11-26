#' @title Select climate predictors once per analytical unit
#' @description
#' Apply the response-free within-group collinearity filter to the climate
#' candidates of every analytical unit, before any human proxy is attached,
#' so every human-proxy model of a unit can reuse the same climate block.
#' @param data_units
#' Data frame with the unit identifier columns and a list column of unit
#' data.
#' @param unit_columns
#' Character vector of columns identifying an analytical unit.
#' @param data_col
#' Name of the list column holding each unit's data frame.
#' @param climate_candidates
#' Character vector of climate candidates in preference order.
#' @param max_cor
#' Maximum absolute within-group correlation.
#' @param max_vif
#' Maximum within-group variance inflation factor.
#' @return
#' A tibble with the unit columns and `climate_status` (the status of
#' `select_local_hvarpart_predictor_group()`), `climate_predictors` (list of
#' retained climate columns in preference order), `climate_set` (retained
#' columns sorted and joined with `" + "`, `NA` when none is retained),
#' `selection_error`, and `selection_audit` (list of column-level audits).
#' @details
#' Each unit is passed to `select_local_hvarpart_predictor_group()` with
#' `group_name = "climate"`; only the climate columns are read. The function
#' stops with an error when the unit identifiers are not unique.
#' @examples
#' \dontrun{
#' select_shared_climate_predictors(units, c("window_id", "dataset_id"))
#' }
select_shared_climate_predictors <- function(
  data_units,
  unit_columns,
  data_col = "data_merge",
  climate_candidates = c(
    "temp_annual", "temp_cold", "prec_summer", "prec_win"
  ),
  max_cor = 0.8,
  max_vif = 5
) {
  assertthat::assert_that(
    is.data.frame(data_units),
    is.character(unit_columns),
    length(unit_columns) > 0L,
    all(c(unit_columns, data_col) %in% names(data_units)),
    is.list(data_units[[data_col]]),
    !anyDuplicated(data_units[unit_columns]),
    is.character(climate_candidates),
    length(climate_candidates) > 0L,
    msg = "Shared climate-selection inputs do not satisfy the contract."
  )

  list_selections <-
    data_units[[data_col]] |>
    purrr::map(
      .f = ~ select_local_hvarpart_predictor_group(
        data_source = .x,
        candidates = climate_candidates,
        preference = climate_candidates,
        group_name = "climate",
        max_cor = max_cor,
        max_vif = max_vif
      )
    )

  res <-
    data_units |>
    dplyr::select(dplyr::all_of(unit_columns)) |>
    dplyr::mutate(
      climate_status = purrr::map_chr(list_selections, "status"),
      climate_predictors = purrr::map(list_selections, "selected"),
      climate_set = purrr::map_chr(
        .data[["climate_predictors"]],
        .f = ~ {
          if (
            length(.x) == 0L
          ) {
            NA_character_
          } else {
            stringr::str_c(sort(.x), collapse = " + ")
          }
        }
      ),
      selection_error = purrr::map_chr(list_selections, "error_message"),
      selection_audit = purrr::map(list_selections, "audit")
    )

  return(res)
}
