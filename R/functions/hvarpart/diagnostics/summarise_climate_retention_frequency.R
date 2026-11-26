#' @title Summarise how often each climate predictor is retained
#' @description
#' Count, by region and overall, the share of analytical units in which each
#' climate candidate survives the shared response-free climate selection.
#' @param data_climate_selection
#' Output of `select_shared_climate_predictors()` with an added `region`
#' column.
#' @param climate_candidates
#' Character vector of climate candidates.
#' @param group_columns
#' Character vector of further grouping columns (for example the window);
#' may be empty.
#' @return
#' A tibble with the grouping columns, `region` (each region and `"All"`),
#' `predictor`, `n_units`, `n_retained`, and `share_retained`.
#' @details
#' Every unit counts, including units whose selection kept no climate
#' predictor.
#' @examples
#' \dontrun{
#' summarise_climate_retention_frequency(selection, c("a", "b"), "window_id")
#' }
summarise_climate_retention_frequency <- function(
  data_climate_selection,
  climate_candidates,
  group_columns = character()
) {
  assertthat::assert_that(
    is.data.frame(data_climate_selection),
    all(c(group_columns, "region", "climate_predictors") %in%
      names(data_climate_selection)),
    is.list(data_climate_selection[["climate_predictors"]]),
    is.character(climate_candidates),
    length(climate_candidates) > 0L,
    msg = "Climate-retention inputs do not satisfy the contract."
  )

  data_long <-
    data_climate_selection |>
    dplyr::mutate(
      region = as.character(.data[["region"]]),
      .unit_index = dplyr::row_number()
    ) |>
    dplyr::select(
      dplyr::all_of(c(
        group_columns, "region", ".unit_index", "climate_predictors"
      ))
    ) |>
    tidyr::expand_grid(predictor = climate_candidates) |>
    dplyr::mutate(
      is_retained = purrr::map2_lgl(
        .data[["predictor"]],
        .data[["climate_predictors"]],
        .f = ~ .x %in% .y
      )
    )

  data_all_regions <-
    data_long |>
    dplyr::mutate(region = "All")

  res <-
    dplyr::bind_rows(data_long, data_all_regions) |>
    dplyr::summarise(
      n_units = dplyr::n(),
      n_retained = sum(.data[["is_retained"]]),
      .by = dplyr::all_of(c(group_columns, "region", "predictor"))
    ) |>
    dplyr::mutate(
      n_units = as.integer(.data[["n_units"]]),
      n_retained = as.integer(.data[["n_retained"]]),
      share_retained = .data[["n_retained"]] / .data[["n_units"]]
    ) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(c(group_columns, "region", "predictor")))
    )

  return(res)
}
