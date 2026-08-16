#' @title Summarise spatially controlled region-age results
#' @description Convert nested spatial HVarPart results into status,
#' component, partial-fraction, Moran, and remaining-signal tables.
#' @param data_results Output from `fit_spatial_hvarpart_dataset()`.
#' @return A named list of machine-readable result tables.
#' @examples
#' \dontrun{
#' summarise_spatial_hvarpart_results(spatial_results)
#' }
summarise_spatial_hvarpart_results <- function(data_results) {
  keys <- c("analysis", "region", "age")
  assertthat::assert_that(
    is.data.frame(data_results),
    all(c(keys, "result") %in% names(data_results)),
    is.list(data_results[["result"]]),
    msg = "Spatial result extraction inputs do not satisfy the contract."
  )

  data_status <-
    data_results |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      status = purrr::map_chr(.data[["result"]], ~ .x[["status"]]),
      n_samples = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["n_samples"]])
      ),
      selection_status = purrr::map_chr(
        .data[["result"]],
        ~ .x[["selection"]][["status"]]
      ),
      n_candidates = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["selection"]][["n_candidates"]])
      ),
      n_selected = purrr::map_int(
        .data[["result"]],
        ~ length(.x[["selection"]][["selected_names"]])
      ),
      global_p_value = purrr::map_dbl(
        .data[["result"]],
        ~ as.numeric(.x[["selection"]][["global_p_value"]])
      ),
      spatial_adjusted_r_squared = purrr::map_dbl(
        .data[["result"]],
        ~ as.numeric(.x[["selection"]][["full_adjusted_r_squared"]])
      )
    )
  data_selection <-
    data_results |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      selection_status = purrr::map_chr(
        .data[["result"]],
        ~ .x[["selection"]][["status"]]
      ),
      n_complete = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["selection"]][["n_complete"]])
      ),
      n_candidates = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["selection"]][["n_candidates"]])
      ),
      global_p_value = purrr::map_dbl(
        .data[["result"]],
        ~ as.numeric(.x[["selection"]][["global_p_value"]])
      ),
      full_adjusted_r_squared = purrr::map_dbl(
        .data[["result"]],
        ~ as.numeric(
          .x[["selection"]][["full_adjusted_r_squared"]]
        )
      ),
      selected_terms = purrr::map_chr(
        .data[["result"]],
        ~ stringr::str_c(
          .x[["selection"]][["selected_names"]],
          collapse = ";"
        )
      )
    )
  data_dbmem <-
    data_results |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      dbmem = purrr::map(
        .data[["result"]],
        ~ .x[["dbmem"]][["diagnostics"]]
      )
    ) |>
    tidyr::unnest(cols = "dbmem")
  data_components <-
    data_results |>
    dplyr::mutate(
      components = purrr::map(
        .x = .data[["result"]],
        .f = prepare_hvarpart_control_components,
        controlled_result_name = "spatial_hvarpart",
        controlled_profile = "human_climate_space"
      )
    ) |>
    dplyr::select(dplyr::all_of(c(keys, "components"))) |>
    tidyr::unnest(cols = "components")
  data_unique_adjusted_r2 <-
    data_results |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      partial = purrr::map(
        .data[["result"]],
        ~ .x[["unique_adjusted_r2"]]
      )
    ) |>
    tidyr::unnest(cols = "partial")
  data_moran <-
    data_results |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      moran = purrr::map(
        .data[["result"]],
        ~ .x[["residual_moran"]]
      )
    ) |>
    tidyr::unnest(cols = "moran")
  data_remaining <-
    data_results |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      remaining = purrr::map(
        .data[["result"]],
        ~ .x[["remaining_spatial_test"]]
      )
    ) |>
    tidyr::unnest(cols = "remaining")

  return(
    list(
      status = data_status,
      selection = data_selection,
      dbmem_diagnostics = data_dbmem,
      components = data_components,
      unique_adjusted_r2 = data_unique_adjusted_r2,
      residual_moran = data_moran,
      remaining_spatial_test = data_remaining
    )
  )
}
