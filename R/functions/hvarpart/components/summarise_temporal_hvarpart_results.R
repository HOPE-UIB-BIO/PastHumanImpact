#' @title Summarise time-controlled HVarPart result tables
#' @description Convert nested within-dataset results into status, component,
#' partial-fraction, and temporal-Moran tables.
#' @param data_results Output from `fit_temporal_hvarpart_datasets()`.
#' @param analysis Analysis identifier.
#' @param id_col Core identifier column.
#' @return A named list of four machine-readable tables.
#' @examples
#' \dontrun{
#' summarise_temporal_hvarpart_results(results, analysis = "spatial_spd")
#' }
summarise_temporal_hvarpart_results <- function(
  data_results,
  analysis,
  id_col = "dataset_id"
) {
  assertthat::assert_that(
    is.data.frame(data_results),
    all(c(id_col, "result") %in% names(data_results)),
    is.list(data_results[["result"]]),
    assertthat::is.string(analysis),
    msg = "Temporal extraction inputs do not satisfy the contract."
  )

  data_status <-
    data_results |>
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      analysis = analysis,
      status = purrr::map_chr(.data[["result"]], ~ .x[["status"]]),
      n_samples = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["n_samples"]])
      ),
      n_unique_ages = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["n_unique_ages"]])
      ),
      design_rank = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["design_rank"]])
      ),
      design_full_rank = purrr::map_lgl(
        .data[["result"]],
        ~ as.logical(.x[["design_full_rank"]])
      ),
      residual_df = purrr::map_int(
        .data[["result"]],
        ~ as.integer(.x[["residual_df"]])
      )
    )
  data_components <-
    data_results |>
    dplyr::mutate(
      components = purrr::map(
        .x = .data[["result"]],
        .f = ~ {
          data_human_climate_only <-
            if (
              is.null(.x[["human_climate_only_hvarpart"]])
            ) {
              tibble::tibble()
            } else {
              .x[["human_climate_only_hvarpart"]][["summary_table"]] |>
                dplyr::mutate(
                  model_profile = "human_climate",
                  total_adjusted_r_squared =
                    .x[["human_climate_only_hvarpart"]][["varhp_output"]][[
                      "Total_explained_variation"
                    ]]
                )
            }
          data_temporal <-
            if (
              is.null(.x[["temporal_hvarpart"]])
            ) {
              tibble::tibble()
            } else {
              .x[["temporal_hvarpart"]][["summary_table"]] |>
                dplyr::mutate(
                  model_profile = "human_climate_time",
                  total_adjusted_r_squared =
                    .x[["temporal_hvarpart"]][["varhp_output"]][[
                      "Total_explained_variation"
                    ]]
                )
            }

          return(dplyr::bind_rows(data_human_climate_only, data_temporal))
        }
      )
    ) |>
    dplyr::select(dplyr::all_of(c(id_col, "components"))) |>
    tidyr::unnest(cols = "components") |>
    dplyr::mutate(analysis = analysis, .after = dplyr::all_of(id_col))
  data_unique_adjusted_r2 <-
    data_results |>
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      partial = purrr::map(
        .data[["result"]],
        ~ .x[["unique_adjusted_r2"]]
      )
    ) |>
    tidyr::unnest(cols = "partial") |>
    dplyr::mutate(analysis = analysis, .after = dplyr::all_of(id_col))
  data_moran <-
    data_results |>
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      moran = purrr::map(
        .data[["result"]],
        ~ .x[["residual_moran"]]
      )
    ) |>
    tidyr::unnest(cols = "moran") |>
    dplyr::mutate(analysis = analysis, .after = dplyr::all_of(id_col))

  return(
    list(
      status = data_status,
      components = data_components,
      unique_adjusted_r2 = data_unique_adjusted_r2,
      residual_moran = data_moran
    )
  )
}
