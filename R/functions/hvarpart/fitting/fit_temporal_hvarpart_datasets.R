#' @title Fit time control for all within-dataset HVarPart models
#' @description Apply `fit_temporal_hvarpart_dataset()` reproducibly to a
#' nested dataset data set and retain failures as explicit result statuses.
#' @param data_source Nested dataset data with identifier and data columns.
#' @param response_vars Response variable names.
#' @param predictor_vars Named human and climate predictor groups.
#' @param id_col Core identifier column.
#' @param data_col Nested data column.
#' @param seed Base integer random seed.
#' @param ... Arguments passed to `fit_temporal_hvarpart_dataset()`.
#' @return A tibble with one dataset identifier and one result list-column.
#' @examples
#' \dontrun{
#' fit_temporal_hvarpart_datasets(
#'   data_source = nested_dataset_data,
#'   response_vars = c("n0", "n1"),
#'   predictor_vars = list(human = "spd", climate = "temperature")
#' )
#' }
fit_temporal_hvarpart_datasets <- function(
  data_source,
  response_vars,
  predictor_vars,
  id_col = "dataset_id",
  data_col = "data_merge",
  seed = 1234L,
  ...
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c(id_col, data_col) %in% names(data_source)),
    is.list(data_source[[data_col]]),
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Temporal HVarPart dataset inputs do not satisfy the contract."
  )

  safe_analysis <-
    purrr::possibly(
      .f = fit_temporal_hvarpart_dataset,
      otherwise = list(
        status = "model_error",
        n_samples = NA_integer_,
        n_unique_ages = NA_integer_,
        design_rank = NA_integer_,
        design_full_rank = NA,
        residual_df = NA_integer_,
        human_climate_only_hvarpart = NULL,
        temporal_hvarpart = NULL,
        unique_adjusted_r2 = tibble::tibble(),
        residual_moran = tibble::tibble()
      ),
      quiet = TRUE
    )
  list_arguments <- rlang::list2(...)
  list_results <-
    seq_len(nrow(data_source)) |>
    purrr::map(
      .f = ~ rlang::exec(
        .fn = safe_analysis,
        data_dataset = data_source[[data_col]][[.x]],
        response_vars = response_vars,
        predictor_vars = predictor_vars,
        seed = as.integer(seed + .x),
        !!!list_arguments
      )
    )
  res_data <-
    tibble::tibble(
      !!id_col := data_source[[id_col]],
      result = list_results
    )

  return(res_data)
}
