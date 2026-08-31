#' @title Fit time control for all within-dataset HVarPart models
#' @description Apply `fit_temporal_hvarpart_dataset()` reproducibly to a
#' nested dataset data set and retain failures as explicit result statuses.
#' @param data_source Nested dataset data with identifier and data columns.
#' @param response_vars Response variable names.
#' @param predictor_vars Named human and climate predictor groups, or a
#'   region-aware resolver function.
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
    is.list(predictor_vars) || is.function(predictor_vars),
    !is.function(predictor_vars) || "region" %in% names(data_source),
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
      .f = function(index) {
        data_dataset <- data_source[[data_col]][[index]]
        region <-
          if ("region" %in% names(data_source)) {
            data_source[["region"]][[index]]
          } else {
            NA_character_
          }
        selected_predictors <- resolve_hvarpart_predictor_vars(
          predictor_vars = predictor_vars,
          region = region,
          available_columns = names(data_dataset)
        )

        rlang::exec(
          .fn = safe_analysis,
          data_dataset = data_dataset,
          response_vars = response_vars,
          predictor_vars = selected_predictors,
          seed = as.integer(seed + index),
          !!!list_arguments
        )
      }
    )
  res_data <-
    tibble::tibble(
      !!id_col := data_source[[id_col]],
      result = list_results
    )

  return(res_data)
}
