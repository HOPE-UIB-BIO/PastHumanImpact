#' @title Fit spatial control across region-age groups
#' @description Apply spatially explicit HVarPart analysis to every nested
#' region-age group with reproducible group-specific seeds.
#' @param data_source Region-age data with a nested data column.
#' @param analysis Analysis identifier.
#' @param response_vars Response variable names.
#' @param predictor_vars Named human and climate predictor groups, or a
#'   region-aware resolver function.
#' @param data_col Nested data column.
#' @param seed Base integer random seed.
#' @param ... Arguments passed to `fit_spatial_hvarpart_group()`.
#' @return A tibble with analysis, region, age, and result columns.
#' @examples
#' \dontrun{
#' fit_spatial_hvarpart_dataset(
#'   data_source = time_bins,
#'   analysis = "temporal_spd",
#'   response_vars = c("n0", "n1"),
#'   predictor_vars = list(human = "spd", climate = "temperature")
#' )
#' }
fit_spatial_hvarpart_dataset <- function(
  data_source,
  analysis,
  response_vars,
  predictor_vars,
  data_col = "data_merge",
  seed = 1234L,
  ...
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c("region", "age", data_col) %in% names(data_source)),
    is.list(data_source[[data_col]]),
    is.list(predictor_vars) || is.function(predictor_vars),
    assertthat::is.string(analysis),
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Spatial HVarPart dataset inputs do not satisfy the contract."
  )

  empty_result <-
    build_empty_spatial_hvarpart_result(
      status = "model_error",
      n_samples = 0L
    )
  safe_analysis <-
    purrr::possibly(
      .f = fit_spatial_hvarpart_group,
      otherwise = empty_result,
      quiet = TRUE
    )
  list_arguments <- rlang::list2(...)
  list_results <-
    seq_len(nrow(data_source)) |>
    purrr::map(
      .f = function(index) {
        data_group <- data_source[[data_col]][[index]]
        selected_predictors <- resolve_hvarpart_predictor_vars(
          predictor_vars = predictor_vars,
          region = data_source[["region"]][[index]],
          available_columns = names(data_group)
        )

        rlang::exec(
          .fn = safe_analysis,
          data_group = data_group,
          response_vars = response_vars,
          predictor_vars = selected_predictors,
          seed = as.integer(
            seed + data_source[["age"]][index] + index
          ),
          !!!list_arguments
        )
      }
    )
  res_data <-
    data_source |>
    dplyr::select(dplyr::all_of(c("region", "age"))) |>
    dplyr::mutate(
      analysis = analysis,
      result = list_results,
      .before = 1L
    )

  return(res_data)
}
