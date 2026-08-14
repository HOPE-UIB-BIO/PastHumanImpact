#' @title Fit one time-controlled within-dataset HVarPart model
#' @description
#' Fit human_climate_only human-climate and time-controlled HVarPart models, calculate
#' pure fractions, and diagnose residual temporal autocorrelation.
#' @param data_dataset One dataset-level data frame with unique ages.
#' @param response_vars Response variable names.
#' @param predictor_vars Named human and climate predictor groups.
#' @param age_col Age column.
#' @param min_unique_ages Minimum unique ages.
#' @param min_residual_df Minimum residual degrees of freedom.
#' @param distance_years Temporal Moran thresholds.
#' @param permutations Requested permutations.
#' @param seed Integer random seed.
#' @return A list containing model fits, fractions, diagnostics, and status.
#' @examples
#' \dontrun{
#' fit_temporal_hvarpart_dataset(
#'   data_dataset = dataset_data,
#'   response_vars = c("n0", "n1"),
#'   predictor_vars = list(
#'     human = "spd",
#'     climate = "temperature"
#'   )
#' )
#' }
fit_temporal_hvarpart_dataset <- function(
  data_dataset,
  response_vars,
  predictor_vars,
  age_col = "age",
  min_unique_ages = 10L,
  min_residual_df = 5L,
  distance_years = c(500, 1000),
  permutations = 999L,
  seed = 1234L
) {
  assertthat::assert_that(
    is.data.frame(data_dataset),
    is.character(response_vars),
    is.list(predictor_vars),
    identical(sort(names(predictor_vars)), c("climate", "human")),
    age_col %in% names(data_dataset),
    is.numeric(data_dataset[[age_col]]),
    msg = "Temporal HVarPart dataset inputs do not satisfy the contract."
  )

  ages <- data_dataset[[age_col]]
  n_unique_ages <- dplyr::n_distinct(ages[is.finite(ages)])
  initial_status <-
    dplyr::case_when(
      any(!is.finite(ages)) ~ "incomplete_ages",
      anyDuplicated(ages) > 0L ~ "repeated_ages",
      n_unique_ages < min_unique_ages ~ "insufficient_unique_ages",
      .default = NA_character_
    )

  if (
    !is.na(initial_status)
  ) {
    return(
      list(
        status = initial_status,
        n_samples = nrow(data_dataset),
        n_unique_ages = n_unique_ages,
        design_rank = NA_integer_,
        design_full_rank = NA,
        residual_df = NA_integer_,
        human_climate_only_hvarpart = NULL,
        temporal_hvarpart = NULL,
        unique_adjusted_r2 = tibble::tibble(),
        residual_moran = tibble::tibble()
      )
    )
  }

  data_time <-
    scale_temporal_age(
      data_source = data_dataset,
      age_col = age_col,
      output_col = "time"
    )
  predictor_vars_time <-
    c(
      predictor_vars,
      list(time = "time")
    )
  design <-
    diagnose_temporal_hvarpart_design(
      data_source = data_time,
      response_vars = response_vars,
      predictor_vars = predictor_vars_time,
      age_col = age_col,
      min_unique_ages = min_unique_ages,
      min_residual_df = min_residual_df
    )

  if (
    design[["status"]] != "estimable"
  ) {
    return(
      list(
        status = design[["status"]],
        n_samples = design[["n_rows"]],
        n_unique_ages = design[["n_unique_ages"]],
        design_rank = design[["design_rank"]],
        design_full_rank = design[["design_full_rank"]],
        residual_df = design[["residual_df"]],
        human_climate_only_hvarpart = NULL,
        temporal_hvarpart = NULL,
        unique_adjusted_r2 = tibble::tibble(),
        residual_moran = tibble::tibble()
      )
    )
  }

  data_model <- design[["data"]]
  active_predictors <- design[["predictor_vars"]]
  human_climate_only_predictors <- active_predictors[c("human", "climate")]
  result_human_climate_only <-
    fit_varhp(
      data_source = data_model,
      response_vars = design[["response_vars"]],
      predictor_vars = human_climate_only_predictors,
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = permutations
    )
  result_temporal <-
    fit_varhp(
      data_source = data_model,
      response_vars = design[["response_vars"]],
      predictor_vars = active_predictors,
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = permutations
    )
  mat_response <-
    data_model |>
    dplyr::select(dplyr::all_of(design[["response_vars"]])) |>
    as.matrix()
  mat_human <-
    data_model |>
    dplyr::select(dplyr::all_of(active_predictors[["human"]])) |>
    as.matrix() |>
    select_nonconstant_matrix_columns()
  mat_climate <-
    data_model |>
    dplyr::select(dplyr::all_of(active_predictors[["climate"]])) |>
    as.matrix() |>
    select_nonconstant_matrix_columns()
  mat_time <-
    data_model |>
    dplyr::select(dplyr::all_of(active_predictors[["time"]])) |>
    as.matrix()
  data_unique_adjusted_r2 <-
    compute_three_group_unique_adjusted_r2(
      response = mat_response,
      human = mat_human,
      climate = mat_climate,
      structure = mat_time,
      structure_name = "time"
    )
  data_human_climate_only_axes <-
    compute_hvarpart_residual_axes(
      response = mat_response,
      predictors = cbind(mat_human, mat_climate)
    )
  data_temporal_axes <-
    compute_hvarpart_residual_axes(
      response = mat_response,
      predictors = cbind(mat_human, mat_climate, mat_time)
    )
  data_human_climate_only_moran <-
    if (
      ncol(data_human_climate_only_axes) == 0L
    ) {
      tibble::tibble()
    } else {
      compute_temporal_moran_diagnostics(
        data_source = dplyr::bind_cols(
          data_model |>
            dplyr::select(dplyr::all_of(age_col)),
          data_human_climate_only_axes
        ),
        value_cols = names(data_human_climate_only_axes),
        distance_years = distance_years,
        age_col = age_col,
        permutations = permutations,
        seed = seed
      ) |>
        dplyr::mutate(stage = "human_climate_only", .before = 1L)
    }
  data_temporal_moran <-
    if (
      ncol(data_temporal_axes) == 0L
    ) {
      tibble::tibble()
    } else {
      compute_temporal_moran_diagnostics(
        data_source = dplyr::bind_cols(
          data_model |>
            dplyr::select(dplyr::all_of(age_col)),
          data_temporal_axes
        ),
        value_cols = names(data_temporal_axes),
        distance_years = distance_years,
        age_col = age_col,
        permutations = permutations,
        seed = seed
      ) |>
        dplyr::mutate(stage = "time_controlled", .before = 1L)
    }
  data_moran <-
    dplyr::bind_rows(data_human_climate_only_moran, data_temporal_moran)
  status <-
    if (
      any(
        data_temporal_moran[["positive_autocorrelation"]],
        na.rm = TRUE
      )
    ) {
      "estimated_residual_temporal_dependence"
    } else {
      "estimated"
    }

  return(
    list(
      status = status,
      n_samples = design[["n_rows"]],
      n_unique_ages = design[["n_unique_ages"]],
      design_rank = design[["design_rank"]],
      design_full_rank = design[["design_full_rank"]],
      residual_df = design[["residual_df"]],
      human_climate_only_hvarpart = result_human_climate_only,
      temporal_hvarpart = result_temporal,
      unique_adjusted_r2 = data_unique_adjusted_r2,
      residual_moran = data_moran
    )
  )
}
