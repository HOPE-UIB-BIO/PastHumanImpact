#' @title Analyse one spatially explicit HVarPart group
#' @description
#' Fit baseline two-group HVarPart, select dbMEMs conditional on human and
#' climate predictors, fit a three-group HVarPart when spatial terms are
#' selected, calculate pure partial fractions, and diagnose residual spatial
#' structure.
#' @param data_group One cross-site region-by-age data frame.
#' @param response_vars Response variable names.
#' @param predictor_vars Named list with `human` and `climate` variables.
#' @param id_col Site identifier column.
#' @param long_col Longitude column.
#' @param lat_col Latitude column.
#' @param permutations Number of permutations.
#' @param alpha Spatial selection threshold.
#' @param min_unique_locations Minimum unique spatial locations.
#' @param min_residual_df Minimum residual degrees of freedom.
#' @param distance_km Moran diagnostic distances in kilometres.
#' @param seed Integer random seed.
#' @return
#' A list with fit status, baseline and spatial HVarPart results, dbMEM
#' diagnostics and selection, pure adjusted-R-squared fractions, residual
#' Moran diagnostics, and a remaining-spatial-signal test.
#' @examples
#' \dontrun{
#' analyse_spatial_hvarpart_group(
#'   data_group = region_age_data,
#'   response_vars = pap_variables,
#'   predictor_vars = list(
#'     human = "spd",
#'     climate = c("temp_annual", "prec_summer")
#'   )
#' )
#' }
analyse_spatial_hvarpart_group <- function(
  data_group,
  response_vars,
  predictor_vars = list(
    human = "spd",
    climate = c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    )
  ),
  id_col = "dataset_id",
  long_col = "long",
  lat_col = "lat",
  permutations = 999L,
  alpha = 0.05,
  min_unique_locations = 20L,
  min_residual_df = 10L,
  distance_km = c(250, 500),
  seed = 1234L
) {
  required_columns <-
    unique(c(
      id_col,
      long_col,
      lat_col,
      response_vars,
      unlist(predictor_vars, use.names = FALSE)
    ))
  assertthat::assert_that(
    is.data.frame(data_group),
    is.character(response_vars),
    length(response_vars) > 0L,
    is.list(predictor_vars),
    identical(sort(names(predictor_vars)), c("climate", "human")),
    all(required_columns %in% names(data_group)),
    msg = "Spatial HVarPart inputs do not satisfy the required contract."
  )

  data_complete <-
    data_group |>
    dplyr::filter(
      stats::complete.cases(
        dplyr::across(dplyr::all_of(required_columns))
      )
    ) |>
    dplyr::mutate(.spatial_record_id = dplyr::row_number())
  assertthat::assert_that(
    nrow(data_complete) > length(unlist(predictor_vars)) + 2L,
    msg = "Spatial HVarPart requires more complete rows than predictors."
  )
  data_complete <-
    validate_spatial_coordinates(
      data_source = data_complete,
      id_col = ".spatial_record_id",
      long_col = long_col,
      lat_col = lat_col
    )

  response_matrix <-
    data_complete |>
    dplyr::select(dplyr::all_of(response_vars)) |>
    as.matrix()
  data_human <-
    data_complete |>
    dplyr::select(dplyr::all_of(predictor_vars[["human"]]))
  data_climate <-
    data_complete |>
    dplyr::select(dplyr::all_of(predictor_vars[["climate"]]))
  mat_conditions <-
    cbind(
      intercept = 1,
      as.matrix(data_human),
      as.matrix(data_climate)
    )

  result_baseline <-
    get_varhp(
      data_source = data_complete,
      response_vars = response_vars,
      predictor_vars = predictor_vars,
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE,
      permutations = permutations
    )
  result_dbmem <-
    get_dbmem_basis(
      data_source = data_complete,
      id_col = ".spatial_record_id",
      long_col = long_col,
      lat_col = lat_col,
      min_unique_locations = min_unique_locations
    )
  vec_mem_names <-
    names(result_dbmem[["basis"]]) |>
    stringr::str_subset("^dbmem_")

  result_selection <-
    if (
      length(vec_mem_names) == 0L
    ) {
      create_empty_dbmem_selection(
        status_value = "spatial_not_estimable",
        n_complete = nrow(data_complete),
        n_candidates = 0L
      )
    } else {
      select_dbmem_predictors(
        response = response_matrix,
        mem_basis = result_dbmem[["basis"]][vec_mem_names],
        conditions = mat_conditions,
        permutations = permutations,
        alpha = alpha,
        min_residual_df = min_residual_df,
        seed = seed
      )
    }
  vec_selected <- result_selection[["selected_names"]]
  vec_unselected <- setdiff(vec_mem_names, vec_selected)
  data_model <-
    data_complete |>
    dplyr::bind_cols(result_dbmem[["basis"]][vec_mem_names])
  data_space <-
    if (
      length(vec_selected) == 0L
    ) {
      NULL
    } else {
      data_model[vec_selected]
    }

  result_spatial <-
    if (
      is.null(data_space)
    ) {
      NULL
    } else {
      get_varhp(
        data_source = data_model,
        response_vars = response_vars,
        predictor_vars = c(
          predictor_vars,
          list(space = vec_selected)
        ),
        run_all_predictors = FALSE,
        time_series = FALSE,
        get_significance = FALSE,
        permutations = permutations
      )
    }

  mat_human <- as.matrix(data_human)
  mat_climate <- as.matrix(data_climate)
  mat_space <-
    if (
      is.null(data_space)
    ) {
      NULL
    } else {
      as.matrix(data_space)
    }
  pure_human <-
    calculate_partial_rda_adjusted_r_squared(
      response = response_matrix,
      explanatory = mat_human,
      conditioning = cbind(mat_climate, mat_space)
    )
  pure_climate <-
    calculate_partial_rda_adjusted_r_squared(
      response = response_matrix,
      explanatory = mat_climate,
      conditioning = cbind(mat_human, mat_space)
    )
  pure_space <-
    if (
      is.null(mat_space)
    ) {
      0
    } else {
      calculate_partial_rda_adjusted_r_squared(
        response = response_matrix,
        explanatory = mat_space,
        conditioning = cbind(mat_human, mat_climate)
      )
    }
  mat_all_predictors <- cbind(mat_human, mat_climate, mat_space)
  total_adjusted_r_squared <-
    calculate_partial_rda_adjusted_r_squared(
      response = response_matrix,
      explanatory = mat_all_predictors
    )
  data_partial <-
    tibble::tibble(
      fraction = c(
        "pure_human",
        "pure_climate",
        "pure_space",
        "shared",
        "total_explained",
        "unexplained"
      ),
      adjusted_r_squared = c(
        pure_human,
        pure_climate,
        pure_space,
        total_adjusted_r_squared -
          pure_human - pure_climate - pure_space,
        total_adjusted_r_squared,
        1 - total_adjusted_r_squared
      )
    )

  model_full <-
    vegan::rda(
      X = response_matrix,
      Y = mat_all_predictors,
      scale = TRUE
    )
  mat_residuals <- stats::residuals(model_full)
  model_residual_axes <- vegan::rda(X = mat_residuals, scale = TRUE)
  n_axes <-
    min(
      3L,
      model_residual_axes[["CA"]][["rank"]],
      ncol(mat_residuals),
      nrow(mat_residuals) - 1L
    )
  mat_axes <-
    vegan::scores(
      x = model_residual_axes,
      display = "sites",
      choices = seq_len(n_axes)
    )
  colnames(mat_axes) <- stringr::str_c("residual_axis_", seq_len(n_axes))
  data_residuals <-
    data_complete |>
    dplyr::select(
      dplyr::all_of(c(
        ".spatial_record_id",
        long_col,
        lat_col
      ))
    ) |>
    dplyr::bind_cols(tibble::as_tibble(mat_axes))
  data_moran <-
    calculate_moran_diagnostics(
      data_source = data_residuals,
      value_cols = colnames(mat_axes),
      distance_km = distance_km,
      id_col = ".spatial_record_id",
      long_col = long_col,
      lat_col = lat_col,
      permutations = permutations,
      seed = seed
    )

  data_remaining_test <-
    if (
      length(vec_unselected) == 0L
    ) {
      tibble::tibble(
        n_unselected_mem = 0L,
        p_value = NA_real_,
        status = "no_unselected_mem"
      )
    } else {
      model_remaining <-
        vegan::rda(
          X = response_matrix,
          Y = as.matrix(data_model[vec_unselected]),
          Z = mat_all_predictors,
          scale = TRUE
        )
      test_remaining <-
        vegan::anova.cca(
          object = model_remaining,
          permutations = permutations
        )
      tibble::tibble(
        n_unselected_mem = length(vec_unselected),
        p_value = as.numeric(test_remaining[["Pr(>F)"]][1]),
        status = "estimated"
      )
    }

  analysis_status <-
    dplyr::case_when(
      result_selection[["status"]] == "spatial_not_estimable" ~
        "spatial_not_estimable",
      length(vec_selected) == 0L ~ "no_spatial_terms_selected",
      .default = "spatial_model_estimated"
    )
  res_analysis <-
    list(
      status = analysis_status,
      n_samples = nrow(data_complete),
      baseline_hvarpart = result_baseline,
      spatial_hvarpart = result_spatial,
      dbmem = result_dbmem,
      selection = result_selection,
      partial_fractions = data_partial,
      residual_moran = data_moran,
      remaining_spatial_test = data_remaining_test
    )

  return(res_analysis)
}
