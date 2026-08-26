#' @title Fit one spatially explicit HVarPart group
#' @description
#' Fit human_climate_only two-group HVarPart, select dbMEMs conditional on human and
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
#' A list with fit status, human_climate_only and spatial HVarPart results, dbMEM
#' diagnostics and selection, pure adjusted-R-squared fractions, residual
#' Moran diagnostics, and a remaining-spatial-signal test.
#' @examples
#' \dontrun{
#' fit_spatial_hvarpart_group(
#'   data_group = region_age_data,
#'   response_vars = pap_variables,
#'   predictor_vars = list(
#'     human = "spd",
#'     climate = c("temp_annual", "prec_summer")
#'   )
#' )
#' }
fit_spatial_hvarpart_group <- function(
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
  candidate_columns <-
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
    all(candidate_columns %in% names(data_group)),
    msg = "Spatial HVarPart inputs do not satisfy the required contract."
  )

  active_predictors <-
    predictor_vars |>
    purrr::map(
      .f = ~ .x |>
        purrr::keep(
          .p = ~ {
            vec_values <- data_group[[.x]]
            vec_finite <- vec_values[is.finite(vec_values)]
            length(vec_finite) > 1L &&
              dplyr::n_distinct(vec_finite) > 1L
          }
        )
    )
  if (
    any(purrr::map_int(active_predictors, length) == 0L)
  ) {
    res <-
      build_empty_spatial_hvarpart_result(
        status = "missing_predictor_group",
        n_samples = nrow(data_group)
      )

    return(res)
  }
  predictor_vars <- active_predictors
  required_columns <-
    unique(c(
      id_col,
      long_col,
      lat_col,
      response_vars,
      unlist(predictor_vars, use.names = FALSE)
    ))
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

  result_human_climate_only <-
    fit_varhp(
      data_source = data_complete,
      response_vars = response_vars,
      predictor_vars = predictor_vars,
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE,
      permutations = permutations
    )
  result_dbmem <-
    compute_dbmem_basis(
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
      build_empty_dbmem_selection(
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

  if (
    result_selection[["status"]] == "spatial_not_estimable"
  ) {
    res <-
      list(
        status = "spatial_not_estimable",
        n_samples = nrow(data_complete),
        human_climate_only_hvarpart = result_human_climate_only,
        spatial_hvarpart = NULL,
        dbmem = result_dbmem,
        selection = result_selection,
        unique_adjusted_r2 = tibble::tibble(),
        residual_moran = tibble::tibble(),
        remaining_spatial_test = tibble::tibble()
      )

    return(res)
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
      fit_varhp(
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
    compute_partial_rda_adjusted_r_squared(
      response = response_matrix,
      explanatory = mat_human,
      conditioning = cbind(mat_climate, mat_space)
    )
  pure_climate <-
    compute_partial_rda_adjusted_r_squared(
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
      compute_partial_rda_adjusted_r_squared(
        response = response_matrix,
        explanatory = mat_space,
        conditioning = cbind(mat_human, mat_climate)
      )
    }
  mat_all_predictors <- cbind(mat_human, mat_climate, mat_space)
  total_adjusted_r_squared <-
    compute_partial_rda_adjusted_r_squared(
      response = response_matrix,
      explanatory = mat_all_predictors
    )
  data_unique_adjusted_r2 <-
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

  data_axes <-
    compute_hvarpart_residual_axes(
      response = response_matrix,
      predictors = mat_all_predictors
    )
  data_residuals <-
    data_complete |>
    dplyr::select(
      dplyr::all_of(c(
        ".spatial_record_id",
        long_col,
        lat_col
      ))
    ) |>
    dplyr::bind_cols(data_axes)
  connectivity_threshold <-
    result_dbmem[["thresholds"]] |>
    dplyr::filter(is.finite(.data[["threshold_km"]])) |>
    dplyr::pull("threshold_km") |>
    dplyr::first()
  data_moran_fixed <-
    if (
      ncol(data_axes) == 0L
    ) {
      tibble::tibble()
    } else {
      compute_moran_diagnostics(
        data_source = data_residuals,
        value_cols = names(data_axes),
        distance_km = distance_km,
        id_col = ".spatial_record_id",
        long_col = long_col,
        lat_col = lat_col,
        permutations = permutations,
        seed = seed
      ) |>
        dplyr::mutate(
          spatial_scope =
            stringr::str_c("fixed_", .data[["distance_km"]], "km")
        )
    }
  data_moran_connectivity <-
    if (
      ncol(data_axes) == 0L ||
        length(connectivity_threshold) == 0L ||
        !is.finite(connectivity_threshold)
    ) {
      tibble::tibble()
    } else {
      compute_moran_diagnostics(
        data_source = data_residuals,
        value_cols = names(data_axes),
        distance_km = connectivity_threshold,
        id_col = ".spatial_record_id",
        long_col = long_col,
        lat_col = lat_col,
        permutations = permutations,
        seed = seed
      ) |>
        dplyr::mutate(spatial_scope = "dbmem_connectivity")
    }
  data_moran <-
    dplyr::bind_rows(
      data_moran_fixed,
      data_moran_connectivity
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
      length(vec_selected) == 0L ~ "no_spatial_terms_selected",
      .default = "spatial_model_estimated"
    )
  res_analysis <-
    list(
      status = analysis_status,
      n_samples = nrow(data_complete),
      human_climate_only_hvarpart = result_human_climate_only,
      spatial_hvarpart = result_spatial,
      dbmem = result_dbmem,
      selection = result_selection,
      unique_adjusted_r2 = data_unique_adjusted_r2,
      residual_moran = data_moran,
      remaining_spatial_test = data_remaining_test
    )

  return(res_analysis)
}
