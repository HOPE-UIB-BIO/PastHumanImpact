#' @title Spatially filter core-level HVarPart importance
#' @description
#' Select continent-specific dbMEMs from signed core-level importance
#' conditional on region-by-climate-zone strata, fit weighted aggregation
#' models, and diagnose Moran autocorrelation before and after filtering.
#' @param data_records Output from `prepare_spatial_importance_records()`.
#' @param permutations Number of permutations.
#' @param alpha Significance threshold for dbMEM selection.
#' @param min_unique_locations Minimum unique locations per dbMEM network.
#' @param min_residual_df Minimum residual degrees of freedom.
#' @param distance_km Fixed Moran diagnostic distances in kilometres.
#' @param seed Integer random seed.
#' @return
#' A list containing dbMEM diagnostics, selection, adjusted estimates, Moran
#' diagnostics, and fitted weighted models.
#' @examples
#' \dontrun{
#' analyse_spatial_importance(data_records = figure2_records)
#' }
analyse_spatial_importance <- function(
  data_records,
  permutations = 999L,
  alpha = 0.05,
  min_unique_locations = 20L,
  min_residual_df = 10L,
  distance_km = c(250, 500),
  seed = 1234L
) {
  required_columns <-
    c(
      "model_id",
      "long",
      "lat",
      "region",
      "climatezone",
      "signed_balance",
      "signed_weight",
      "zero_balance",
      "zero_weight"
    )
  assertthat::assert_that(
    is.data.frame(data_records),
    all(required_columns %in% names(data_records)),
    all(is.finite(data_records[["signed_balance"]])),
    all(data_records[["signed_weight"]] > 0),
    msg = "Spatial importance records do not satisfy the required contract."
  )

  data_analysis <-
    data_records |>
    dplyr::mutate(
      spatial_stratum = interaction(
        .data[["region"]],
        .data[["climatezone"]],
        drop = TRUE,
        lex.order = TRUE
      )
    )
  result_dbmem <-
    get_dbmem_basis(
      data_source = data_analysis,
      id_col = "model_id",
      group_col = "region",
      min_unique_locations = min_unique_locations
    )
  vec_mem_names <-
    names(result_dbmem[["basis"]]) |>
    stringr::str_subset("^dbmem_")
  mat_conditions <-
    stats::model.matrix(
      object = ~ 0 + spatial_stratum,
      data = data_analysis
    )

  result_selection <-
    if (
      length(vec_mem_names) == 0L
    ) {
      create_empty_dbmem_selection(
        status_value = "no_eligible_network",
        n_complete = nrow(data_analysis),
        n_candidates = 0L
      )
    } else {
      select_dbmem_predictors(
        response = data_analysis[["signed_balance"]],
        mem_basis = result_dbmem[["basis"]][vec_mem_names],
        conditions = mat_conditions,
        permutations = permutations,
        alpha = alpha,
        min_residual_df = min_residual_df,
        seed = seed
      )
    }
  vec_selected <- result_selection[["selected_names"]]
  data_model <-
    data_analysis |>
    dplyr::bind_cols(result_dbmem[["basis"]][vec_mem_names])

  model_signed <-
    fit_spatial_importance_profile(
      data_model = data_model,
      response_col = "signed_balance",
      weight_col = "signed_weight",
      selected_mem_names = vec_selected
    )
  model_zero <-
    fit_spatial_importance_profile(
      data_model = data_model,
      response_col = "zero_balance",
      weight_col = "zero_weight",
      selected_mem_names = vec_selected
    )

  data_estimates <-
    dplyr::bind_rows(
      get_adjusted_spatial_importance_estimates(
        model_object = model_signed,
        data_model = data_model,
        profile_name = "signed",
        weight_col = "signed_weight",
        mem_names = vec_mem_names
      ),
      get_adjusted_spatial_importance_estimates(
        model_object = model_zero,
        data_model = data_model,
        profile_name = "zero_truncated",
        weight_col = "zero_weight",
        mem_names = vec_mem_names
      )
    ) |>
    dplyr::mutate(selection_status = result_selection[["status"]])

  data_moran_source <-
    data_model |>
    dplyr::mutate(
      signed_residual = stats::residuals(model_signed),
      zero_residual = stats::residuals(model_zero)
    )
  value_columns <-
    c(
      "signed_balance",
      "signed_residual",
      "zero_balance",
      "zero_residual"
    )
  data_diagnostics_fixed <-
    data_moran_source |>
    calculate_moran_diagnostics(
      value_cols = value_columns,
      distance_km = distance_km,
      id_col = "model_id",
      block_col = "region",
      permutations = permutations,
      seed = seed
    ) |>
    dplyr::mutate(
      spatial_scope = "global",
      spatial_group = "All"
    )
  data_diagnostics_connectivity <-
    result_dbmem[["diagnostics"]] |>
    dplyr::filter(
      .data[["status"]] == "eligible",
      is.finite(.data[["threshold_km"]])
    ) |>
    dplyr::transmute(
      spatial_group = .data[["spatial_group"]],
      threshold_km = .data[["threshold_km"]]
    ) |>
    purrr::pmap_dfr(
      .f = ~ calculate_moran_diagnostics(
        data_source = data_moran_source |>
          dplyr::filter(.data[["region"]] == ..1),
        value_cols = value_columns,
        distance_km = ..2,
        id_col = "model_id",
        permutations = permutations,
        seed = seed
      ) |>
        dplyr::mutate(
          spatial_scope = "dbmem_connectivity",
          spatial_group = ..1
        )
    )
  data_diagnostics <-
    dplyr::bind_rows(
      data_diagnostics_fixed,
      data_diagnostics_connectivity
    ) |>
    dplyr::mutate(
      stage = dplyr::if_else(
        stringr::str_ends(.data[["value"]], "_residual"),
        "residual",
        "baseline"
      ),
      profile = dplyr::case_when(
        stringr::str_starts(.data[["value"]], "signed") ~ "signed",
        .default = "zero_truncated"
      )
    )

  res_analysis <-
    list(
      dbmem = result_dbmem,
      selection = result_selection,
      estimates = data_estimates,
      moran_diagnostics = data_diagnostics,
      models = list(
        signed = model_signed,
        zero_truncated = model_zero
      )
    )

  return(res_analysis)
}
