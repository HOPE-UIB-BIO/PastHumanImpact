#' @title Run one strict-radius H1 sensitivity profile
#' @description
#' Reuse the canonical temporal control, spatial control, and spatial
#' aggregation operations for one strict SPD radius.
#' @param data_predictors_profile Prepared predictor data for one radius.
#' @param data_properties_filtered Canonical filtered PAP properties.
#' @param data_meta Core metadata.
#' @param response_vars H1 response variables.
#' @param predictor_vars Named H1 SPD predictor groups.
#' @param analysis_config Canonical H1 fitting configuration.
#' @param data_profiles Enabled SPD-radius analysis profiles.
#' @return Named list of radius-tagged H1 inputs, results, and provenance.
#' @examples
#' \dontrun{
#' result <- run_spd_radius_h1_profile(
#'   radius_predictors,
#'   properties,
#'   metadata,
#'   responses,
#'   predictors,
#'   config,
#'   profiles
#' )
#' }
run_spd_radius_h1_profile <- function(
  data_predictors_profile,
  data_properties_filtered,
  data_meta,
  response_vars,
  predictor_vars,
  analysis_config,
  data_profiles
) {
  assertthat::assert_that(
    is.data.frame(data_predictors_profile),
    all(
      c(
        "dataset_id",
        "radius_km",
        "spd_radius_specification",
        "data_merge"
      ) %in% names(data_predictors_profile)
    ),
    dplyr::n_distinct(data_predictors_profile[["radius_km"]]) == 1L,
    dplyr::n_distinct(
      data_predictors_profile[["spd_radius_specification"]]
    ) == 1L,
    is.data.frame(data_properties_filtered),
    is.data.frame(data_meta),
    is.character(response_vars),
    is.list(predictor_vars),
    is.list(analysis_config),
    is.data.frame(data_profiles),
    msg = "SPD radius H1 profile inputs do not satisfy the contract."
  )

  radius_km <-
    unique(data_predictors_profile[["radius_km"]])
  radius_specification <-
    unique(
      data_predictors_profile[["spd_radius_specification"]]
    )
  profiles_radius <-
    data_profiles |>
    dplyr::filter(
      .data[["spd_radius_specification"]] == radius_specification,
      .data[["human_proxy"]] == "spd",
      .data[["profile_role"]] == "sensitivity",
      .data[["enabled"]]
    )

  expected_operations <-
    c("time", "space", "time_and_space")

  if (
    nrow(profiles_radius) != 3L ||
      !setequal(
        profiles_radius[["structural_control"]],
        expected_operations
      )
  ) {
    cli::cli_abort(
      "Each strict SPD radius requires three enabled H1 profiles."
    )
  }

  profile_id_time <-
    profiles_radius |>
    dplyr::filter(.data[["structural_control"]] == "time") |>
    dplyr::pull(.data[["profile_id"]])
  profile_id_space <-
    profiles_radius |>
    dplyr::filter(.data[["structural_control"]] == "space") |>
    dplyr::pull(.data[["profile_id"]])
  profile_id_aggregation <-
    profiles_radius |>
    dplyr::filter(
      .data[["structural_control"]] == "time_and_space"
    ) |>
    dplyr::pull(.data[["profile_id"]])
  seed_time <-
    profiles_radius |>
    dplyr::filter(.data[["structural_control"]] == "time") |>
    dplyr::pull(.data[["seed"]]) |>
    as.integer()
  seed_space <-
    profiles_radius |>
    dplyr::filter(.data[["structural_control"]] == "space") |>
    dplyr::pull(.data[["seed"]]) |>
    as.integer()
  seed_aggregation <-
    profiles_radius |>
    dplyr::filter(
      .data[["structural_control"]] == "time_and_space"
    ) |>
    dplyr::pull(.data[["seed"]]) |>
    as.integer()

  tag_result <-
    function(data_source, profile_id) {
      data_source |>
        dplyr::mutate(
          radius_km = radius_km,
          spd_radius_specification = radius_specification,
          profile_id = profile_id,
          .before = 1L
        )
    }

  data_predictors_work <-
    data_predictors_profile |>
    dplyr::select(dplyr::all_of(c("dataset_id", "data_merge")))
  data_hvar <-
    prepare_combined_data(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_work
    )
  output_age_collapse <-
    aggregate_hvar_dataset_ages(
      data_source = data_hvar,
      response_vars = response_vars,
      predictor_vars = unique(unlist(predictor_vars, use.names = FALSE))
    )
  data_hvar_unique_age <-
    output_age_collapse[["data"]]

  output_time_control <-
    fit_temporal_hvarpart_datasets(
      data_source = data_hvar_unique_age,
      response_vars = response_vars,
      predictor_vars = predictor_vars,
      min_unique_ages = analysis_config[["min_unique_ages"]],
      min_residual_df =
        analysis_config[["min_temporal_residual_df"]],
      distance_years =
        analysis_config[["temporal_distances_years"]],
      permutations = analysis_config[["permutations"]],
      seed = seed_time
    )
  result_time_control <-
    summarise_temporal_hvarpart_results(
      data_results = output_time_control,
      analysis = "spatial_spd"
    )
  data_time_records_all <-
    prepare_time_controlled_importance_records(
      data_components = result_time_control[["components"]],
      data_status = result_time_control[["status"]],
      data_meta = data_meta
    )
  data_time_records <-
    data_time_records_all |>
    dplyr::filter(
      is.finite(.data[["signed_difference"]]),
      .data[["signed_weight"]] > 0,
      is.finite(.data[["zero_balance"]]),
      .data[["zero_weight"]] > 0
    )
  output_spatial_aggregation <-
    fit_spatial_importance(
      data_records = data_time_records,
      permutations = analysis_config[["permutations"]],
      alpha = analysis_config[["alpha"]],
      min_unique_locations =
        analysis_config[["min_unique_locations"]],
      min_residual_df =
        analysis_config[["min_spatial_residual_df"]],
      distance_km = analysis_config[["spatial_distances_km"]],
      seed = seed_aggregation
    )
  table_aggregation_selection <-
    tibble::tibble(
      status = output_spatial_aggregation[["selection"]][["status"]],
      n_complete =
        output_spatial_aggregation[["selection"]][["n_complete"]],
      n_candidates =
        output_spatial_aggregation[["selection"]][["n_candidates"]],
      global_p_value =
        output_spatial_aggregation[["selection"]][["global_p_value"]],
      full_adjusted_r_squared =
        output_spatial_aggregation[["selection"]][[
          "full_adjusted_r_squared"
        ]],
      selected_terms = stringr::str_c(
        output_spatial_aggregation[["selection"]][["selected_names"]],
        collapse = ";"
      )
    )

  data_timebins <-
    prepare_hvarpart_timebin_data(
      data_source = data_hvar_unique_age,
      data_meta = data_meta
    ) |>
    dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500))
  output_spatial_control <-
    fit_spatial_hvarpart_dataset(
      data_source = data_timebins,
      analysis = "temporal_spd",
      response_vars = response_vars,
      predictor_vars = predictor_vars,
      permutations = analysis_config[["permutations"]],
      alpha = analysis_config[["alpha"]],
      min_unique_locations =
        analysis_config[["min_unique_locations"]],
      min_residual_df =
        analysis_config[["min_spatial_residual_df"]],
      distance_km = analysis_config[["spatial_distances_km"]],
      seed = seed_space
    )
  result_spatial_control <-
    summarise_spatial_hvarpart_results(output_spatial_control)
  table_spatial_composition <-
    prepare_spatial_hvarpart_composition(
      data_components = result_spatial_control[["components"]],
      data_status = result_spatial_control[["status"]]
    )
  table_spatial_rankings <-
    diagnose_spatial_hvarpart_rankings(
      data_components = result_spatial_control[["components"]],
      data_status = result_spatial_control[["status"]]
    )

  res <-
    list(
      dataset_age_collapse = tag_result(
        output_age_collapse[["audit"]],
        profile_id_time
      ),
      time_control_status = tag_result(
        result_time_control[["status"]],
        profile_id_time
      ),
      time_control_components = tag_result(
        result_time_control[["components"]],
        profile_id_time
      ),
      time_control_unique_adjusted_r2 = tag_result(
        result_time_control[["unique_adjusted_r2"]],
        profile_id_time
      ),
      time_control_residual_moran = tag_result(
        result_time_control[["residual_moran"]],
        profile_id_time
      ),
      time_controlled_balance_records_all = tag_result(
        data_time_records_all,
        profile_id_time
      ),
      time_controlled_balance_records = tag_result(
        data_time_records,
        profile_id_time
      ),
      spatial_control_status = tag_result(
        result_spatial_control[["status"]],
        profile_id_space
      ),
      spatial_control_selection = tag_result(
        result_spatial_control[["selection"]],
        profile_id_space
      ),
      spatial_control_dbmem_diagnostics = tag_result(
        result_spatial_control[["dbmem_diagnostics"]],
        profile_id_space
      ),
      spatial_control_components = tag_result(
        result_spatial_control[["components"]],
        profile_id_space
      ),
      spatial_control_unique_adjusted_r2 = tag_result(
        result_spatial_control[["unique_adjusted_r2"]],
        profile_id_space
      ),
      spatial_control_residual_moran = tag_result(
        result_spatial_control[["residual_moran"]],
        profile_id_space
      ),
      spatial_control_remaining_signal = tag_result(
        result_spatial_control[["remaining_spatial_test"]],
        profile_id_space
      ),
      spatial_control_composition = tag_result(
        table_spatial_composition,
        profile_id_space
      ),
      spatial_control_rankings = tag_result(
        table_spatial_rankings,
        profile_id_space
      ),
      spatiotemporal_balance_estimates = tag_result(
        output_spatial_aggregation[["estimates"]],
        profile_id_aggregation
      ),
      spatiotemporal_balance_moran = tag_result(
        output_spatial_aggregation[["moran_diagnostics"]],
        profile_id_aggregation
      ),
      spatiotemporal_balance_dbmem_diagnostics = tag_result(
        output_spatial_aggregation[["dbmem"]][["diagnostics"]],
        profile_id_aggregation
      ),
      spatiotemporal_balance_dbmem_selection = tag_result(
        table_aggregation_selection,
        profile_id_aggregation
      ),
      provenance = tibble::tibble(
        radius_km = radius_km,
        spd_radius_specification = radius_specification,
        time_control_profile_id = profile_id_time,
        spatial_control_profile_id = profile_id_space,
        spatial_aggregation_profile_id = profile_id_aggregation,
        input_hash = rlang::hash(list(
          predictors = data_predictors_profile,
          properties = data_properties_filtered
        )),
        profile_hash = rlang::hash(profiles_radius),
        configuration_hash = rlang::hash(analysis_config),
        time_control_seed = seed_time,
        spatial_control_seed = seed_space,
        spatial_aggregation_seed = seed_aggregation
      )
    )

  return(res)
}
