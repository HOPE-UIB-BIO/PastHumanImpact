#----------------------------------------------------------#
# H1 spatially controlled time-slice SPD models
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )

runner_inputs <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"

list(
  targets::tar_target(
    name = fingerprint_h1_inputs,
    command = compute_target_store_fingerprint(
      store = store_inputs,
      target_names = c(
        "data_hvar_timebins_spd_unique_age",
        "h1_response_variables",
        "h1_predictor_sets",
        "h1_analysis_config",
        "data_analysis_profiles"
      ),
      runner = runner_inputs
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = data_hvar_timebins_spd_unique_age,
    command = {
      fingerprint_h1_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_hvar_timebins_spd_unique_age",
        runner = runner_inputs
      )
    }
  ),
  targets::tar_target(
    name = h1_response_variables,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_response_variables",
      runner = runner_inputs
    )
  ),
  targets::tar_target(
    name = h1_predictor_sets,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_predictor_sets",
      runner = runner_inputs
    )
  ),
  targets::tar_target(
    name = h1_analysis_config,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_analysis_config",
      runner = runner_inputs
    )
  ),
  targets::tar_target(
    name = h1_analysis_profile,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_analysis_profiles",
      runner = runner_inputs
    ) |>
      dplyr::filter(
        .data[["profile_id"]] ==
          "time_slice_spd_spatial_control"
      )
  ),
  targets::tar_target(
    name = output_spatial_controlled_hvarpart_spd,
    command = fit_spatial_hvarpart_dataset(
      data_source = data_hvar_timebins_spd_unique_age,
      analysis = "temporal_spd",
      response_vars = h1_response_variables,
      predictor_vars = h1_predictor_sets[["spd"]],
      permutations = h1_analysis_config[["permutations"]],
      alpha = h1_analysis_config[["alpha"]],
      min_unique_locations =
        h1_analysis_config[["min_unique_locations"]],
      min_residual_df =
        h1_analysis_config[["min_spatial_residual_df"]],
      distance_km = h1_analysis_config[["spatial_distances_km"]],
      seed = h1_analysis_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = result_spatial_controlled_hvarpart_spd,
    command = summarise_spatial_hvarpart_results(
      data_results = output_spatial_controlled_hvarpart_spd
    )
  ),
  targets::tar_target(
    name = table_spatial_control_status,
    command = result_spatial_controlled_hvarpart_spd[["status"]]
  ),
  targets::tar_target(
    name = table_spatial_control_dbmem_selection,
    command = result_spatial_controlled_hvarpart_spd[["selection"]]
  ),
  targets::tar_target(
    name = table_spatial_control_dbmem_diagnostics,
    command = result_spatial_controlled_hvarpart_spd[["dbmem_diagnostics"]]
  ),
  targets::tar_target(
    name = table_spatial_control_hierarchical_contributions,
    command = result_spatial_controlled_hvarpart_spd[["components"]]
  ),
  targets::tar_target(
    name = table_spatial_control_unique_adjusted_r2,
    command = result_spatial_controlled_hvarpart_spd[["unique_adjusted_r2"]]
  ),
  targets::tar_target(
    name = table_spatial_control_residual_moran,
    command = result_spatial_controlled_hvarpart_spd[["residual_moran"]]
  ),
  targets::tar_target(
    name = table_spatial_control_remaining_signal,
    command = result_spatial_controlled_hvarpart_spd[[
      "remaining_spatial_test"
    ]]
  ),
  targets::tar_target(
    name = table_spatial_control_zero_truncated_composition,
    command = prepare_spatial_hvarpart_composition(
      data_components = table_spatial_control_hierarchical_contributions,
      data_status = table_spatial_control_status
    )
  ),
  targets::tar_target(
    name = table_spatial_control_rankings,
    command = diagnose_spatial_hvarpart_rankings(
      data_components = table_spatial_control_hierarchical_contributions,
      data_status = table_spatial_control_status
    )
  ),
  targets::tar_target(
    name = table_h1_result_records,
    command = prepare_h1_result_records(
      data_components =
        table_spatial_control_hierarchical_contributions,
      data_status = table_spatial_control_status,
      profile_id =
        h1_analysis_profile[["profile_id"]][1],
      model_specification = "human_climate_space",
      proxy = "spd",
      analytical_unit = "time_slice",
      selected_control_dimensions = "space",
      input_hash = fingerprint_h1_inputs,
      profile_hash = rlang::hash(h1_analysis_profile),
      configuration_hash = rlang::hash(h1_analysis_config)
    )
  )
)
