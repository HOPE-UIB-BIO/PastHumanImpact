#----------------------------------------------------------#
# H1 time-controlled within-dataset SPD models
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
        "data_hvar_filtered_unique_age",
        "data_meta",
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
    name = data_hvar_filtered_unique_age,
    command = {
      fingerprint_h1_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_hvar_filtered_unique_age",
        runner = runner_inputs
      )
    }
  ),
  targets::tar_target(
    name = data_meta,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_meta",
      runner = runner_inputs
    )
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
          "within_dataset_spd_time_control"
      )
  ),
  targets::tar_target(
    name = output_time_controlled_hvarpart_spd,
    command = fit_temporal_hvarpart_datasets(
      data_source = data_hvar_filtered_unique_age,
      response_vars = h1_response_variables,
      predictor_vars = h1_predictor_sets[["spd"]],
      min_unique_ages = h1_analysis_config[["min_unique_ages"]],
      min_residual_df =
        h1_analysis_config[["min_temporal_residual_df"]],
      distance_years =
        h1_analysis_config[["temporal_distances_years"]],
      permutations = h1_analysis_config[["permutations"]],
      seed = h1_analysis_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = result_time_controlled_hvarpart_spd,
    command = summarise_temporal_hvarpart_results(
      data_results = output_time_controlled_hvarpart_spd,
      analysis = "spatial_spd"
    )
  ),
  targets::tar_target(
    name = table_time_control_status,
    command = result_time_controlled_hvarpart_spd[["status"]]
  ),
  targets::tar_target(
    name = table_time_control_hierarchical_contributions,
    command = result_time_controlled_hvarpart_spd[["components"]]
  ),
  targets::tar_target(
    name = table_time_control_unique_adjusted_r2,
    command = result_time_controlled_hvarpart_spd[["unique_adjusted_r2"]]
  ),
  targets::tar_target(
    name = table_time_control_residual_moran,
    command = result_time_controlled_hvarpart_spd[["residual_moran"]]
  ),
  targets::tar_target(
    name = data_time_controlled_balance_records_all,
    command = prepare_time_controlled_importance_records(
      data_components = table_time_control_hierarchical_contributions,
      data_status = table_time_control_status,
      data_meta = data_meta
    )
  ),
  targets::tar_target(
    name = data_time_controlled_balance_records,
    command = data_time_controlled_balance_records_all |>
      dplyr::filter(
        is.finite(.data[["signed_balance"]]),
        .data[["signed_weight"]] > 0,
        is.finite(.data[["zero_balance"]]),
        .data[["zero_weight"]] > 0
      )
  ),
  targets::tar_target(
    name = table_h1_result_records,
    command = prepare_h1_result_records(
      data_components =
        table_time_control_hierarchical_contributions,
      data_status = table_time_control_status,
      profile_id =
        h1_analysis_profile[["profile_id"]][1],
      model_specification = "human_climate_time",
      proxy = "spd",
      analytical_unit = "within_dataset",
      selected_control_dimensions = "time",
      input_hash = fingerprint_h1_inputs,
      profile_hash = rlang::hash(h1_analysis_profile),
      configuration_hash = rlang::hash(h1_analysis_config)
    )
  )
)
