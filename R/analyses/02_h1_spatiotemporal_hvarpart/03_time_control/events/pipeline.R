#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Time-controlled event H1 models
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the time-controlled event h1 models target graph.
# Run with:
#   R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
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

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint H1 inputs so upstream changes invalidate this pipeline
  #   store.
  targets::tar_target(
    name = "fingerprint_h1_inputs",
    command = compute_target_store_fingerprint(
      store = store_inputs,
      target_names = c(
        "data_hvar_filtered_unique_age",
        "h1_response_variables",
        "h1_predictor_sets",
        "h1_analysis_config",
        "data_analysis_profiles"
      ),
      runner = runner_inputs
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Prepare hvar filtered unique age so downstream targets share one
  #   canonical dataset.
  targets::tar_target(
    name = "data_hvar_filtered_unique_age",
    command = {
      fingerprint_h1_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_hvar_filtered_unique_age",
        runner = runner_inputs
      )
    }
  ),
  # Why: Define H1 response variables once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "h1_response_variables",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_response_variables",
      runner = runner_inputs
    )
  ),
  # Why: Define H1 predictor sets once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "h1_predictor_sets",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_predictor_sets",
      runner = runner_inputs
    )
  ),
  # Why: Define H1 analysis config once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "h1_analysis_config",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_analysis_config",
      runner = runner_inputs
    )
  ),
  # Why: Define H1 analysis profile once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "h1_analysis_profile",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_analysis_profiles",
      runner = runner_inputs
    ) |>
      dplyr::filter(
        .data[["profile_id"]] ==
          "within_dataset_events_time_control"
      )
  ),
  # Why: Compute time controlled hvarpart events once so downstream summaries
  #   reuse the same result.
  targets::tar_target(
    name = "output_time_controlled_hvarpart_events",
    command = fit_temporal_hvarpart_datasets(
      data_source = data_hvar_filtered_unique_age,
      response_vars = h1_response_variables,
      predictor_vars = h1_predictor_sets[["all_event_groups"]],
      min_unique_ages = h1_analysis_config[["min_unique_ages"]],
      min_residual_df =
        h1_analysis_config[["min_temporal_residual_df"]],
      distance_years =
        h1_analysis_config[["temporal_distances_years"]],
      permutations = h1_analysis_config[["permutations"]],
      seed = h1_analysis_config[["seed"]] + 10000L
    )
  ),
  # Why: Assemble time controlled hvarpart events once so its extracted
  #   components remain consistent.
  targets::tar_target(
    name = "result_time_controlled_hvarpart_events",
    command = summarise_temporal_hvarpart_results(
      data_results = output_time_controlled_hvarpart_events,
      analysis = "spatial_events"
    )
  ),
  # Why: Materialize time control status so downstream reporting uses an
  #   auditable result.
  targets::tar_target(
    name = "table_time_control_status",
    command = result_time_controlled_hvarpart_events[["status"]]
  ),
  # Why: Materialize time control hierarchical contributions so downstream
  #   reporting uses an auditable result.
  targets::tar_target(
    name = "table_time_control_hierarchical_contributions",
    command = result_time_controlled_hvarpart_events[["components"]]
  ),
  # Why: Materialize time control unique adjusted R-squared so downstream
  #   reporting uses an auditable result.
  targets::tar_target(
    name = "table_time_control_unique_adjusted_r2",
    command = result_time_controlled_hvarpart_events[["unique_adjusted_r2"]]
  ),
  # Why: Materialize time control residual Moran's I so downstream reporting
  #   uses an auditable result.
  targets::tar_target(
    name = "table_time_control_residual_moran",
    command = result_time_controlled_hvarpart_events[["residual_moran"]]
  ),
  # Why: Materialize H1 result records so downstream reporting uses an auditable
  #   result.
  targets::tar_target(
    name = "table_h1_result_records",
    command = prepare_h1_result_records(
      data_components =
        table_time_control_hierarchical_contributions,
      data_status = table_time_control_status,
      profile_id =
        h1_analysis_profile[["profile_id"]][1],
      model_specification = "human_climate_time",
      proxy = "events",
      analytical_unit = "within_dataset",
      selected_control_dimensions = "time",
      input_hash = fingerprint_h1_inputs,
      profile_hash = rlang::hash(h1_analysis_profile),
      configuration_hash = rlang::hash(h1_analysis_config)
    )
  )
)
