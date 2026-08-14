#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Within-dataset event H1 models
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the within-dataset event h1 models target graph.
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
        "data_hvar_filtered",
        "data_meta",
        "h1_response_variables",
        "h1_predictor_sets",
        "data_analysis_profiles"
      ),
      runner = runner_inputs
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Prepare hvar filtered so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_hvar_filtered",
    command = {
      fingerprint_h1_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_hvar_filtered",
        runner = runner_inputs
      )
    }
  ),
  # Why: Prepare meta so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_meta",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_meta",
      runner = runner_inputs
    )
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
          "within_dataset_events_human_climate"
      )
  ),
  # Why: Compute spatial events once so downstream summaries reuse the same
  #   result.
  targets::tar_target(
    name = "output_spatial_events",
    command = fit_hvarpart_models(
      data_source = data_hvar_filtered,
      response_vars = h1_response_variables,
      predictor_vars = h1_predictor_sets[["all_event_groups"]],
      response_dist = NULL,
      data_response_dist = NULL,
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999L,
      fail_on_error = FALSE
    )
  ),
  # Why: Prepare hvarpart spatial events importance so downstream targets share
  #   one canonical dataset.
  targets::tar_target(
    name = "data_hvarpart_spatial_events_importance",
    command = compute_hvarpart_importance(
      data_source = output_spatial_events |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(dataset_id, region, climatezone),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_events"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    )
  ),
  # Why: Materialize H1 result records so downstream reporting uses an auditable
  #   result.
  targets::tar_target(
    name = "table_h1_result_records",
    command = prepare_h1_result_records(
      data_components = data_hvarpart_spatial_events_importance,
      profile_id = h1_analysis_profile[["profile_id"]][1],
      model_specification = "human_climate_only",
      proxy = "events",
      analytical_unit = "within_dataset",
      selected_control_dimensions = "none",
      input_hash = fingerprint_h1_inputs,
      profile_hash = rlang::hash(h1_analysis_profile),
      configuration_hash = rlang::hash(
        list(
          h1_response_variables,
          h1_predictor_sets[["all_event_groups"]]
        )
      )
    )
  )
)
