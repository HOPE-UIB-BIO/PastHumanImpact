#----------------------------------------------------------#
# Temporal-model lifecycle configuration
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

temporal_specification_columns <-
  c(
    "model_id",
    "analysis",
    "variable",
    "region",
    "climatezone",
    "family_key",
    "requested_model_profile",
    "input_data_hash",
    "configuration_reference_hash"
  )

temporal_configuration_reference_hash <-
  rlang::hash(
    list(
      total_iterations = 3200,
      min_iterations_per_chain = 100,
      max_chains = 4,
      adapt_delta = 0.9,
      max_treedepth = 10,
      large_model_min_records = 100,
      large_model_total_iterations = 6400,
      large_model_adapt_delta = 0.95,
      large_model_max_treedepth = 12
    )
  )

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "temporal_models/inputs"
  )

runner_temporal <-
  "R/analyses/03_temporal_models/00_run.R"

list(
  targets::tar_target(
    name = fingerprint_temporal_inputs,
    command = compute_target_store_fingerprint(
      store = store_inputs,
      target_names = c(
        "data_temporal_model_specifications",
        "temporal_model_input_hash"
      ),
      runner = runner_temporal
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = file_temporal_model_config,
    command = file.path(
      path_temporal_models,
      RUtilpol::get_latest_file_name(
        file_name = "general_model_config_table",
        dir = path_temporal_models
      )
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_temporal_model_config_source,
    command = readr::read_csv(
      file_temporal_model_config,
      show_col_types = FALSE
    )
  ),
  targets::tar_target(
    name = temporal_model_input_hash,
    command = {
      fingerprint_temporal_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "temporal_model_input_hash",
        runner = runner_temporal
      )
    }
  ),
  targets::tar_target(
    name = data_temporal_model,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_temporal_model",
      runner = runner_temporal
    )
  ),
  targets::tar_target(
    name = data_temporal_model_specifications,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_temporal_model_specifications",
      runner = runner_temporal
    )
  ),
  targets::tar_target(
    name = temporal_family_key_by_model,
    command = data_temporal_model_specifications |>
      dplyr::select(
        dplyr::all_of(c("model_id", "family_key"))
      ) |>
      tibble::deframe()
  ),
  targets::tar_target(
    name = data_temporal_model_config_candidate,
    command = build_model_config_table(
      data_model = data_temporal_model,
      analysis = "general_temporal",
      family_key = temporal_family_key_by_model,
      engine = "brms",
      model_profile = "within_stratum_dataset_fs",
      x_var = "age_ka",
      x_model_var = "age_ka_scaled",
      y_var = "value",
      group_var = "dataset_id",
      stratum_var = "stratum",
      smooth_basis = "cr",
      common_k = 8,
      group_k = 3,
      age_min = 0,
      age_max = 8500,
      timestep = 500,
      min_records = min_n_records_per_climate_zone,
      total_iterations = 3200,
      min_iterations_per_chain = 100,
      max_chains = 4,
      adapt_delta = 0.9,
      max_treedepth = 10,
      seed_base = set_seed,
      large_model_min_records = 100,
      large_model_total_iterations = 6400,
      large_model_adapt_delta = 0.95,
      large_model_max_treedepth = 12
    ) |>
      dplyr::mutate(
        input_data_hash = temporal_model_input_hash,
        is_active_model = TRUE
      ) |>
      add_temporal_configuration_reference(
        reference_hash = temporal_configuration_reference_hash,
        overwrite = TRUE
      ) |>
      compute_temporal_model_definition_hashes(
        definition_columns = temporal_specification_columns,
        hash_column = "specification_hash"
      )
  ),
  targets::tar_target(
    name = data_temporal_model_config_current,
    command = data_temporal_model_config_source |>
      dplyr::mutate(
        input_data_hash = temporal_model_input_hash,
        is_active_model = TRUE
      ) |>
      add_temporal_configuration_reference(
        reference_hash = temporal_configuration_reference_hash
      ) |>
      cast_temporal_model_configuration(
        data_candidate = data_temporal_model_config_candidate |>
          dplyr::select(-dplyr::all_of("specification_hash"))
      ) |>
      compute_temporal_model_definition_hashes(
        definition_columns = temporal_specification_columns,
        hash_column = "specification_hash"
      )
  ),
  targets::tar_target(
    name = data_temporal_model_config,
    command = reconcile_temporal_model_configuration(
      data_current = data_temporal_model_config_current,
      data_candidate = data_temporal_model_config_candidate
    ) |>
      compute_temporal_model_definition_hashes()
  ),
  targets::tar_target(
    name = table_temporal_model_lifecycle_audit,
    command = data_temporal_model_config |>
      dplyr::count(
        .data[["is_active_model"]],
        .data[["is_model_eligible"]],
        .data[["need_to_run"]],
        .data[["need_to_be_evaluated"]],
        .data[["prediction_written"]],
        name = "n_models"
      )
  )
)
