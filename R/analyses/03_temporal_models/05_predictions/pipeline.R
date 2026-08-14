#----------------------------------------------------------#
# Temporal-model predictions and cache reuse
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

path_model_dir <-
  file.path(path_temporal_models, "Mods")

path_prediction_dir <-
  file.path(path_temporal_models, "General_trends")

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "temporal_models/inputs"
  )

runner_temporal <-
  "R/analyses/03_temporal_models/00_run.R"

list(
  targets::tar_target(
    name = data_temporal_model,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_temporal_model",
      runner = runner_temporal
    )
  ),
  targets::tar_target(
    name = data_temporal_model_predictions,
    command = run_temporal_model_predictions(
      data_source = data_temporal_model,
      config_dir = path_temporal_models,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      rewrite = FALSE,
      max_prediction_draws = 1000L,
      prediction_range = "group_observed",
      verbose = TRUE
    ),
    cue = targets::tar_cue(mode = "always")
  )
)
