#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Authorized temporal model fitting
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the authorized temporal model fitting target graph.
# Run with:
#   R/analyses/03_temporal_models/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

path_model_dir <-
  file.path(path_temporal_models, "Mods")

path_run_requests <-
  file.path(path_temporal_models, "general_model_run_requests.csv")

path_run_history <-
  file.path(path_temporal_models, "general_model_run_history.csv")

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "temporal_models/inputs"
  )

store_configuration <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "temporal_models/configuration"
  )

runner_temporal <-
  "R/analyses/03_temporal_models/00_run.R"

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track temporal model run requests as a file target so file changes
  #   invalidate downstream results.
  targets::tar_target(
    name = "file_temporal_model_run_requests",
    command = {
      initialize_temporal_model_run_requests(path = path_run_requests)

      path_run_requests
    },
    format = "file"
  ),
  # Why: Prepare temporal model run requests so downstream targets share one
  #   canonical dataset.
  targets::tar_target(
    name = "data_temporal_model_run_requests",
    command = load_temporal_model_run_requests(
      path = file_temporal_model_run_requests
    )
  ),
  # Why: Prepare temporal model config so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_temporal_model_config",
    command = load_target_store_value(
      store = store_configuration,
      target_name = "data_temporal_model_config",
      runner = runner_temporal
    )
  ),
  # Why: Track temporal model lifecycle as a file target so file changes
  #   invalidate downstream results.
  targets::tar_target(
    name = "file_temporal_model_lifecycle",
    command = save_temporal_model_lifecycle(
      data_config = data_temporal_model_config,
      directory = path_temporal_models
    ),
    format = "file"
  ),
  # Why: Prepare temporal model so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_temporal_model",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "data_temporal_model",
      runner = runner_temporal
    )
  ),
  # Why: Prepare temporal model run history so downstream targets share one
  #   canonical dataset.
  targets::tar_target(
    name = "data_temporal_model_run_history",
    command = if (
      file.exists(path_run_history)
    ) {
      readr::read_csv(path_run_history, show_col_types = FALSE)
    } else {
      tibble::tibble()
    },
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Assemble temporal model authorization once so its extracted components
  #   remain consistent.
  targets::tar_target(
    name = "result_temporal_model_authorization",
    command = {
      file_temporal_model_lifecycle

      select_authorized_temporal_model_runs(
        data_config = data_temporal_model_config,
        data_requests = data_temporal_model_run_requests,
        data_history = data_temporal_model_run_history
      )
    }
  ),
  # Why: Materialize temporal model run request audit so downstream reporting
  #   uses an auditable result.
  targets::tar_target(
    name = "table_temporal_model_run_request_audit",
    command = result_temporal_model_authorization[["audit"]]
  ),
  # Why: Materialize authorized temporal model runs so downstream reporting uses
  #   an auditable result.
  targets::tar_target(
    name = "table_authorized_temporal_model_runs",
    command = result_temporal_model_authorization[["authorized"]]
  ),
  # Why: Materialize temporal model fit attempts so downstream reporting uses an
  #   auditable result.
  targets::tar_target(
    name = "table_temporal_model_fit_attempts",
    command = run_authorized_temporal_models(
      data_authorized = table_authorized_temporal_model_runs,
      data_source = data_temporal_model,
      config_dir = path_temporal_models,
      model_dir = path_model_dir,
      path_history = path_run_history,
      git_commit = load_git_state(
        repo_path = here::here(),
        verbose = FALSE
      )[["git_commit"]][1],
      git_is_dirty = load_git_state(
        repo_path = here::here(),
        verbose = FALSE
      )[["git_is_dirty"]][1],
      verbose = TRUE
    )
  )
)
