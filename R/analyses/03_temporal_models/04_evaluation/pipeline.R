#----------------------------------------------------------#
# Temporal-model evaluation lifecycle
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

path_model_dir <-
  file.path(path_temporal_models, "Mods")

path_run_history <-
  file.path(path_temporal_models, "general_model_run_history.csv")

list(
  targets::tar_target(
    name = table_temporal_model_evaluations,
    command = run_pending_temporal_model_evaluations(
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
    ),
    cue = targets::tar_cue(mode = "always")
  )
)
