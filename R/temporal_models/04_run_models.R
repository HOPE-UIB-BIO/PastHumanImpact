#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                         Run models
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

source(
  here::here(
    "R/00_Config_file.R"
  )
)

path_temporal_models <-
  file.path(
    data_storage_path,
    "Temporal_models"
  )

path_model_dir <-
  file.path(
    path_temporal_models,
    "Mods"
  )

run_directory_setup(path_model_dir)

path_model_run_history <-
  file.path(
    path_temporal_models,
    "general_model_run_history.csv"
  )

data_interrupted_runs <-
  if (
    file.exists(path_model_run_history)
  ) {
    readr::read_csv(
      path_model_run_history,
      show_col_types = FALSE
    ) |>
      select_interrupted_model_runs()
  } else {
    tibble::tibble()
  }

git_state <-
  load_git_state(
    repo_path = here::here(),
    verbose = TRUE
  )

git_commit <-
  git_state[["git_commit"]][1]
git_is_dirty <-
  git_state[["git_is_dirty"]][1]


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = path_temporal_models
  )

models_to_run_order <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  ) %>%
  dplyr::arrange(analysis, variable)


#----------------------------------------------------------#
# 2. Run models -----
#----------------------------------------------------------#

purrr::walk(
  .progress = "Fitting general temporal models",
  .x = models_to_run_order[["model_id"]],
  .f = ~ run_configured_temporal_model(
    model_id = .x,
    data_source = data_general_model,
    config_dir = path_temporal_models,
    model_dir = path_model_dir,
    path_history = path_model_run_history,
    data_interrupted_runs = data_interrupted_runs,
    git_commit = git_commit,
    git_is_dirty = git_is_dirty,
    verbose = TRUE
  )
)
