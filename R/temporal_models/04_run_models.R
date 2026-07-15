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

make_dir(path_model_dir)

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
      get_interrupted_model_runs()
  } else {
    tibble::tibble()
  }

git_commit <-
  tryCatch(
    system2(
      command = "git",
      args = c("rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )[1],
    error = function(err) NA_character_
  )

git_status <-
  tryCatch(
    system2(
      command = "git",
      args = c("status", "--porcelain"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(err) NA_character_
  )

git_is_dirty <-
  if (
    all(is.na(git_status))
  ) {
    NA
  } else {
    length(git_status) > 0L
  }


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
