#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                      Evaluate models
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

pareto_k_threshold <- 0.7
loo_threshold <- 0.1
rhat_threshold <- 1.1
rhat_threshold_quantile <- 0.9

path_model_run_history <-
  file.path(
    data_storage_path,
    "Temporal_models",
    "general_model_run_history.csv"
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
# 1. Load config table -----
#----------------------------------------------------------#

models_to_evaluate <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  )


#----------------------------------------------------------#
# 2. Evaluate models -----
#----------------------------------------------------------#

purrr::walk(
  .progress = "Evaluating general temporal models",
  .x = models_to_evaluate[["model_id"]],
  .f = ~ evaluate_configured_temporal_model(
    model_id = .x,
    config_dir = path_temporal_models,
    model_dir = path_model_dir,
    path_history = path_model_run_history,
    pareto_k_threshold = pareto_k_threshold,
    loo_threshold = loo_threshold,
    rhat_threshold = rhat_threshold,
    rhat_threshold_quantile = rhat_threshold_quantile,
    git_commit = git_commit,
    git_is_dirty = git_is_dirty,
    verbose = TRUE
  )
)
