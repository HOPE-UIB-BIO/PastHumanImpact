#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                         Run models
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

n_cores_available <-
  as.numeric(parallelly::availableCores())


#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_to_fit",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

model_config_table <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

#----------------------------------------------------------#
# 1. Run models -----
#----------------------------------------------------------#

models_to_run <-
  model_config_table %>%
  # start with the smallest model first
  dplyr::arrange(n_records)

for (i in seq_len(nrow(models_to_run))) {
  if (
    models_to_run$need_to_run[i] == FALSE
  ) {
    next
  }

  current_env <- environment()

  sel_data_to_fit <-
    data_to_fit %>%
    dplyr::filter(
      region == models_to_run$region[1] &
        climatezone == models_to_run$climatezone[1] &
        variable == models_to_run$variable[1]
    ) %>%
    purrr::chuck("data_to_fit", 1)

  sel_set_total_iter <-
    models_to_run$total_iterations[1]

  sel_set_min_iter_per_chain <-
    models_to_run$min_iterations_per_chain[1]

  if (
    (sel_set_total_iter / n_cores_available) < sel_set_min_iter_per_chain
  ) {
    n_cores_to_use <-
      sel_set_total_iter / sel_set_min_iter_per_chain
  } else {
    n_cores_to_use <-
      n_cores_available
  }

  sel_iter_per_chain <-
    ceiling(sel_set_total_iter / n_cores_to_use)

  # Run model
  sel_mod <-
    fit_brms_hgam(
      y_var = "value",
      data_source = sel_data_to_fit,
      error_family = models_to_run$errro_family[1],
      chains = n_cores_to_use,
      iter = sel_set_min_iter_per_chain,
      prior = models_to_run$prior[1],
    )

  if (
    exists("sel_mod", envir = current_env)
  ) {
    models_to_run$last_run_date[1] <- as.character(Sys.Date())
    models_to_run$last_run_time[1] <- as.character(Sys.time())
    models_to_run$need_to_be_evaluated[1] <- TRUE
    models_to_run$need_to_run[1] <- FALSE

    RUtilpol::save_latest_file(
      object_to_save = models_to_run,
      file_name = "predictor_models_config_table",
      dir = paste0(
        data_storage_path,
        "Data/Predictor_models/"
      ),
      prefered_format = "csv"
    )

    RUtilpol::save_latest_file(
      object_to_save = sel_mod,
      file_name = paste(
        models_to_run$variable[1],
        models_to_run$region[1],
        models_to_run$climatezone[1],
        sep = "__"
      ),
      dir = paste0(
        data_storage_path,
        "Data/Predictor_models/Mods"
      ),
      prefered_format = "qs",
      preset = "archive"
    )
  }

  # clean up
  try(rm(sel_mod))
  try(rm(sel_data_to_fit))
  try(rm(sel_set_total_iter))
  try(rm(sel_set_min_iter_per_chain))
  try(rm(n_cores_to_use))
  try(rm(sel_iter_per_chain))
  try(rm(current_env))
  try(rm(data_to_fit))
}
