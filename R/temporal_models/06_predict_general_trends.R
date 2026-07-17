#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                    Predict trajectories
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

rewrite <- FALSE
max_prediction_draws <- 1000L
prediction_range <- "group_observed"
vec_requested_analyses <-
  commandArgs(trailingOnly = TRUE)

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
path_prediction_dir <-
  file.path(
    path_temporal_models,
    "General_trends"
  )

make_dir(
  path_prediction_dir
)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = path_temporal_models
  )

data_model_config <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  )

vec_available_analyses <-
  data_model_config %>%
  dplyr::filter(is_model_eligible) %>%
  dplyr::distinct(analysis) %>%
  dplyr::pull(analysis)

if (
  length(vec_requested_analyses) == 0L
) {
  vec_requested_analyses <-
    vec_available_analyses
}

assertthat::assert_that(
  all(vec_requested_analyses %in% vec_available_analyses),
  msg = "Requested analyses must exist among eligible temporal models."
)

data_models_pending <-
  data_model_config %>%
  dplyr::filter(
    analysis %in% vec_requested_analyses,
    is_model_eligible,
    need_to_run | need_to_be_evaluated
  )

assertthat::assert_that(
  nrow(data_models_pending) == 0L,
  msg = "All eligible temporal models must pass evaluation before prediction."
)

vec_model_ids_ready <-
  data_model_config %>%
  dplyr::filter(
    analysis %in% vec_requested_analyses,
    is_model_eligible
  ) %>%
  dplyr::pull(model_id)


#----------------------------------------------------------#
# 2. Predict models -----
#----------------------------------------------------------#

list_predictions <-
  purrr::map(
    .progress = "Predicting general temporal models",
    .x = vec_model_ids_ready,
    .f = ~ predict_configured_temporal_model(
      model_id = .x,
      data_source = data_general_model,
      config_dir = path_temporal_models,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      rewrite = rewrite,
      max_prediction_draws = max_prediction_draws,
      prediction_range = prediction_range,
      verbose = TRUE
    )
  )

data_predictions <-
  list_predictions %>%
  purrr::compact() %>%
  dplyr::bind_rows()

if (
  nrow(data_predictions) > 0 &&
    setequal(vec_requested_analyses, vec_available_analyses)
) {
  RUtilpol::save_latest_file(
    object_to_save = data_predictions,
    file_name = "general_temporal_model_predictions",
    dir = path_prediction_dir,
    prefered_format = "csv"
  )

}

data_pap_predictions <-
  data_predictions %>%
  dplyr::filter(analysis == "pap_temporal")

if (
  nrow(data_pap_predictions) > 0
) {
  RUtilpol::save_latest_file(
    object_to_save = data_pap_predictions,
    file_name = "pap_temporal_model_predictions",
    dir = path_prediction_dir,
    prefered_format = "csv"
  )

  readr::write_csv(
    data_pap_predictions,
    here::here(
      "Outputs/Tables/pap_temporal_trends_by_region_climatezone.csv"
    )
  )
}
